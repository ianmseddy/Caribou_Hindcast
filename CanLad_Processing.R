library(terra)
library(sf)
library(data.table)
library(reproducible)
library(stringr)
library(raster)
library(googledrive)
#reproducible::checkPath("cache", create = TRUE)
options("reproducible.cachePath" = "cache")
setDTthreads(4)
runAnalysis <- FALSE #if TRUE, will remake the GIS layers
focalRadius <- 1000 #the radius to use for focal statistics, in metres


#cropping takes 15 minutes a layer - so write some intermediate files - reducing size by 67% (alternatively cache)
#this should be a prepinputs

GISfiles <- list("CaNFIR_att_age_S_2020_v0.tif" =
                   "https://drive.google.com/file/d/18BqqOiwpuY0bZbM1DMmYi5sDnfTb0JIH/view?usp=sharing",
                 "CaNFIR_att_age_S_1985_v0.tif" =
                   "https://drive.google.com/file/d/18C_riLq0l7xG5Dd5UeYa0Gu3zbUtOUuo/view?usp=sharing",
                 "CaNFIR_att_closure_S_2020_v0.tif" =
                   "https://drive.google.com/file/d/17e_qPKROfWsB7Wa2ObMIvtwRgk1GBsM_/view?usp=sharing",
                 "CaNFIR_att_closure_S_1985_v0.tif" =
                   "https://drive.google.com/file/d/17FIcD9_GYOfOB2mMXtSQHoyJryl4L9vu/view?usp=sharing",
                 "CanLaD_Latest_1985_2020_YRT2.tif" =
                   "https://drive.google.com/file/d/19tSq6xsHks-5xMrDGZcAqFUJziESUw8f/view?usp=sharing",
                 "CanLaD_Latest_1985_2020_TYPE.tif" =
                   "https://drive.google.com/file/d/19gEzpGFNbHbL8agLCfLFMBOJYaoYZQns/view?usp=sharing")

downloadedFiles <- lapply(file.path("GIS", names(GISfiles)), file.exists)
if (!all(unlist(downloadedFiles))) {
  missingFiles <- GISfiles[!unlist(downloadedFiles)]
  #download fails in a vectorized function
  for (i in names(missingFiles)) {
    googledrive::drive_download(file = as_id(missingFiles[[i]]),
                                path = file.path("GIS", i))
  }
}

#normally I would use reproducible::prepInputs but I don't want to CHECKSUMS nor load these
CanLadData <- as.list(file.path("GIS", names(GISfiles)))

template <- rast(CanLadData[[1]])

Canada <- prepInputs(url = "https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip",
                     destinationPath = "GIS",
                     fun = "terra::vect")
SA <- Canada[Canada$PRUID %in% c("10", "11", "12", "13", "24", "35"),]

#East of Manitoba, ignoring Nunavut
SA <- terra::project(SA, template)

if (!all(file.exists(paste0("GIS/Eastern_", basename(CanLadData))))) {
  bulkPostProcess <- function(filePath, SA){
    dType <- ifelse(basename(filePath) == "CanLaD_Latest_1985_2020_YRt2.tif", "INT2U", "INT1U")
    pre <- rast(file.path("C:/Ian/Data/CanLad", filePath))
    newName <- file.path("GIS", paste0("Eastern_", basename(filePath)))

    post <- postProcessTerra(from = pre, cropTo = SA, maskTo = SA,
                             writeTo = newName, datatype = dType)
  }

  lapply(CanLadData, bulkPostProcess, SA = SA)
}
#even at ~1/3 the original size, 8 billion pixels is too many for logical queries so I will split these into tiles
#splitRaster is great but isn't configured for terra yet :(
CanLadSA <- lapply(paste0("GIS/Eastern_", basename(CanLadData)), raster)
names(CanLadSA) <- stringr::str_remove(basename(CanLadData), pattern = ".tif")

if (runAnalysis) {
 #don't do this unless necessary
lapply(CanLadSA, SpaDES.tools::splitRaster, nx = 3, ny = 2, buffer = c(35, 35), rType = "INT2U", path = "GIS/tiles")
}

#this produced potentially 1.5 TB of temp files - must be careful..


# [1] "CaNFIR_att_age_S_2020_v0-001.tif" stand age
# [2] "CaNFIR_att_closure_S_1985_v0-006.tif" canopy closure 1985
# [3] "CaNFIR_att_closure_S_2020_v0-007.tif" canopy closure 2020
# [4] "CaNFIR_att_height_S_1985_v0-003.tif"   height 1985
# [5] "CaNFIR_att_height_S_2020_v0-008.tif"  height 2020
# [6] "CaNFIR_sps_prcD_S_2020_v0-004.tif"   Not sure - species?
# [7] "CanLaD_Latest_1985_2020_TYPE.tif" disturbance type - 1 = fire, 2 = harvest, 255 = non-disturbed
# [8] "CanLaD_Latest_1985_2020_YRt2.tif" year of disturbance
# [9] "CaNFIR_att_land_pos_S_2020_v0.tif"  Not sure
# [10] "CaNFIR_sps_prcD_S_1985_v0.tif"   Not sure, but 1985


#also need disturbance year, and

#Leblond formula
# section 4.1 of Leblond et al 2014
# 0.25 * mature conifer  > 70 yo  --- uses CaNFIR_att_age,
# 0.14 * wetlands
# 0.22 * open lichen woodlands (forest with < 30% cover)
# 0.06 * natural disturbances < 20
# 0.04 * cutblocks 5 yo or less
# 0.04 * cutblocks 6-20 - same weight as 5yo but s.d. is different
# 0.06 * regenerating stands > 20 years post disturbance (but that aren't mature conifers)

#calculate the binaries, then focal statistics, then multiply the resulting fraction by it's weight.
#so an area of 1 km that is entirely natural disturbance < 20 becomes 0.06. Then we sum the weights?
#since these are mutually exclusive, the theoretical maximum will be 0.25 (a square km of mature conifer > 70 y.o)


#####mature conifer#####
ageList <- list.files(path = "GIS/tiles", pattern = "age_S", recursive = TRUE, full.names = TRUE)
coniferList <- list.files(path = "GIS/tiles", pattern = "?")


#after, these need to be divided by 9 and multiplied by 0.06, but wait until we know the focal statistic to use



