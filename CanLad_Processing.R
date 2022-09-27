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

GISfiles <- list("CaNFIR_att_age_S_2020_v0.tif" =
                   "https://drive.google.com/file/d/18BqqOiwpuY0bZbM1DMmYi5sDnfTb0JIH/view?usp=sharing",
                 "CaNFIR_att_age_S_1985_v0.tif" =
                   "https://drive.google.com/file/d/18C_riLq0l7xG5Dd5UeYa0Gu3zbUtOUuo/view?usp=sharing",
                 "CaNFIR_att_closure_S_2020_v0.tif" =
                   "https://drive.google.com/file/d/17e_qPKROfWsB7Wa2ObMIvtwRgk1GBsM_/view?usp=sharing",
                 "CaNFIR_att_closure_S_1985_v0.tif" =
                   "https://drive.google.com/file/d/17FIcD9_GYOfOB2mMXtSQHoyJryl4L9vu/view?usp=sharing",
                 "CaNFIR_sps_prcD_S_2020_v0.tif" =
                   "https://drive.google.com/file/d/17PRFhPw3e26sJonTxnbU12Lw7Cmtmp3s/view?usp=sharing",
                 "CaNFIR_sps_prcD_S_1985_v0.tif" =
                   "https://drive.google.com/file/d/16yhcNWll7RKd1-t7R-cHcPII-4Wua9Zp/view?usp=sharing",
                 "CaNFIR_att_land_pos_S_2020_v0.tif" =
                   "https://drive.google.com/file/d/17aACgxp6an0WSzBNn0_5gsaza_aPTVmH/view?usp=sharing",
                 #assume land position is constant between 1985-2020,
                 "CaNFIR_att_VegTypeClass_S_2020_v0.tif" =
                 "https://drive.google.com/file/d/1-SO9bXl0trI48IDKvshgDR4qLVPM5X3n/view?usp=sharing",
                 "CanLaD_Latest_1985_2020_YRT2.tif" =
                   "https://drive.google.com/file/d/19tSq6xsHks-5xMrDGZcAqFUJziESUw8f/view?usp=sharing",
                 "CanLaD_Latest_1985_2020_TYPE.tif" =
                   "https://drive.google.com/file/d/19gEzpGFNbHbL8agLCfLFMBOJYaoYZQns/view?usp=sharing")

#normally I would use reproducible::prepInputs but I don't want to CHECKSUMS nor load these
CanLadData <- as.list(file.path("GIS", names(GISfiles)))

downloadedFiles <- lapply(CanLadData, file.exists)
if (!all(unlist(downloadedFiles))) {
  missingFiles <- GISfiles[!unlist(downloadedFiles)]
  #download fails in a vectorized function
  for (i in names(missingFiles)) {
    googledrive::drive_download(file = as_id(missingFiles[[i]]),
                                path = file.path("GIS", i))
  }
  rm(missingFiles, downloadedFiles)
}



Canada <- prepInputs(url = paste0("https://www12.statcan.gc.ca/census-recensement/2011/",
                                  "geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip"),
                     destinationPath = "GIS",
                     fun = "terra::vect")
#East of Manitoba, ignoring Nunavut
SA <- Canada[Canada$PRUID %in% c("10", "11", "12", "13", "24", "35"),]

# SA <- terra::project(SA, "EPSG:9001")

processed <- paste0("GIS/Eastern_",basename( unlist(CanLadData)))
missing <- processed[!file.exists(processed)]
if (length(missing) > 0) {
  inFiles <- CanLadData[processed %in% missing]
  bulkPostProcess <- function(infile, SA){
    dType <- ifelse(basename(infile) == "CanLaD_Latest_1985_2020_YRt2.tif", "INT2U", "INT1U")
    outName <- file.path("GIS", paste0("Eastern_", basename(infile)))
    infile <- rast(infile)
    SA <- project(SA, crs(infile))
    post <- postProcessTerra(from = infile, cropTo = SA, maskTo = SA, useSAcrs = FALSE,
                             writeTo = outName, datatype = dType)
  }

  lapply(inFiles, bulkPostProcess, SA = SA)
  rm(bulkPostProcess, inFiles, missing)
}

#even at ~1/3 the original size, 8 billion pixels is too many for logical queries so I will split these into tiles
#splitRaster is great but isn't configured for terra yet :(
CanLadSA <- lapply(paste0("GIS/Eastern_", basename(CanLadData)), raster)
names(CanLadSA) <- stringr::str_remove(basename(CanLadData), pattern = ".tif")

if (FALSE) {
 #don't do this unless necessary
lapply(CanLadSA, SpaDES.tools::splitRaster, nx = 3, ny = 2, buffer = c(35, 35), rType = "INT2U", path = "GIS/tiles")
}

#The CaNFIR and CanLAD data
# [1] "CaNFIR_att_age_S_2020_v0-001.tif" stand age
# [2] "CaNFIR_att_closure_S_1985_v0-006.tif" canopy closure 1985
# [3] "CaNFIR_att_closure_S_2020_v0-007.tif" canopy closure 2020
# [4] "CaNFIR_att_height_S_1985_v0-003.tif"   height 1985
# [5] "CaNFIR_att_height_S_2020_v0-008.tif"  height 2020
# [6] "CaNFIR_sps_prcD_S_2020_v0-004.tif"   percent deciduous
# [7] "CanLaD_Latest_1985_2020_TYPE.tif" disturbance type - 1 = fire, 2 = harvest, 255 = non-disturbed
# [8] "CanLaD_Latest_1985_2020_YRt2.tif" year of disturbance
# [9] "CaNFIR_att_land_pos_S_2020_v0.tif"  land position - missing legend but 5 = wetland
# [10] "CaNFIR_sps_prcD_S_1985_v0.tif"   percent deciduous
# [11] "CaNFIR_att_VegTypeClass_S_2020_v0.tif" Vegetation type: bryoid (1), herbs(2) rock(3) shrub (4),
# Treed Broadleaf (5) Treed Conifer (6) Treed Mixed (7) Water (8)


#Leblond formula
# section 4.1 of Leblond et al 2014
# 0.25 * mature conifer  > 70 yo  --- uses CaNFIR_att_age,
# 0.19 * young mature conifer forests 50-70 years old
# 0.14 * wetlands
# 0.22 * open lichen woodlands (age 50+ conifers with < 30% cover)
# 0.06 * natural disturbances < 20
# 0.04 * cutblocks 5 yo or less
# 0.04 * cutblocks 6-20 - same weight as 5yo but s.d. is different
# 0.06 * regenerating stands > 20 years post disturbance (but that aren't mature conifers or wetlands)

#calculate the binaries, then focal statistics, then multiply the resulting fraction by its weight.
#so an area of 1 km that is entirely natural disturbance < 20 becomes 0.06. Then we sum the weights?
#since these are mutually exclusive, the theoretical maximum will be 0.25 (a square km of mature conifer > 70 y.o)

# In order
#1)	Identify the two mature conifer classes, age 50-70 and 70+, with canopy closure > 30%
source("tiles_MatureConifers.R")
#2)	Identify the natural disturbances < 20 y.o
source("tiles_NaturalDisturbanceStands.R")
#3)	Identify the two age classes of regenerating cutblocks (0-5 and 6-20)
source("tiles_HarvestedStands.R")

#4)	Identify the Open Lichen Woodlands: conifers age 50+ with canopy closure < 30% that aren't wetlands
#this assumse that ages and disturbances are consistent
source("tiles_OpenWoodlands.R")

#5)	Identify Regenerating Stands: any stands > 20 years that aren’t conifer 50+ (ie none of the above classes)
#this class includes majority-conifer pixels that are 21-49 years of age, and deciduous stands age 20+ that aren't wetland.
#some of these deciduous 20+ may be wetland - these will be classified as wetland instead
#since disturbance supersedes all - must check that the 20-50 are also undisturbed..
source("tiles_regeneratingStands.R")

#6)	Identify wetlands
#per Mathieu, wetland was Alnus spp, open (ie non forest) or flooded. Alnus is an 'unproductive' land cover class
#therefore, 'mature conifer' that falls under wetland would still be mature conifer.
#Our wetland will be non-forest wetland, deciduous 20+, and age 50+ conifer with cover <30%
#


