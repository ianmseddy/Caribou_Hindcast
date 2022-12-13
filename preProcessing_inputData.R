
#options
reproducible::checkPath("cache", create = TRUE)
options("reproducible.cachePath" = "cache")
setDTthreads(4)
runAnalysis <- FALSE #if TRUE, will remake the GIS layers
focalRadius <- 1000 #the radius to use for focal statistics, in metres
nx = 3 #referring to tile columns
ny = 3 # referring to tile rows
#a silly utility function
getYear <- function(pat, List) { return(List[grep(pat, List)])}

#create some folders for output
checkPath("outputs/raw", create = TRUE)
checkPath("GIS/tiles", create = TRUE)

#alternatively get entire folder at https://drive.google.com/drive/folders/10-abXWuOiXm35Orfko33tXsui2_Jx1Hb?usp=share_link

##
# googledrive::drive_auth("<your email>")
googledrive::drive_auth("ian.eddy@nrcan-rncan.gc.ca")

GISfiles <- list("SCANFI_att_age_S_2020_v0.tif" =
                   "https://drive.google.com/file/d/10dMdxii63X4zufdCOGb6DK18V_qVdeqF/view?usp=share_link",
                 "SCANFI_att_age_S_1985_v0.tif" =
                   "https://drive.google.com/file/d/10Xg7eBb_46nch0NTLfBzJz37l-lBI9Sb/view?usp=share_link",
                 "SCANFI_att_closure_S_2020_v0.tif" =
                   "https://drive.google.com/file/d/10rYVC35xsp-PQ_IsTe_OxLRuBMLdlyKG/view?usp=share_link",
                 "SCANFI_att_closure_S_1985_v0.tif" =
                   "https://drive.google.com/file/d/10eyP-q3Bf0Hq944Y2RjDKqix--L_Cq8O/view?usp=share_link",
                 "SCANFI_sps_prcD_S_2020_v0.tif" =
                   "https://drive.google.com/file/d/10F3ixB5GehFb3Bg_bcmlZw46Ph8JVskp/view?usp=share_link",
                 "SCANFI_sps_prcD_S_1985_v0.tif" =
                   "https://drive.google.com/file/d/10GxCWeplsh7ns2DDihk7VVWDqCuHxKuh/view?usp=share_link",
                 "SCANFI_att_land_pos_S_2020_v0.tif" =
                   "https://drive.google.com/file/d/11L81hDLGbDg1GqyRWqfACDmE6xTFIiZq/view?usp=share_link",
                 #assume land position is constant between 1985-2020,
                 "SCANFI_att_VegTypeClass_S_2020_v0.tif" =
                 "https://drive.google.com/file/d/11evgT3klHt4U6qImkuibxjIj6MFCEdOG/view?usp=share_link",
                 "SCANFI_att_VegTypeClass_S_1985_v0.tif" =
                 "https://drive.google.com/file/d/11SHnzGDoIL1bqHV5YJCna2GrlPBH3CpW/view?usp=share_link",
                 "CanLaD_Latest_1985_2020_YRT2.tif" =
                   "https://drive.google.com/file/d/19tSq6xsHks-5xMrDGZcAqFUJziESUw8f/view?usp=sharing",
                 "CanLaD_Latest_1985_2020_TYPE.tif" =
                   "https://drive.google.com/file/d/19gEzpGFNbHbL8agLCfLFMBOJYaoYZQns/view?usp=sharing")


CanLaD_and_SCaNFI <- as.list(file.path("GIS", names(GISfiles)))

downloadedFiles <- lapply(CanLaD_and_SCaNFI, file.exists)
if (!all(unlist(downloadedFiles))) {
  missingFiles <- GISfiles[!unlist(downloadedFiles)]
  #download fails in a vectorized function
  for (i in names(missingFiles)) {
    googledrive::drive_download(file = as_id(missingFiles[[i]]),
                                path = file.path("GIS", i))
  }
  rm(missingFiles, downloadedFiles)
}
#study area is the intersection of Quebec/Ontario and ecozones
Canada <- prepInputs(url = paste0("https://www12.statcan.gc.ca/census-recensement/2011/",
                                  "geo/bound-limit/files-fichiers/2016/lpr_000b16a_e.zip"),
                     destinationPath = "GIS",
                     fun = "terra::vect")
QuebecOntario <- Canada[Canada$PRUID %in% c("24", "35"),]

Ecozones <- prepInputs(url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
                       destinationPath = "GIS",
                       fun = "terra::vect",
                       studyArea = QuebecOntario)
Ecozones <- Ecozones[Ecozones$ZONE_NAME %in% c("Taiga Shield", "Boreal Shield", "MixedWood Plain", "Hudson Plains")]
SA <- terra::aggregate(Ecozones)


# SA <- terra::project(SA, "EPSG:9001")

processed <- paste0("GIS/Eastern_",basename( unlist(CanLaD_and_SCaNFI)))
missing <- processed[!file.exists(processed)]
#postProcessTerra is leaving some files in tempdrive - FYI
if (length(missing) > 0) {
  inFiles <- CanLaD_and_SCaNFI[processed %in% missing]
  bulkPostProcess <- function(infile, SA){
    dType <- "INT2U" #age and disturbance year were signed, only for the NA?
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
#in hindsight, more tiles would be better... could parallelize
filenamesNoExt <- basename(processed) %>%
  stringr::str_remove(., pattern = ".tif")
notTiled <- lapply(filenamesNoExt, list.files, path = "GIS/tiles") %>%
  lapply(., length) %>%
  unlist(.)


if (any(notTiled < 1)) {
  missing <- filenamesNoExt[notTiled < 1]
  lapply(missing, FUN = function(toTile){
    asRaster <- raster(file.path("GIS", paste0(toTile, ".tif"))) #must be raster
    Type <- ifelse(length(grep("YR|age", toTile)) == 1, "INT2U", "INT1U") #technically 240 is max age?
    #cc, percD, landcover, pos, all under 255
    SpaDES.tools::splitRaster(asRaster, nx = nx, ny = ny, buffer = c(35, 35),
                              rType = Type, path = "GIS/tiles", fExt = ".tif")
  })
}

rm(CanLaD_and_SCaNFI, missing)

####sanity check####
files <- list.files("GIS", pattern = "tif", full.names = TRUE) %>%
  unlist(.) %>%
  grep(., pattern = "Eastern", value = TRUE)

cellNum <- lapply(files, rast) %>%
  lapply(., ncell)
if (length(unlist(unique(cellNum))) > 1) {
  stop("GIS error - unequal file size")
}
rm(files, cellNum)


##### 1985 disturbance mapping ####

#as we need a template raster, this section is processsed after the CanLAD data
#template raster will be the processed age data.
ageRTM <- raster("GIS/Eastern_SCANFI_att_age_S_2020_v0.tif")
#get map of forest management (2020) hopefully this isn't outdated... make sure to check
#values are 11 - long-term tenure, 12 - short-term tenure, 13 other, 20 Protected aras,
#31 Federal reserve, 32 Indian Reserve, 33 Restricted, 40 Treaty and Settlement, 50 Private forests

#TODO: this is stored on my (personal) googledrive - change file settings
ManagedForest <- prepInputs(url = paste0("https://drive.google.com/file/d",
                                         "/1W2EiRtHj_81ZyKk5opqMkRqCA1tRMMvB/view?usp=share_link"),
                            fun = "terra::rast",
                            destinationPath = "GIS",
                            targetFile = "Canada_MFv2017.tif")
ManagedForest <- postProcessTerra(from = ManagedForest, to = ageRTM,
                                  writeTo = "GIS/Eastern_ManagedForest.tif",
                                  method = "near", datatype = "INT1U")
gc()
#read it in again as a raster - required for SpaDES.tools::splitRaster
ManagedForest <- raster("GIS/Eastern_ManagedForest.tif")

#this buffer refers to pixels - 35 will be greater than 1 km, ensuring no edge effect when we merge back.
#(there is still an edge effect on the provincial land borders, ie Ontario/Manitoba)
splitRaster(ManagedForest, nx = nx, ny = ny, buffer = c(35, 35),
            rType = "INT1U", path = "GIS/tiles", fExt = ".tif")

rm(ManagedForest)

NFDB <- Cache(prepInputs,
              url = "https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip",
              destinationPath = "GIS", fun = "st_read",
              userTags = c("NFDB"))
NFDB <- NFDB[NFDB$YEAR > 1964 & NFDB$YEAR < 1986,]
NFDB <- sf::st_transform(NFDB, crs = st_crs(ageRTM))

#fasterize does not have a filename argument, so this raster will temporarily exist in RAM.
NFDBras <- fasterize(NFDB, raster = ageRTM, field = "YEAR")

writeRaster(NFDBras, "GIS/NFDB_raster_1965_1985.tif", overwrite = TRUE)
rm(NFDBras)
gc()
NFDBras <- raster("GIS/NFDB_raster_1965_1985.tif")
#tile the NFDB
fireTiles <- list.files("GIS/tiles", pattern = "NFDB", full.names = TRUE)
if (length(fireTiles) < 1) {
  names(NFDBras) <- "Eastern_NFDB"
  SpaDES.tools::splitRaster(NFDBras, nx = nx, ny = ny, buffer = c(35, 35),
                            rType = "INT2U", path = "GIS/tiles", fExt = ".tif")

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


