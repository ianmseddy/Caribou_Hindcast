#identying disturbance classes pre-1985
#in order to distinguish stands originating from fire or harvest in 1985, age 20 or less
#we will use the NFDB and the forest management boundary, in addition to the 1985 age layer
#Pixels that have age < 20 y.o in 1985 that fall outside the forest management boundary will be assumed to have burned.
#Pixels that are inside the boundary will be checked first if they are in the rasterized NFDB between 1965-1985, if so
#assume fire, else harvest. This will likely overestimate harvest and underestimate natural disturbance,
#but I am not sure if a better solution.

#in hindsight, should have buffered the NFDB fire, as we might end up with "salt and pepper" harvest/fire.

#in hindsight, didn't need to restrict fire year to 65-85 in the rasterized version

InferDisturbances <- function(NFDB, MngFor, age, lcc, dBaseYear = 1985, outDir) {
  tileNum <- stringr::str_extract(age, pattern = "tile[0-9]+")
  #Non-forest pixels are 0 age, therefore I need Landcover 1985
  age <- rast(age)
  #sanity check
  NFDB1 <- rast(NFDB)
  MngFor1 <- rast(MngFor)
  lcc1 <- rast(lcc)
  compareGeom(NFDB1, age, lcc1)
  compareGeom(lcc1, MngFor1)
  rm(NFDB1, MngFor1, lcc1)

  # focalMatrix <- terra::focalMat(x = age, d = focalWindow, type = "circle")

  ageDT <- data.table(age = values(age), pixelID = 1:ncell(age))
  rm(age)
  setnames(ageDT, c("age", "pixelID"))
  ageDT <- ageDT[age < 21]

  #drop non-forest as these are age zero
  lcc <- rast(lcc)
  ageDT[, lcc := lcc[][ageDT$pixelID]] #get landcover
  ageDT <- ageDT[lcc < 8 & lcc > 4,] #forest
  ageDT[, lcc := NULL]
  rm(lcc)
  gc()

  #get managed forest value and fire
  MngFor <- rast(MngFor)
  ageDT[, MngFor := MngFor[][ageDT$pixelID]]
  rm(MngFor)

  NFDB <- rast(NFDB)
  ageDT[, NFDB := NFDB[][ageDT$pixelID]]
  #sort into three classes: young harvest, old harvest, fire
  #anything that isn't in 50, 11, or 12 (managed and private forest) is burned
  #anything with no record of fire and inside these classes is harvested
  youngHarvest <- ageDT[MngFor %in% c(50, 11, 12) & is.na(NFDB) & age < 6]$pixelID
  oldHarvest <- ageDT[MngFor %in% c(50, 11, 12) & is.na(NFDB) & age > 5]$pixelID
  fire <- ageDT[!c(pixelID %in% oldHarvest| pixelID %in% youngHarvest)]$pixelID
  rm(ageDT)
  gc()

  #write the natural disturbance raster
  fireRepVals <- rep(NA, times = ncell(NFDB))
  fireRepVals[fire] <- 1
  fire <- setValues(NFDB, fireRepVals)
  gc()# for data.table overwrite
  outFile <- file.path(outDir, paste0("naturalDisturbance", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(fire, filename = outFile, datatype = "INT1U", overwrite = TRUE)
  rm(fire, fireRepVals)
  gc()

  #write the young harvest
  harvestRepVals <-rep(NA, times = ncell(NFDB))
  harvestRepVals[youngHarvest] <- 1
  youngHarvest <- setValues(NFDB, harvestRepVals)
  outFile <- file.path(outDir, paste0("harvest_0to5_", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(youngHarvest, filename = outFile, datatype = "INT1U", overwrite = TRUE)
  rm(youngHarvest, harvestRepVals)
  gc()

  #write the older harvest
  harvestRepVals <-rep(NA, times = ncell(NFDB))
  harvestRepVals[oldHarvest] <- 1
  oldHarvest <- setValues(NFDB, harvestRepVals)
  outFile <- file.path(outDir, paste0("harvest_6to20_", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(oldHarvest, filename = outFile, datatype = "INT1U", overwrite = TRUE)
  # outFile <- file.path("outputs", paste0("harvest_6to20_", dBaseYear,  "_focal", focalWindow, "_", tileNum, ".tif"))
  # oldFocal <- terra::focal(oldHarvest, w = focalMatrix, fun = sum, na.rm = TRUE, expand = FALSE,
  #                            filename = outFile, overwrite = TRUE)
  rm(oldHarvest, harvestRepVals, NFDB)
  gc()

}

if (runAnalysis) {

  #TODO: decide how to organize these (on Windows, better to write to SSD)
  outputDir = "outputs/raw"

  NFDBlist <- list.files(path = "GIS/tiles", pattern = "NFDB", full.names = TRUE)
  MngForList <- list.files(path = "GIS/tiles", pattern = "MFv", full.names = TRUE)
  ageList <- list.files(path = "GIS/tiles", pattern = "age_S_1985", full.names = TRUE)
  lccList <- list.files(path = "GIS/tiles", pattern = "VegTypeClass_S_1985", full.names = TRUE)

  Map(InferDisturbances, NFDB = NFDBlist, MngFor = MngForList,
      age = ageList, lcc = lccList,
      MoreArgs = list(dBaseYear = 1985,
                      outDir = outputDir))
}

