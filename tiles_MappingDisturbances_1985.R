#identying disturbance classes pre-1985
#in order to distinguish stands originating from fire or harvest in 1985, age 20 or less
#we will use the NFDB and the forest management boundary, in addition to the 1985 age layer
#Pixels that have age < 20 y.o in 1985 that fall outside the forest management boundary will be assumed to have burned.
#Pixels that are inside the boundary will be checked first if they are in the rasterized NFDB between 1965-1985, if so
#assume fire, else harvest. This will likely overestimate harvest and underestimate natural disturbance,
#but I am not sure if a better solution.

#March 2023: a considerable amount of landcover classified as shrub/grassland in 1985 regenerates to forest.
#therefore, in 1985, this should be classified as natural disturbance < 20 y.o., rather than omitting it entirely,
#as it is clearly a forest successional stage. However some of these areas are classified as Open Woodland.
#there is no  floor on the canopy cover requirement, and their age is 50+, with majority conifer.
#

InferDisturbances <- function(NFDB, MngFor, age, lcc85, lcc2020, wetland, dBaseYear = 1985, outDir) {
  tileNum <- stringr::str_extract(age, pattern = "tile[0-9]+")
  #Non-forest pixels are 0 age, therefore I need Landcover 1985
  age <- rast(age)
  NFDB <- rast(NFDB)
  MngFor <- rast(MngFor)
  wetland <- rast(wetland)
  lcc85 <- rast(lcc85)
  lcc2020 <- rast(lcc2020)
  compareGeom(NFDB, age, lcc85)
  compareGeom(lcc2020, MngFor, wetland)

  ageDT <- data.table(lcc85 = values(lcc85, mat = FALSE),
                      lcc20 = values(lcc2020, mat = FALSE),
                      pixelID = 1:ncell(lcc2020))
  setnames(ageDT, c("lcc85", "lcc2020", "pixelID"))
  ageDT <- ageDT[lcc85 %in% c(5:7) | c(lcc85 %in% c(2, 4) & lcc2020 %in% c(5:7))]
  #include forest or pixels that are vegetated that become forest
  ageDT[, lcc2020 := NULL]
  gc()

  ageDT[, age := values(age, mat = FALSE)[pixelID]]
  ageDT <- ageDT[age < 21 | c(lcc85 %in% c(2,4) & c(age < 21 | is.na(age)))]
  #the age < 21 prevents any open woodland and regenerating forest being double-counted as natural disturbance,
  #as both classes may have non-forest values
  ageDT[, lcc85 := NULL]
  gc()

  #drop pixels on wetland as they cannot be disturbed
  ageDT[, wetland := values(wetland, data.frame = FALSE)[ageDT$pixelID]]
  ageDT <- ageDT[is.na(wetland)] #wetland is either 1 or NA
  ageDT[, wetland := NULL]
  gc()

  #get managed forest value and fire
  ageDT[, MngFor := MngFor[][ageDT$pixelID]]
  rm(MngFor)

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
  rm(oldHarvest, harvestRepVals, NFDB)
  gc()

}

if (runAnalysis) {

  #TODO: decide how to organize these (on Windows, better to write to SSD)
  outputDir = "outputs/raw"

  NFDBlist <- list.files(path = "GIS/tiles", pattern = "NFDB", full.names = TRUE)
  MngForList <- list.files(path = "GIS/tiles", pattern = "ManagedForest", full.names = TRUE)
  ageList <-getAtt("age", 1985)
  lccList85 <- getAtt("VegTypeClass", 1985)
  wetlandList <- getAtt("wetland", 1985, "outputs/raw")
  lccList2020 <- getAtt("VegTypeClass", 2020)

  Map(InferDisturbances, NFDB = NFDBlist, MngFor = MngForList, age = ageList,
      lcc85 = lccList85, lcc2020 = lccList2020, wetland = wetlandList,
      MoreArgs = list(dBaseYear = 1985, outDir = outputDir))
}

rm(NFDBlist, MngForList, ageList, lccList85, lccList2020, wetlandList)
