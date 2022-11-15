#purpose is two-fold:
# convert all focal habitat rasters into single weighted habitat layer for each year
# calculate the difference
# generate a habitat class layer by assigning 1:8 to binary habitats
outputDir <- "D:/Ian/YanBoulanger/focalHabitat"
tiles <- paste0("tile", 1:6)

habitatRasters <- lapply(tiles, list.files, path = outputDir, full.names = TRUE)

makeWeightedHabitat <- function(tileList, year, outputPath) {


  tileList <- tileList[grep(tileList, pattern = year)] #1985 or 2020
  #as a float, each individual habitat tile is 20 GB

  fire <- rast(tileList[grep("naturalDisturbance", tileList)]) * 0.06
  harvestY <- rast(tileList[grep("harvest_0to5", tileList)]) * 0.04
  harvestO <- rast(tileList[grep("harvest_6to20", tileList)]) * 0.04
  weightedHabitat <- sum(harvestY, harvestO, fire, na.rm = TRUE)
  rm(harvestY, harvestO, fire)
  gc()

  coniferY <- rast(tileList[grep("youngConifer", tileList)]) * 0.19
  coniferO <- rast(tileList[grep("matureConifer", tileList)]) * 0.25
  weightedHabitat <- sum(coniferY, coniferO, weightedHabitat, na.rm = TRUE)
  rm(coniferY, coniferO)
  gc()

  woodlands <- rast(tileList[grep("openWoodland", tileList)]) * 0.22
  wetlands <- rast(tileList[grep("wetland", tileList)]) * 0.06
  weightedHabitat <- sum(weightedHabitat, woodlands, wetlands, na.rm = TRUE)
  rm(woodlands, wetlands)
  gc()


  regen <- rast(tileList[grep("regenerating", tileList)]) * 0.06
  weightedHabitat <- sum(weightedHabitat, regen, na.rm = TRUE)
  rm(regen)
  gc()


  tileNum <- stringr::str_extract(tileList[1], pattern = "tile[0-9]+")
  outFile <- file.path(outputPath, paste0("weightedHabitat_", tileNum, ".tif"))
  writeRaster(weightedHabitat, filename = outFile)
  message(tileNum, " complete")
}

lapply(habitatRasters, FUN = makeWeightedHabitat, year = 2020, outputPath = outputDir)
