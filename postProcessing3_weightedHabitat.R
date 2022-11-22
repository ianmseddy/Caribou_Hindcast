#purpose is two-fold:
# convert all focal habitat rasters into single weighted habitat layer for each year
# calculate the difference
# generate a habitat class layer by assigning 1:8 to binary habitats
# thsi cannot be run in parallel (well without a better machine - RAM use hits 60 GB one tile)


if (Sys.info()["sysname"] == "Linux") {
  inputDir1 <- "outputs/focalHabitat"
  outputDir1 <- "outputs/weightedHabitat"
  inputDir2 <- "outputs/maskedHabitat"
  outputDir2 <- "outputs/compositeHabitat"
} else {
  inputDir1 <- "D:/Ian/YanBoulanger/maskedHabitat"
  outputDir1 <- "D:/Ian/YanBoulanger/focalHabitat"
  inputDir2 <- "D:/Ian/YanBoulanger/maskedHabitat"
  outputDir2 <- "D:/Ian/YanBoulanger/compositeHabitat"
}

checkPath(outputDir1, create = TRUE)
checkPath(outputDir2, create = TRUE)
tiles <- paste0("tile", 1:6)

habitatRasters <- lapply(tiles, list.files, path = inputDir1, full.names = TRUE)

makeWeightedHabitat <- function(tileList, year, outputPath) {


  tileList <- tileList[grep(tileList, pattern = year)] #1985 or 2020
  if (length(tileList) == 0) {stop("incorrect year")}
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
  wetlands <- rast(tileList[grep("wetland", tileList)]) * 0.14
  weightedHabitat <- sum(weightedHabitat, woodlands, wetlands, na.rm = TRUE)
  rm(woodlands, wetlands)
  gc()

  regen <- rast(tileList[grep("regenerating", tileList)]) * 0.06
  weightedHabitat <- sum(weightedHabitat, regen, na.rm = TRUE)
  rm(regen)
  gc()


  tileNum <- stringr::str_extract(tileList[1], pattern = "tile[0-9]+")
  outFile <- file.path(outputPath, paste0("weightedHabitat_", year, "_", tileNum, ".tif"))
  writeRaster(weightedHabitat, filename = outFile, overwrite = TRUE)
  message(tileNum, " complete")
}

lapply(habitatRasters, FUN = makeWeightedHabitat, year = 2020, outputPath = outputDir1)
lapply(habitatRasters, FUN = makeWeightedHabitat, year = 1985, outputPath = outputDir1)



#make a map with
#fire = 1, young/old harvest = 2 and 3, young/mature conifer = 4 and 5,
#open woodland 6, wetland 7, regenerating forest = 8
makeCompositeHabitat <- function(tileList, year, outputPath) {
  checkPath(outputPath, create = TRUE)
  tileList <- tileList[grep(tileList, pattern = year)] #1985 or 2020
  if (length(tileList) == 0) {stop("incorrect year")}
  #as a float, each individual habitat tile is 20 GB

  fire <- rast(tileList[grep("naturalDisturbance", tileList)]) * 1
  harvestY <- rast(tileList[grep("harvest_0to5", tileList)]) * 2
  harvestO <- rast(tileList[grep("harvest_6to20", tileList)]) * 3
  compositeHabitat <- sum(harvestY, harvestO, fire, na.rm = TRUE)
  rm(harvestY, harvestO, fire)
  gc()

  coniferY <- rast(tileList[grep("youngConifer", tileList)]) * 4
  coniferO <- rast(tileList[grep("matureConifer", tileList)]) * 5
  compositeHabitat <- sum(coniferY, coniferO, compositeHabitat, na.rm = TRUE)
  rm(coniferY, coniferO)
  gc()

  woodlands <- rast(tileList[grep("openWoodland", tileList)]) * 6
  wetlands <- rast(tileList[grep("wetland", tileList)]) * 7
  compositeHabitat <- sum(compositeHabitat, woodlands, wetlands, na.rm = TRUE)
  rm(woodlands, wetlands)
  gc()

  regen <- rast(tileList[grep("regenerating", tileList)]) * 8
  compositeHabitat <- sum(compositeHabitat, regen, na.rm = TRUE)
  rm(regen)
  gc()


  tileNum <- stringr::str_extract(tileList[1], pattern = "tile[0-9]+")
  outFile <- file.path(outputPath, paste0("compositeHabitat_", year, "_", tileNum, ".tif"))
  writeRaster(compositeHabitat, filename = outFile, overwrite = TRUE)
  message(tileNum, " complete")

}



habitatClasses <- lapply(tiles, list.files, path = inputDir2, full.names = TRUE)
#some memory leakage happens with this function. Not sure why. Don't recommend running all at once
lapply(habitatClasses, makeCompositeHabitat, year = 2020, outputPath = outputDir2)
lapply(habitatClasses[1:3], makeCompositeHabitat, year = 1985, outputPath = outputDir2)
gc()
lapply(habitatClasses[4:6], makeCompositeHabitat, year = 1985, outputPath = outputDir2)
gc()
