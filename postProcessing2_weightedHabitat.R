#purpose is two-fold:
# convert all focal habitat rasters into single weighted habitat layer for each year
# calculate the difference
# generate a habitat class layer by assigning 1:8 to binary habitats
if (Sys.info()["sysname"] == "Linux") {
  focalHabitatDir <- "outputs/focalHabitat"
  focalHabitatDir1000 <- "outputs/focalHabitat1000" #this should have been done in focal - if I ever rerun...
  weightedDir <- "outputs/weightedHabitat"
  compositeDir <- "outputs/compositeHabitat"

} else {
  focalHabitatDir <- "D:/Ian/YanBoulanger/maskedHabitat"
  focalHabitatDir1000 <- "outputs/focalHabitat1000" #this should have been done in focal - if I ever rerun...
  weightedDir <- "D:/Ian/YanBoulanger/focalHabitat"
  compositeDir <- "D:/Ian/YanBoulanger/compositeHabitat"
}

checkPath(weightedDir, create = TRUE)
tiles <- paste0("tile", 1:8)


checkPath("outputs/focalHabitat1000", create = TRUE)

habitatRasters <- lapply(tiles, list.files, path = focalHabitatDir, full.names = TRUE)
multi <- function(X, by){X * by}
#multiply focal files by 1000 - this should have been done earlier...

temp <- Reduce(c, habitatRasters)

lapply(temp, FUN = function(x){
  oldName <- basename(x)
  newRas <- terra::app(rast(x), fun = multi, by = 1000, overwrite = TRUE,
                       filename = file.path(focalHabitatDir1000, oldName),
                       wopt = list(datatype = "INT2U"))
  gc()
})



#Weighted habitat ####
makeWeightedHabitat <- function(tileList, year, outputPath) {


  tileList <- tileList[grep(tileList, pattern = year)] #1985 or 2020
  if (length(tileList) == 0) {stop("incorrect year")}
  #as a float, each individual habitat tile is 20 GB
  #this function writes too many temp files so clean up must occur inside lapply loop

  #put in function for terra so filename can be provided, allowing for easy deletion
  multi <- function(x, by){x * by}

  tempFile1 <- tempfile(fileext = ".tif")
  tempFile2 <- tempfile(fileext = ".tif")
  tempFile3 <- tempfile(fileext = ".tif")
  tempFile4 <- tempfile(fileext = ".tif")

  fire <- app(rast(tileList[grep("naturalDisturbance", tileList)]),
                     fun = multi, by = 0.06, filename = tempFile1)
  harvestY <- app(rast(tileList[grep("harvest_0to5", tileList)]),
                  fun = multi, by = 0.04, filename = tempFile2)
  harvestO <- app(rast(tileList[grep("harvest_6to20", tileList)]),
                  fun = multi, by = 0.04, filename = tempFile3)
  weightedHabitat <- sum(harvestY, harvestO, fire, na.rm = TRUE, filename = tempFile4)
  rm(harvestY, harvestO, fire)
  gc()

  coniferY <- app(x = rast(tileList[grep("youngConifer", tileList)]),
                  fun = multi, by = 0.19, filename = tempFile2, overwrite = TRUE)
  coniferO <- app(rast(tileList[grep("matureConifer", tileList)]),
                  fun = multi, by = 0.25, filename = tempFile1, overwrite = TRUE)
  weightedHabitat <- sum(coniferY, coniferO, weightedHabitat, na.rm = TRUE,
                         filename = tempFile3, overwrite = TRUE)
  rm(coniferY, coniferO)
  gc()

  woodlands <- app(x = rast(tileList[grep("openWoodland", tileList)]),
                   fun = multi, by = 0.22, filename = tempFile1, overwrite = TRUE)
  wetlands <- app(rast(tileList[grep("wetland", tileList)]),
                  fun = multi, by = 0.14, filename = tempFile2, overwrite = TRUE)
  weightedHabitat <- sum(weightedHabitat, woodlands, wetlands, filename = tempFile4,
                         na.rm = TRUE, overwrite = TRUE)
  rm(woodlands, wetlands)
  gc()

  regen <- app(rast(tileList[grep("regenerating", tileList)]),
               fun = multi, by = 0.06, filename = tempFile1, overwrite = TRUE)

  #prepare final file
  tileNum <- stringr::str_extract(tileList[1], pattern = "tile[0-9]+")
  outFile <- file.path(outputPath, paste0("weightedHabitat_", year, "_", tileNum, ".tif"))
  weightedHabitat <- sum(weightedHabitat, regen, na.rm = TRUE, filename = outFile,
                         overwrite = TRUE, wopt = list(datatype = "INT2U"))

  #clean up
  unlink(x = c(tempFile1, tempFile2, tempFile3, tempFile4))
  gc()
  message(tileNum, " complete")
}

habitatRasters <- lapply(tiles, list.files, path = "outputs/focalHabitat1000", full.names = TRUE)
#this could be stored as INT1U - as the theoretical max habitat is 240 (1000 * 0.24 weight)
lapply(habitatRasters, FUN = makeWeightedHabitat, year = 2020, outputPath = weightedDir)
lapply(habitatRasters, FUN = makeWeightedHabitat, year = 1985, outputPath = weightedDir)



#####Composite Habitat#####
#fire = 1, young/old harvest = 2 and 3, young/mature conifer = 4 and 5,
#open woodland 6, wetland 7, regenerating forest = 8
makeCompositeHabitat <- function(tileList, year, outputPath) {
  checkPath(outputPath, create = TRUE)
  tileList <- tileList[grep(tileList, pattern = year)] #1985 or 2020
  if (length(tileList) == 0) {stop("incorrect year")}
  #as a float, each individual habitat tile is 20 GB

  tempFile1 <- tempfile(fileext = ".tif")
  tempFile2 <- tempfile(fileext = ".tif")
  tempFile3 <- tempfile(fileext = ".tif")
  tempFile4 <- tempfile(fileext = ".tif")

  multi <- function(x, by){x * by}

  fire <- rast(tileList[grep("naturalDisturbance", tileList)]) #this one is 1 - so it doesn't need to be multiplied
  harvestY <- app(x = rast(tileList[grep("harvest_0to5", tileList)]),
                  fun = multi, by = 2, filename = tempFile1)
  harvestO <- app(x = rast(tileList[grep("harvest_6to20", tileList)]),
                  fun = multi, by = 3, filename = tempFile2)
  compositeHabitat <- sum(harvestY, harvestO, fire, filename = tempFile3, na.rm = TRUE)
  rm(harvestY, harvestO, fire)
  gc()

  coniferY <- app(rast(tileList[grep("youngConifer", tileList)]),
                  fun = multi, by = 4, filename = tempFile1, overwrite = TRUE)
  coniferO <- app(rast(tileList[grep("matureConifer", tileList)]),
                  fun = multi, by = 5, filename = tempFile2, overwrite = TRUE)
  compositeHabitat <- sum(coniferY, coniferO, compositeHabitat, na.rm = TRUE,
                          filename = tempFile4)
  rm(coniferY, coniferO)
  gc()

  woodlands <- app(rast(tileList[grep("openWoodland", tileList)]),
                   fun = multi, by = 6, filename = tempFile1, overwrite = TRUE)

  wetlands <- app(rast(tileList[grep("wetland", tileList)]),
                  fun = multi, by = 7, filename = tempFile2, overwrite = TRUE)
  compositeHabitat <- sum(compositeHabitat, woodlands, wetlands, na.rm = TRUE,
                          filename = tempFile3, overwrite = TRUE)
  rm(woodlands, wetlands)
  gc()

  regen <- app(rast(tileList[grep("regenerating", tileList)]),
               fun = multi, by = 8, filename = tempFile1, overwrite = TRUE)
  tileNum <- stringr::str_extract(tileList[1], pattern = "tile[0-9]+")
  outFile <- file.path(outputPath, paste0("compositeHabitat_", year, "_", tileNum, ".tif"))
  compositeHabitat <- sum(compositeHabitat, regen, na.rm = TRUE, filename = outFile)
  rm(regen)
  gc()
  stopifnot(max(terra::minmax(compositeHabitat) == 8))


  unlink(c(tempFile1, tempFile2, tempFile3, tempFile4))


  message(tileNum, " complete")

}

habitatClasses <- lapply(tiles, list.files, path = "outputs/raw/", full.names = TRUE)
#some memory leakage happens with this function. Not sure why. Don't recommend running all at once
lapply(habitatClasses, makeCompositeHabitat, year = 2020, outputPath = compositeDir)
lapply(habitatClasses, makeCompositeHabitat, year = 1985, outputPath = compositeDir)
gc()

#upload files
if (FALSE) {
  toZip <- list.files("outputs/weightedHabitat", full.names = TRUE)
  utils::zip(zipfile = "outputs/weightedHabitat.zip",
             files = toZip,
             flags = "-j")
  thePath <- as_dribble("PFC/Yan/Caribou Hindcast Results V2/weighted habitat")
  drive_put("outputs/weightedHabitat.zip", path = thePath)
}

if (FALSE){
# #the composite tiles are 8 GB each, so upload tiles separately)
  toZip <- list.files("outputs/focalHabitat1000", full.names = TRUE)
  utils::zip(zipfile = "outputs/focalHabitat1000.zip",
           files = toZip,
           flags = "-j")
  thePath <- googledrive::as_dribble("PFC/Yan/Caribou Hindcast Results V2/focal habitat layers X 1000")
  drive_put("outputs/focalHabitat1000.zip", path = thePath)
}


