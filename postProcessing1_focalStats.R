#run the focal stats
library(parallel)
#the radius argument is 1000 - the area is 3.14 km2

if (Sys.info()["sysname"] == "Linux") {
  inputDir <- "outputs/raw"
  outputDir <- "outputs/focalHabitat"
  num_cores <- 5
  cl <- makeCluster(num_cores)
} else {
  inputDir <- "D:/Ian/YanBoulanger/maskedHabitat"
  outputDir <- "D:/Ian/YanBoulanger/focalHabitat"
  num_cores <- 6
  cl <- makeCluster(num_cores)
}

focalFiles <- list.files(inputDir, pattern = ".tif", full.names = TRUE)
focalFiles <- focalFiles[c(grep("Conifer2020", focalFiles),
                           grep("harvest_0to5_2020", focalFiles),
                           grep("harvest_6to20_2020", focalFiles))]
focalMatrix <- terra::focalMat(x = rast(focalFiles[1]), d = focalRadius, type = "circle")

#run on 1985 first

focalStats <- function(rastFile, weights = focalMatrix, outDir) {
  baseName <- basename(rastFile)
  outFile <- file.path(outDir, paste0("focal_", baseName))
  inFile <- rast(rastFile)
  set.names(inFile, baseName) #these rasters have been inheriting the wrong names..
  focal(inFile, w = weights, sum, na.rm = TRUE,
        expand = FALSE, filename = outFile, overwrite = TRUE)
  rm(inFile)
  gc()
  return(NULL)
}

#each focal operation is ~22 GB/tile/year/habitat class -
clusterExport(cl, varlist = c("focalMatrix"))
clusterEvalQ(cl, {
  library(terra)
})


# clusterEvalQ(cl, focalMatrix)
parLapply(cl, focalFiles, focalStats,
          outDir = outputDir)
stopCluster(cl)

#check all is kosher
tiles <- paste0("tile", c(ny * nx))
nComplete <- unlist(lapply(tiles, list.files, path = outputDir, full.names = TRUE) %>%
  lapply(., length))
if (!all(nComplete == length(tiles))) {
  stop("aahhh")
}
