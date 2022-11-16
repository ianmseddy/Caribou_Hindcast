#run the focal stats
library(parallel)
#apparently it is faster if things are in memory when running focal, and then written to SSD

#the radius argument is 1000 - the area is supposed to be 3.14 km2

outputDir <- "D:/Ian/YanBoulanger/focalHabitat"

focalFiles <- list.files("D:/Ian/YanBoulanger/maskedHabitat", pattern = ".tif", full.names = TRUE)
focalMatrix <- terra::focalMat(x = rast(focalFiles[1]), d = focalRadius, type = "circle")

focalStats <- function(rastFile, weights = focalMatrix, outDir) {
  library(terra)

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
num_cores <- 3
cl <- makeCluster(num_cores, type = "PSOCK")
clusterExport(cl, "focalMatrix")
clusterEvalQ(cl, {
  library(terra)
})


# clusterEvalQ(cl, focalMatrix)
parLapply(cl, focalFiles, focalStats,
          outDir = outputDir)
stopCluster(cl)

#wetlands were not in /masked because they were the primary masking layer
wetlands <- list.files("outputs/raw/", pattern = "wetland", full.names = TRUE)
parLapply(cl, wetlands, focalStats, outDir = outputDir)


#check all is kosher
tiles <- paste0("tile", 1:6)
lapply(tiles, list.files, path = outputDir, full.names = TRUE) %>%
  lapply(., length)
#16 each tile, due to 8 habitats * 2 years. Done!
