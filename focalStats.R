#run the focal stats
library(parallel)
#apparently it is faster if things are in memory when running focal, and then written to SSD

#the radius argument is 564 (1 km2 = 1000000m2 = pi*r2, solving for r = 564)
#if we used a square, it would be 1000 as the argument is the length of 1 or 2 sides
#currently writing to a SSD to speed this part up
focalFiles <- list.files("D:/Ian/YanBoulanger/maskedHabitat", pattern = ".tif", full.names = TRUE)
focalMatrix <- terra::focalMat(x = rast(focalFiles[1]), d = 564, type = "circle")

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
parLapply(cl, focalFiles[grep(focalFiles, pattern = "tile4")], focalStats,
             outDir = "D:/Ian/YanBoulanger/focalHabitat")
stopCluster(cl)

#need to finish 4, 5, and 6, and redo wetlands
