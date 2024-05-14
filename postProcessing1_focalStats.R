#run the focal stats
library(parallel)
#the radius argument is 1000 - the area is 3.14 km2

inputDir <- "outputs/raw"
outputDir <- "outputs/focalHabitat"
cl <- makeCluster(ncores)

focalFiles <- list.files(inputDir, pattern = ".tif", full.names = TRUE) %>%
  .[grep(., pattern = "2020")]
focalMatrix <- terra::focalMat(x = rast(focalFiles[1]), d = focalRadius, type = "circle")

#run on 1985 first

focalStats <- function(rastFile, weights = focalMatrix, outDir) {
  baseName <- basename(rastFile)
  outFile <- file.path(outDir, paste0("focal_", baseName))
  inFile <- terra::rast(rastFile)

  out <- focal(inFile, w = weights, fun = "sum",
               na.rm = TRUE,
               expand = FALSE)
  terra::varnames(out) <- baseName
  convertToInt <- function(x){x * 1000}
  terra::app(out, convertToInt,
             filename = outFile,
             overwrite = TRUE,
             wopt = list(datatype = "INT2U"))
  gc()
}

#each focal operation is ~22 GB/tile/year/habitat class -
clusterExport(cl, varlist = c("focalMatrix"))
clusterEvalQ(cl, {
  library(terra)
})


# clusterEvalQ(cl, focalMatrix)
parLapply(cl, X = focalFiles, fun = focalStats,
          outDir = outputDir)
stopCluster(cl)

#check all is kosher
tiles <- paste0("tile", c(ny * nx))
nComplete <- unlist(lapply(tiles, list.files, path = outputDir, full.names = TRUE) %>%
  lapply(., length))
if (!all(nComplete == length(tiles))) {
  stop("aahhh you must be missing tiles!")
}
