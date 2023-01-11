#### natural disturbances that occured within 20 years.####
#Because the disturbance time-series begins in 1985, the first year for which we can guarantee a stand originated from natural process
# would be 2005 (ie in 1985 we do not know stands <20 y.o. originated from fire or harvest)
dTypeList <- list.files(path = "GIS/tiles", pattern = "1985_2020_TYPE", full.names = TRUE)
dYearList <- list.files(path = "GIS/tiles", pattern = "1985_2020_YRT2", full.names = TRUE)
landPosList <- list.files(path = "GIS/tiles", pattern = "land_pos", full.names = TRUE)

RecentNaturalDist <- function(dType, dYear, dBaseYear, landPos){

  tileNum <- stringr::str_extract(dType, pattern = "tile[0-9]+")
  dType <- rast(dType)
  dYear <- rast(dYear)
  landPos <- rast(landPos)
  lcc <- rast(lcc)

  compareGeom(dYear, lcc, landPos)
  compareGeom(dType, dYear)

  burnDT <- data.table(pixelID = 1:ncell(dType), burn = values(dYear))
  names(burnDT) <- c("pixelID", "burn", "age")
  burnDT <- burnDT[burn == 1]
  gc()
  burnDT[, year := dYear[burnDT$pixelID]]
  burnDT <- burnDT[c(year <= dBaseYear & year + 20 >= dBaseYear)]

  burnDT[, year := NULL]
  gc()
  burnDT[, pos := landPos[burnDT$pixelID]]

  burnDT <- burnDT[pos != 5] #wetland habitat supersedes natural disturbance
  gc()
  burnDT <- burnDT$pixeLID

  #easier to do the logical queries on the non-NA and then rebuild the rasters with the eventual binary values
  burnVals <- rep(NA, ncell(dYear))
  #20 = year <= dBaseYear & year + 20 > dBaseYear
  #in 2020, a pixel that burned in 2000 would be last to qualify
  burnVals[burnDT] <- 1
  rm(burnDT)
  outRas <- rast(dYear)
  outRas <- setValues(x = outRas, burnVals)

  outFile <- file.path("outputs/raw", paste0("naturalDisturbance", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(outRas, filename = outFile, datatype = "INT1U", overwrite = TRUE)

  rm(outRas)
  gc()
}
#
if (runAnalysis) {
  Map(RecentNaturalDist, dType = dTypeList, dYear = dYearList, landPos = landPosList,
      MoreArgs = list(dBaseYear = 2020))
}
