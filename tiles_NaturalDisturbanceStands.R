#### natural disturbances that occured within 20 years.####
#Because the disturbance time-series begins in 1985, the first year for which we can guarantee a stand originated from natural process
# would be 2005 (ie in 1985 we do not know stands <20 y.o. originated from fire or harvest)
dTypeList <- list.files(path = "GIS/tiles", pattern = "1985_2020_TYPE", full.names = TRUE)
dYearList <- list.files(path = "GIS/tiles", pattern = "1985_2020_YRT2", full.names = TRUE)


#assume that if a pixel is under 20-years of age, forested, and has no harvest record, it was due to natural disturbance
RecentNaturalDist <- function(dType, dYear, age, dBaseYear, lcc){

  tileNum <- stringr::str_extract(dType, pattern = "tile[0-9]+")
  dType <- rast(dType)
  dYear <- rast(dYear)
  age <- rast(age)
  lcc <- rast(lcc)

  compareGeom(dYear, lcc)
  compareGeom(dType, dYear, age)

  burnDT <- data.table(pixelID = 1:ncell(dType), burn = values(dYear), age = values(age))
  names(burnDT) <- c("pixelID", "burn", "age")
  burnDT <- burnDT[burn == 1 | c(burn != 2 & age < 21)]
  gc()
  burnDT[, year := dYear[burnDT$pixelID]]
  burnDT <- burnDT[c(year <= dBaseYear & year + 20 >= dBaseYear) | age < 21]
  #the above operation ensures that the function can accept any year with an accompanying age and lcc layer

  burnDT[, year := NULL]
  gc()
  burnDT[, lcc := lcc[burnDT$pixelID]]
  browser()
  #TODO: check how many (if any) burns occur on non-forest.
  burnDT <- burnDT[lcc %in% c(5:7)] #this means burned non-forest is dropped. worth investigating...
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
  # focalMatrix <- terra::focalMat(x = outRas, d = focalWindow, type = "circle")
  # outFile <- file.path("outputs", paste0("naturalDisturbance", dBaseYear,  "_focal", focalWindow, "_", tileNum, ".tif"))
  # focalOut <- terra::focal(outRas, w = focalMatrix, sum, na.rm = TRUE, expand = FALSE,
  #                          filename = outFile, overwrite = TRUE)
  #recording focal window size in filename in case it changes.
  rm(outRas)
  gc()
}
#
if (runAnalysis) {
  Map(RecentNaturalDist, dType = dTypeList, dYear = dYearList,
      MoreArgs = list(dBaseYear = 2020))
}
