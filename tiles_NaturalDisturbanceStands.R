#### natural disturbances that occured within 20 years.####
#Because the disturbance time-series begins in 1985, the first year for which we can guarantee a stand originated from fire
# would be 2005 (ie in 1985 we do not know stands <20 y.o. originated from fire or harvest)
dTypeList <- list.files(path = "GIS/tiles", pattern = "1985_2020_TYPE", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
dYearList <- list.files(path = "GIS/tiles", pattern = "1985_2020_YRt2", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)


RecentNaturalDist <- function(dType, dYear, dBaseYear, focalWindow = focalRadius){

  tileNum <- stringr::str_extract(dType, pattern = "tile[0-9]+")
  dType <- rast(dType)
  dYear <- rast(dYear)
  #using terra::subst is horrifically slow so I use data.tables instead
  # burns <- terra::subst(dType, 2:255, NA) #this won't work for harvest
  burnDT <- data.table(pixelID = 1:ncell(dType), burn = values(dType))
  #why does the col name inherit the layer name?
  names(burnDT) <- c("pixelID", "burn")
  burnDT <- burnDT[burn == 1,]
  rm(dType)
  gc()#30 GB ram - Yikes
  burnDT[, year := c(values(dYear)[burnDT$pixelID])]
  #easier to do the logical queries on the non-NA and then rebuild the rasters with the eventual binary values
  burnVals <- rep(NA, ncell(dYear))
  #20 = year <= dBaseYear & year + 20 > dBaseYear
  #in 2020, a pixel that burned in 2020 would be last to qualify
  burnVals[burnDT[year <= dBaseYear & year + 20 >= dBaseYear]$pixelID] <- 1
  rm(burnDT)
  outRas <- rast(dYear)
  outRas <- setValues(x = outRas, burnVals)

  focalMatrix <- terra::focalMat(x = outRas, d = focalWindow, type = "circle")
  outFile <- file.path("outputs", paste0("naturalDisturbance", dBaseYear,  "_focal", focalWindow, "_", tileNum, ".tif"))
  focalOut <- terra::focal(outRas, w = focalMatrix, fun = sum, na.rm = TRUE, expand = FALSE, filename = outFile,
                           datatype = "INT1U", overwrite = TRUE) #30 * 30
  #recording focal window size in filename in case it changes.
  rm(outRas, focalOut)
  gc()
}
#
if (runAnalysis) {
  # Map(RecentNaturalDist, dType = dTypeList[3:6], dYear = dYearList[3:6],
  #     MoreArgs = list(dBaseYear = 2005, focalWindow = focalRadius)) Not sure if we need this yet - perhaps we just use age for these
  Map(RecentNaturalDist, dType = dTypeList[3:6], dYear = dYearList[3:6],
      MoreArgs = list(dBaseYear = 2020, focalWindow = focalRadius))
}
