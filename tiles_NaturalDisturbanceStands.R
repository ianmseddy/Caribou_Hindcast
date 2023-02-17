#### natural disturbances that occured within 20 years.#### harvest)

#assume that if a pixel is under 20-years of age, forested, and has no harvest record, it was due to natural disturbance
#the exception is for pixels on wetland (which also override disturbance)
RecentNaturalDist <- function(dType, dYear, age, dBaseYear, lcc, wetland){

  tileNum <- stringr::str_extract(dType, pattern = "tile[0-9]+")
  dType <- rast(dType)
  dYear <- rast(dYear)
  age <- rast(age)
  lcc <- rast(lcc)
  wetland <- rast(wetland)

  compareGeom(dYear, lcc, wetland)
  compareGeom(dType, dYear, age)

  burnDT <- data.table(pixelID = 1:ncell(dType), burn = values(dType), age = values(age))
  names(burnDT) <- c("pixelID", "burn", "age")
  burnDT <- burnDT[!is.na(age)]
  burnDT <- burnDT[burn == 1 | c(burn != 2 & age < 21)]
  gc()
  burnDT[, year := dYear[burnDT$pixelID]]
  burnDT <- burnDT[c(year <= dBaseYear & year + 20 >= dBaseYear) | age < 21]
  #the above operation ensures that the function can accept any year with an accompanying age and lcc layer

  burnDT[, year := NULL]
  gc()
  burnDT[, lcc := lcc[burnDT$pixelID]]
  burnDT <- burnDT[lcc %in% c(5:7)] #this means burned non-forest is dropped. worth investigating...
  gc()

  #drop wetland
  burnDT[, wetland := wetland[burnDT$pixelID]]
  burnDT <- burnDT[is.na(wetland),] #burned wetland is still wetland

  burnDT <- burnDT$pixelID
  burnVals <- rep(NA, ncell(dYear))
  burnVals[burnDT] <- 1
  rm(burnDT)
  outRas <- rast(dYear)
  outRas <- setValues(x = outRas, burnVals)


  outFile <- file.path("outputs/raw", paste0("naturalDisturbance", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(outRas, filename = outFile, datatype = "INT1U", overwrite = TRUE)
  rm(outRas)
  gc()
}

if (runAnalysis) {
  lccList <- getAtt(Att = "VegType", 2020)
  dTypeList <- getAtt(Att = "1985_2020_TYPE", 2020)
  dYearList <- getAtt(Att = "1985_2020_YRT2", 2020)
  wetlandList <- getAtt("wetland", 2020, "outputs/raw")
  ageList <- getAtt("age", 2020)
  Map(RecentNaturalDist, dType = dTypeList, dYear = dYearList, lcc = lccList,
      wetland = wetlandList, age = ageList,
      MoreArgs = list(dBaseYear = 2020))
}
