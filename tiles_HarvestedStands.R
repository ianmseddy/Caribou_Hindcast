
####cut blocks age 0-5 and 6-20 ####
RecentCutBlocks <- function(dType, dYear, wetland, dBaseYear) {
  tileNum <- stringr::str_extract(dType, pattern = "tile[0-9]+")
  dType = rast(dType)
  dYear = rast(dYear)
  wetland <- rast(wetland)

  compareGeom(dType, dYear, wetland)

  harvestDT = data.table(1:ncell(dType),
                         values(dType, mat = FALSE))
  names(harvestDT) <- c("pixelID", "harvest")
  rm(dType)
  harvestDT <- harvestDT[harvest == 2]
  gc()
  harvestDT[, harvest := values(dYear, mat = FALSE)[harvestDT$pixelID]]

  #drop wetland
  harvestDT[, wetland := wetland[harvestDT$pixelID]]
  harvestDT <- harvestDT[is.na(wetland),] #harvested wetland is still wetland

  harvestDT[, isYoung := dBaseYear - harvest <= 5 & dBaseYear >= harvest]
  youngHarvest <- rast(dYear)
  rm(dYear)
  gc()
  youngHarvestValues <- rep(NA, length = ncell(youngHarvest))
  youngHarvestValues[harvestDT[isYoung == TRUE,]$pixelID] <- 1
  youngHarvest <- setValues(youngHarvest, youngHarvestValues)
  rm(youngHarvestValues)

  gc()
  # focalMatrix <- terra::focalMat(x = youngHarvest, type = "circle", d = focalWindow)
  youngHarvestFile <- file.path("outputs/raw", paste0("harvest_0to5_", dBaseYear, "_", tileNum, ".tif"))
  writeRaster(youngHarvest, filename = youngHarvestFile, overwrite = TRUE)
  harvestDT[, isYoung := NULL] #Old is not the corollary as disturbances can be up to 35

  gc()

  harvestDT[, isOld := dBaseYear - harvest > 5 & dBaseYear - harvest <= 20]
  oldHarvestValues <- rep(NA, length = ncell(youngHarvest))
  oldHarvestValues[harvestDT[isOld == TRUE,]$pixelID] <- 1
  youngHarvest <- setValues(youngHarvest, oldHarvestValues)
  oldHarvestFile <- file.path("outputs/raw", paste0("harvest_6to20_", dBaseYear, "_", tileNum, ".tif"))
  writeRaster(youngHarvest, oldHarvestFile, overwrite = TRUE)


  rm(youngHarvest, harvestDT)
  gc()
}

#this is where we should talk with Yan -
if (runAnalysis) {

  dTypeList <- list.files(path = "GIS/tiles", pattern = "1985_2020_TYPE", full.names = TRUE)
  dYearList <- list.files(path = "GIS/tiles", pattern = "1985_2020_YRT2", full.names = TRUE)
  wetlandList <- getAtt("wetland", 2020, "outputs/raw")

  Map(RecentCutBlocks, dType = dTypeList, dYear = dYearList, wetland = wetlandList,
      MoreArgs = list(dBaseYear = 2020))
}


