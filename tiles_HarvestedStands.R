
dTypeList <- list.files(path = "GIS/tiles", pattern = "1985_2020_TYPE", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
dYearList <- list.files(path = "GIS/tiles", pattern = "1985_2020_YRt2", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)

####cut blocks age 0-5 and 6-20 ####
RecentCutBlocks <- function(dType, dYear, focalWindow = focalRadius, dBaseYear) {
  tileNum <- stringr::str_extract(dType, pattern = "tile[0-9]+")
  dType = rast(dType)
  dYear = rast(dYear)
  harvestDT = data.table(1:ncell(dType),
                         values(dType))
  names(harvestDT) <- c("pixelID", "harvest")
  rm(dType)
  harvestDT <- harvestDT[harvest == 2]
  gc()
  harvestDT[, harvest := values(dYear)[harvestDT$pixelID]]

  harvestDT[, isYoung := dBaseYear - harvest <= 5 & dBaseYear >= harvest]
  youngHarvest <- rast(dYear)
  rm(dYear)
  gc()
  youngHarvestValues <- rep(NA, length = ncell(youngHarvest))
  youngHarvestValues[harvestDT[isYoung == TRUE,]$pixelID] <- 1
  youngHarvest <- setValues(youngHarvest, youngHarvestValues)
  rm(youngHarvestValues)

  gc()
  focalMatrix <- terra::focalMat(x = youngHarvest, type = "circle", d = focalWindow)
  youngFocalFile <- file.path("outputs", paste0("harvest_0to5_", dBaseYear,  "_focal", focalWindow, "_", tileNum, ".tif"))
  youngFocal <- terra::focal(youngHarvest, w = focalMatrix, fun = sum, na.rm = TRUE, expand = FALSE, filename = youngFocalFile,
                             datatype = "INT1U", overwrite = TRUE) #30 * 30
  harvestDT[, isYoung := NULL] #Old is not the corollary as disturbances can be up to 35
  rm(youngFocal)
  gc()

  harvestDT[, isOld := dBaseYear - harvest > 5 & dBaseYear - harvest <= 20]
  oldHarvestValues <- rep(NA, length = ncell(youngHarvest))
  oldHarvestValues[harvestDT[isOld == TRUE,]$pixelID] <- 1
  youngHarvest <- setValues(youngHarvest, oldHarvestValues)
  oldFocalFile <- file.path("outputs", paste0("harvest_6to20_", dBaseYear,  "_focal", focalWindow, "_", tileNum, ".tif"))
  oldFocal <- terra::focal(youngHarvest, w = focalMatrix, fun = sum, na.rm = TRUE,
                           expand = FALSE, filename = oldFocalFile, overwrite = TRUE)

  rm(oldFocal, youngHarvest, harvestDT)
  gc()
}

#this is where we should talk with Yan -
if (runAnalysis) {
  # Map(RecentCutBlocks, dType = dTypeList, dYear = dYearList,
  #     MoreArgs = list(dBaseYear = 2005, focalWindow = focalRadius))
  Map(RecentCutBlocks, dType = dTypeList[4:6], dYear = dYearList[4:6],
      MoreArgs = list(dBaseYear = 2020, focalWindow = focalRadius))
}


