# some disturbances leave behind remnant vegetation that will be double-counted as Open Woodlands
# if the remaining veg has age > 50 - must use the output harvest and natural distance
# layers, as opposed to the CanLad, which can't correct this issue for the 1985 layer

OpenWoodlands <- function(age, canopyCover, percDecid, pos, youngHarvest, oldHarvest, fire, dBaseYear) {


  tileNum <- stringr::str_extract(age, pattern = "tile[0-9]+")

  canopyCover <- rast(canopyCover)
  percDecid <- rast(percDecid)
  age <- rast(age)
  youngHarvest <- rast(youngHarvest)
  oldHarvest <- rast(oldHarvest)
  fire <- rast(fire)

  compareGeom(canopyCover, percDecid, age)
  compareGeom(fire, youngHarvest, age)
  compareGeom(oldHarvest, age)

  dt <- data.table(pixelID = 1:ncell(percDecid))
  dt[, percDecid := as.vector(percDecid)]

  #keep only pixels that are 50% or more coniferous
  dt <- dt[percDecid < 50,]
  dt[, percDecid := NULL]
  rm(percDecid)
  gc()
  #keep only pixels less than 25% cover

  #keep only pixels age 50+
  dt[, age := as.vector(age)[dt$pixelID]]
  dt <- dt[age > 49,]
  dt[, age := NULL]
  gc()


  list.files("outputs/raw")
  dt[, cover := canopyCover[][dt$pixelID]]
  #keeping index outside of values (i.e. instead of canopyCover[<index>])
  dt <- dt[cover < 25,]
  dt[, cover := NULL]
  rm(canopyCover)
  gc()

  #remove any pixels that were disturbed recently (this is rare but happens)
  dt[, fire := as.vector(fire)[pixelID]]
  dt[, oldHarvest := as.vector(oldHarvest)[pixelID]]
  dt[, youngHarvest := as.vector(youngHarvest)[pixelID]]
  dt <- dt[is.na(fire) & is.na(oldHarvest) & is.na(youngHarvest),]
  dt[, c("fire", "oldHarvest", "youngHarvest") := NULL]
  gc()

  pos <- rast(pos)
  dt[, pos := pos[][dt$pixelID]]
  dt <- dt[pos != 5] #remove wetlands
  dt[, pos := NULL]
  rm(pos)
  gc()

  openWoodland <- dt$pixelID
  rm(dt)
  repvals <- rep(NA, times = ncell(age))
  repvals[openWoodland] <- 1
  openWoodland <- setValues(age, repvals)
  rm(repvals)

  outFile <- file.path("outputs/raw", paste0("openWoodland", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(openWoodland, filename = outFile, datatype = "INT1U", overwrite = TRUE)

  rm(openWoodland)
  gc()

  for (i in 1:3) gc() #terra really hangs on for some reason

}

if (runAnalysis) {

  posList <- list.files(path = "GIS/tiles", pattern = "pos", full.names = TRUE)

  #2020
  canopyCoverList2020 <- getAtt("att_closure", Year = 2020)
  ageList2020 <- getAtt("att_age", Year = 2020)
  percDecidList <- getAtt("prcD", 2020)
  youngHarvestList2020 <- getAtt("harvest_0to5", 2020, Path = "outputs/raw")
  oldHarvestList2020 <- getAtt("harvest_6to20", 2020, Path = "outputs/raw")
  fireList2020 <- getAtt("natural", 2020, Path = "outputs/raw")

  Map(OpenWoodlands, age = ageList2020, canopyCover = canopyCoverList2020,
      percDecid = percDecidList2020, pos = posList, youngHarvest = youngHarvestList2020,
      oldHarvest = oldHarvestList2020, fire = fireList2020,
      MoreArgs = list(dBaseYear = 2020))

  #1985
  canopyCoverList1985 <- getAtt("att_closure", Year = 1985)
  ageList1985 <- getAtt("att_age", Year = 1985)
  percDecidList1985 <- getAtt("prcD", 1985)
  youngHarvestList1985 <- getAtt("harvest_0to5", 1985, Path = "outputs/raw")
  oldHarvestList1985 <- getAtt("harvest_6to20", 1985, Path = "outputs/raw")
  fireList1985 <- getAtt("natural", 1985, Path = "outputs/raw")

  Map(OpenWoodlands, age = ageList1985, canopyCover = canopyCoverList1985,
      percDecid = percDecidList1985, pos = posList, youngHarvest = youngHarvestList1985,
      oldHarvest = oldHarvestList1985, fire = fireList1985,
      MoreArgs = list(dBaseYear = 1985))
}

rm(OpenWoodlands, ageList1985, canopyCoverList1985, percDecidList1985, youngHarvestList1985,
   oldHarvestList1985, fireList1985, fireList2020, ageList2020, canopyCoverList2020,
   oldHarvestList2020, youngHarvestList2020, posList)
