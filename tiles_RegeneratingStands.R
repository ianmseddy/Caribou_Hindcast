#5)	Identify Regenerating Stands: any stands > 20 years that aren’t conifer 50+ (ie none of the above classes)

ageList <- list.files(path = "GIS/tiles", pattern = "att_age", full.names = TRUE)
percDecidList <- list.files(path = "GIS/tiles", pattern = "prcD", full.names = TRUE)
posList <- list.files(path = "GIS/tiles", pattern = "pos", full.names = TRUE)
canopyCoverList <- list.files(path = "GIS/tiles", pattern = "att_closure", full.names = TRUE)

RegeneratingStands <- function(age, percDecid, pos, canopyCover, distYear = NULL, dBaseYear) {

  tileNum <- stringr::str_extract(age, pattern = "tile[0-9]+")

  #create focal matrix for use later
  age <- rast(age)
  # focalMatrix <- terra::focalMat(x = age, d = focalWindow, type = "circle")

  #create dt for indexing
  dt <- data.table(pixelID = 1:ncell(age))

  dt$age <- age[]
  dt <- dt[age > 20]
  rm(age)
  gc()

  #remove the pixels that are 20+ but disturbed <20 y.a.
  if (!is.null(distYear)) {
    distYear <- rast(distYear)
    dt[distYear := distYear[dt$pixelID]]
    dt <- dt[dBaseYear - distYear > 20,]
    dt[,distYear := NULL]
  }

  #pixels that are 25% or more coniferous will fall into woodland, mature conifer
  #so keep all deciduous or young coniferous
  percDecid <- rast(percDecid)
  dt[, percDecid := percDecid[][dt$pixelID]]

  pos <- rast(pos)
  dt[, pos := pos[][dt$pixelID]]

  #any wetland is not regenerating stand (it may be mature conifer or disturbed or wetland)
  dt <- dt[!pos == 5]
  rm(pos)
  dt[, pos := NULL]

  canopyCover <- rast(canopyCover)
  dt[, cc := canopyCover[][dt$pixelID]]

  #assume age 50+ with cover below 30 is wetland or woodland, and won't regenerate to forest
  #this excludes deciduous woodlands (along with e.g. grassland)
  dt <- dt[!c(age > 50 & cc < 25)]

  #remove the coniferous age 50+
  #this assumes disturbed mature conifer on wetland becomes regenerating stand, not wetland
  regeneratingStand <- dt[percDecid > 75 | c(percDecid <= 75 & age < 50),]$pixelID
  rm(dt)
  repvals <- rep(NA, times = ncell(percDecid))
  repvals[regeneratingStand] <- 1
  regeneratingStand <- setValues(percDecid, repvals)
  rm(repvals, percDecid)
  gc()

  outFile <- file.path("outputs/raw", paste0("regeneratingStand", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(regeneratingStand, filename = outFile, datatype = "INT1U", overwrite = TRUE)

  # outFile <- file.path("outputs", paste0("regeneratingStand_", dBaseYear,  "_focal", focalWindow, "_", tileNum, ".tif"))
  # focalOut <- terra::focal(regeneratingStand, w = focalMatrix, sum, na.rm = TRUE, expand = FALSE,
  #                          filename = outFile, overwrite = TRUE)
  rm(regeneratingStand)
  gc()

  for (i in 1:3) gc() #terra really hangs on for some reason

}

if (runAnalysis) {
  #filter the years outside of function, I think...
  #here the year is irrelevant, unlike disturbance, because the rasters are snapshot in time.
  #but we should record the year for outputs.
  percDecidList2020 <- getYear(2020, percDecidList)
  ageList2020 <- getYear(2020, ageList)
  canopyCoverList2020 <- getYear(2020, canopyCoverList)
  distYearList <- list.files(path = "GIS/tiles", pattern = "YRT2", full.names = TRUE)
  Map(RegeneratingStands, age = ageList2020, percDecid = percDecidList2020,
      canopyCover = canopyCoverList2020, pos = posList, distYear = distYearList,
      MoreArgs = list(dBaseYear = 2020))
  #1985
  percDecidList1985 <- getYear(1985, percDecidList)
  ageList1985 <- getYear(1985, ageList)
  canopyCoverList1985 <- getYear(1985, canopyCoverList)
  Map(RegeneratingStands, age = ageList1985, percDecid = percDecidList1985,
      canopyCover = canopyCoverList1985, pos = posList,
      MoreArgs = list(dBaseYear = 1985))
}

rm(RegeneratingStands)
