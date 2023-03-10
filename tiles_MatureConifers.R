#identify the age 70 conifer stands with canopy cover > 30%
canopyCoverList <- list.files(path = "GIS/tiles", pattern = "att_closure", full.names = TRUE)
ageList <- list.files(path = "GIS/tiles", pattern = "att_age", full.names = TRUE)
percDecidList <- list.files(path = "GIS/tiles", pattern = "prcD", full.names = TRUE)

MatureConifer <- function(age, canopyCover, percDecid, distYear = NULL, dBaseYear) {

  tileNum <- stringr::str_extract(age, pattern = "tile[0-9]+")

  percDecid <- rast(percDecid)
  #sanity check

  canopyCover <- rast(canopyCover)
  age <- rast(age)
  compareGeom(percDecid, canopyCover, age)


  dt <- data.table(pixelID = 1:ncell(percDecid))
  dt$percDecid <- percDecid[]

  #keep only pixels that are 25% or more coniferous
  dt <- dt[percDecid < 75,]
  dt[, percDecid := NULL]
  rm(percDecid)
  gc()
  #keep only pixels with 30% or more cover
  dt[, cover := canopyCover[][dt$pixelID]]
  #keeping index outside of values (i.e. instead of canopyCover[<index>]) is significantly faster!
  dt <- dt[cover >= 25,]
  dt[, cover := NULL]
  rm(canopyCover)
  gc()


  #remove the pixels that are 20+ but disturbed <20 y.a.
  if (!is.null(distYear)) {
    distYear <- rast(distYear)
    dt[, distYear := distYear[dt$pixelID]]
    dt <- dt[dBaseYear - distYear > 20 | is.na(distYear),]
    dt[, distYear := NULL]
  }

  #write young conifer

  dt[, age := age[dt$pixelID]]
  youngConifer <- dt[age > 49 & age < 70]$pixelID
  oldConifer <- dt[age >= 70]$pixelID
  rm(dt)
  repvals <- rep(NA, times = ncell(age))
  repvals[youngConifer] <- 1
  youngConifer <- setValues(age, repvals)
  rm(repvals)
  outFile <- file.path("outputs/raw", paste0("youngConifer", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(youngConifer, filename = outFile, datatype = "INT1U", overwrite = TRUE)

  rm(youngConifer)
  gc()
  #write old conifer
  repvals <- rep(NA, times = ncell(age))
  repvals[oldConifer] <- 1
  oldConifer <- setValues(age, repvals)
  rm(repvals)
  outFile <- file.path("outputs/raw", paste0("matureConifer", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(oldConifer, filename = outFile, datatype = "INT1U", overwrite = TRUE)
  rm(oldConifer)
  for (i in 1:3) gc() #terra really hangs on for some reason

}


if (runAnalysis) {

  percDecidList2020 <- getYear(2020, percDecidList)
  ageList2020 <- getYear(2020, ageList)
  canopyCoverList2020 <- getYear(2020, canopyCoverList)
  distYearList <- list.files(path = "GIS/tiles", pattern = "YRT2", full.names = TRUE)
  Map(MatureConifer, age = ageList2020, canopyCover = canopyCoverList2020,
      percDecid = percDecidList2020, distYear = distYearList,
      MoreArgs = list(dBaseYear = 2020))

  #no need to pass disturbance year in 1985
  percDecidList1985 <- getYear(1985, percDecidList)
  ageList1985 <- getYear(1985, ageList)
  canopyCoverList1985 <- getYear(1985, canopyCoverList)
  Map(MatureConifer, age = ageList1985, canopyCover = canopyCoverList1985, percDecid = percDecidList1985,
      MoreArgs = list(dBaseYear = 1985))
}

rm(MatureConifer)
