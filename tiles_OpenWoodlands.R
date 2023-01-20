canopyCoverList <- list.files(path = "GIS/tiles", pattern = "att_closure", full.names = TRUE)
ageList <- list.files(path = "GIS/tiles", pattern = "att_age", full.names = TRUE)
percDecidList <- list.files(path = "GIS/tiles", pattern = "prcD", full.names = TRUE)
posList <- list.files(path = "GIS/tiles", pattern = "pos", full.names = TRUE)

OpenWoodlands <- function(age, canopyCover, percDecid, pos, dBaseYear) {

  tileNum <- stringr::str_extract(age, pattern = "tile[0-9]+")

  #create focal matrix for use later
  percDecid <- rast(percDecid)
  # focalMatrix <- terra::focalMat(x = percDecid, d = focalWindow, type = "circle")

  dt <- data.table(pixelID = 1:ncell(percDecid))
  dt$percDecid <- percDecid[]

  #keep only pixels that are 50% or more coniferous
  dt <- dt[percDecid < 50,]
  dt[, percDecid := NULL]
  rm(percDecid)
  gc()
  #keep only pixels less than 30% cover
  canopyCover <- rast(canopyCover)
  dt[, cover := canopyCover[][dt$pixelID]]
  #keeping index outside of values (i.e. instead of canopyCover[<index>])
  dt <- dt[cover < 25,]
  dt[, cover := NULL]
  rm(canopyCover)
  gc()

  pos <- rast(pos)
  dt[, pos := pos[][dt$pixelID]]
  dt <- dt[pos != 5] #remove wetlands
  dt[, pos := NULL]
  rm(pos)
  gc()

  age <- rast(age)
  dt[, age := age[][dt$pixelID]]
  openWoodland <- dt[age > 49,]$pixelID
  rm(dt)
  repvals <- rep(NA, times = ncell(age))
  repvals[openWoodland] <- 1
  openWoodland <- setValues(age, repvals)
  rm(repvals)

  outFile <- file.path("outputs/raw", paste0("openWoodland", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(openWoodland, filename = outFile, datatype = "INT1U", overwrite = TRUE)

  # outFile <- file.path("outputs", paste0("openWoodland_", dBaseYear,  "_focal", focalWindow, "_", tileNum, ".tif"))
  # focalOut <- terra::focal(openWoodland, w = focalMatrix, sum, na.rm = TRUE, expand = FALSE,
  #                          filename = outFile, overwrite = TRUE)
  rm(openWoodland)
  gc()

  for (i in 1:3) gc() #terra really hangs on for some reason

}

if (runAnalysis) {
  percDecidList2020 <- getYear(2020, percDecidList)
  ageList2020 <- getYear(2020, ageList)
  canopyCoverList2020 <- getYear(2020, canopyCoverList)
  Map(OpenWoodlands, age = ageList2020, canopyCover = canopyCoverList2020,
      percDecid = percDecidList2020, pos = posList,
      MoreArgs = list(dBaseYear = 2020))

  #1985
  percDecidList1985 <- getYear(1985, percDecidList)
  ageList1985 <- getYear(1985, ageList)
  canopyCoverList1985 <- getYear(1985, canopyCoverList)
  Map(OpenWoodlands, age = ageList1985, canopyCover = canopyCoverList1985,
      percDecid = percDecidList1985, pos = posList,
      MoreArgs = list(dBaseYear = 1985))
}

rm(OpenWoodlands)
