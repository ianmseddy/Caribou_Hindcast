ageList <- list.files(path = "GIS/tiles", pattern = "att_age", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
landPosList <- list.files(path = "GIS/tiles", pattern = "land_pos", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
lccList <- list.files(path = "GIS/tiles", pattern = "VegType")
percDecidList <- list.files(path = "GIS/tiles", pattern = "prcD", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
canopyCoverList <- list.files(path = "GIS/tiles", pattern = "att_closure", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)

Wetland <- function(age, landPos, lcc, canopyCover, percDecid, focalWindow, dBaseYear) {

  tileNum <- stringr::str_extract(age, pattern = "tile[0-9]+")

  #create focal matrix for use later
  landPos <- rast(landPos)
  focalMatrix <- terra::focalMat(x = landPos, d = focalWindow, type = "circle")

  #create dt for indexing
  dt <- data.table(pixelID = 1:ncell(landPos))

  dt$landPos <- landPos[]
  dt <- dt[landPos == 5]
  rm(landPos)
  dt[, landPos := NULL]
  gc()

  #lcc
  lcc <- rast(lcc)
  dt[, lcc := lcc[][dt$pixelID]]
  dt <- dt[lcc != 8] #remove open water wetland
  dt[, lcc := NULL]
  rm(lcc)
  gc()


  #include majority conifer that is open woodland (age 50+, cc <30) and all 20+ deciduous
  #exclude majority conifer that is younger than 50 or c(age 50+ and cc >30)
  percDecid <- rast(percDecid)
  dt[, percDecid := percDecid[][dt$pixelID]]
  rm(percDecid)

  age <- rast(age)
  dt[, age := age[][dt$pixelID]]
  rm(age)

  cc <- rast(canopyCover)
  dt[, cc := cc[][dt$pixelID]]
  rm(cc)


  dt <- dt[!c(percDecid <= 50 & age > 50 & cc > 30)] #this preserves open woodlands and deciduous
  dt <- dt[]

  repvals <- rep(NA, times = ncell(age))
  repvals[wetland] <- 1
  wetland <- setValues(age, repvals)
  rm(repvals, age)
  gc()

  #write the binary output
  outFile <- file.path("outputs/raw", paste0("wetland", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(wetland, filename = outFile, datatype = "INT1U", overwrite = TRUE)

  outFile <- file.path("outputs", paste0("wetland_", dBaseYear,  "_focal", focalWindow, "_", tileNum, ".tif"))
  focalOut <- terra::focal(wetland, w = focalMatrix, sum, na.rm = TRUE, expand = FALSE,
                           filename = outFile, overwrite = TRUE)
  rm(wetland)
  gc()

  for (i in 1:3) gc() #terra really hangs on for some reason

}

if (runAnalysis) {
  getYear <- function(pat, List) { return(List[grep(pat, List)])}
  #2020 first
  landPosList2020 <- getYear(2020, landPosList)
  ageList2020 <- getYear(2020, ageList)
  canopyCoverList2020 <- getYear(2020, canopyCoverList)
  Map(Wetland, age = ageList2020, landPos = landPosList2020,
      percDecid = percDecidList2020, canopyCover = canopyCoverList2020,
      MoreArgs = list(dBaseYear = 2020, focalWindow = focalRadius))
  #1985 afer
}

rm(Wetland)

