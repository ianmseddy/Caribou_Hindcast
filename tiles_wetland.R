ageList <- list.files(path = "GIS/tiles", pattern = "att_age", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
landPosList <- list.files(path = "GIS/tiles", pattern = "land_pos", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)

Wetland <- function(age, landPos, focalWindow, dBaseYear) {

  tileNum <- stringr::str_extract(age, pattern = "tile[0-9]+")

  #create focal matrix for use later
  landPos <- rast(landPos)
  focalMatrix <- terra::focalMat(x = landPos, d = focalWindow, type = "circle")

  #create dt for indexing
  dt <- data.table(pixelID = 1:ncell(landPos))

  dt$landPos <- landPos[]
  dt <- dt[landPos == <FIGURE THIS OUT>]
  rm(landPos)
  dt[, landPos := NULL]
  gc()

  #pixels age 0 are unforested - used to distinguish nonforest as I am missing CaNFIR LCC
  age <- rast(age)
  dt[, age := age[][dt$pixelID]]
  wetland <- dt[age == 0,]$pixelID
  rm(dt)
  repvals <- rep(NA, times = ncell(age))
  repvals[wetland] <- 1
  wetland <- setValues(age, repvals)
  rm(repvals, age)
  gc()

  outFile <- file.path("outputs", paste0("wetland_", dBaseYear,  "_focal", focalWindow, "_", tileNum, ".tif"))
  focalOut <- terra::focal(wetland, w = focalMatrix, sum, na.rm = TRUE, expand = FALSE,
                           filename = outFile, overwrite = TRUE)
  rm(wetland)
  gc()

  for (i in 1:3) gc() #terra really hangs on for some reason

}

if (runAnalysis) {
  #filter the years outside of function, I think...
  #here the year is irrelevant, unlike disturbance, because the rasters are snapshot in time.
  #but we should record the year for outputs.
  getYear <- function(pat, List) { return(List[grep(pat, List)])}
  landPosList2020 <- getYear(2020, landPosList)
  ageList2020 <- getYear(2020, ageList)
  Map(Wetland, age = ageList2020, landPos = landPosList2020,
      MoreArgs = list(dBaseYear = 2020, focalWindow = focalRadius))
}

rm(Wetland)

