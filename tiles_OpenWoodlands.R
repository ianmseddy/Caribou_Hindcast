canopyCoverList <- list.files(path = "GIS/tiles", pattern = "att_closure", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
ageList <- list.files(path = "GIS/tiles", pattern = "att_age", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
percDecidList <- list.files(path = "GIS/tiles", pattern = "prcD", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)

OpenWoodlands <- function(age, canopyCover, percDecid, focalWindow, dBaseYear) {

  tileNum <- stringr::str_extract(age, pattern = "tile[0-9]+")

  #create focal matrix for use later
  percDecid <- rast(percDecid)
  focalMatrix <- terra::focalMat(x = percDecid, d = focalWindow, type = "circle")

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
  dt <- dt[cover < 30,]
  dt[, cover := NULL]
  rm(canopyCover)
  gc()

  #age must be 50+
  age <- rast(age)
  dt[, age := age[][dt$pixelID]]
  openWoodland <- dt[age > 49,]$pixelID
  rm(dt)
  repvals <- rep(NA, times = ncell(age))
  repvals[openWoodland] <- 1
  openWoodland <- setValues(age, repvals)
  rm(repvals)
  outFile <- file.path("outputs", paste0("openWoodland_", dBaseYear,  "_focal", focalWindow, "_", tileNum, ".tif"))
  focalOut <- terra::focal(openWoodland, w = focalMatrix, sum, na.rm = TRUE, expand = FALSE,
                           filename = outFile, overwrite = TRUE)
  rm(openWoodland)
  gc()

  for (i in 1:3) gc() #terra really hangs on for some reason

}

if (runAnalysis) {
  #filter the years outside of function, I think...
  #here the year is irrelevant, unlike disturbance, because the rasters are snapshot in time.
  #but we should record the year for outputs.
  getYear <- function(pat, List) { return(List[grep(pat, List)])}
  percDecidList2020 <- getYear(2020, percDecidList)
  ageList2020 <- getYear(2020, ageList)
  canopyCoverList2020 <- getYear(2020, canopyCoverList)
  Map(OpenWoodlands, age = ageList2020, canopyCover = canopyCoverList2020, percDecid = percDecidList2020,
      MoreArgs = list(dBaseYear = 2020, focalWindow = focalRadius))
}

rm(OpenWoodlands)
