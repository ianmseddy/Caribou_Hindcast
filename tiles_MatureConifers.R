#identify the age 70 conifer stands with canopy cover > 30%
canopyCoverList <- list.files(path = "GIS/tiles", pattern = "att_closure", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
ageList <- list.files(path = "GIS/tiles", pattern = "att_age", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
percDecidList <- list.files(path = "GIS/tiles", pattern = "prcD", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)

MatureConifer <- function(age, canopyCover, percDecid, focalWindow, dBaseYear) {

  tileNum <- stringr::str_extract(age, pattern = "tile[0-9]+")

  #create focal matrix for use later
  percDecid <- rast(percDecid)
  focalMatrix <- terra::focalMat(x = percDecid, d = focalWindow, type = "circle")

  dt <- data.table(pixelID = 1:ncell(percDecid))
  dt$percDecid <- percDecid[]

  #keep only pixels that are 25% or more coniferous
  dt <- dt[percDecid < 75,]
  dt[, percDecid := NULL]
  rm(percDecid)
  gc()
  #keep only pixels with 30% or more cover
  canopyCover <- rast(canopyCover)
  dt[, cover := canopyCover[][dt$pixelID]]
  #keeping index outside of values (i.e. instead of canopyCover[<index>]) is significantly faster!
  dt <- dt[cover >= 30,]
  dt[, cover := NULL]
  rm(canopyCover)
  gc()

  #write young conifer
  age <- rast(age)
  dt[, age := age[][dt$pixelID]]
  youngConifer <- dt[age > 49 & age < 70]$pixelID
  oldConifer <- dt[age >= 70]$pixelID
  rm(dt)
  repvals <- rep(NA, times = ncell(age))
  repvals[youngConifer] <- 1
  youngConifer <- setValues(age, repvals)
  rm(repvals)
  outFile <- file.path("outputs/raw", paste0("youngConifer", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(youngConifer, filename = outFile, datatype = "INT1U", overwrite = TRUE)
  outFile <- file.path("outputs", paste0("youngConifer_", dBaseYear,  "_focal", focalWindow, "_", tileNum, ".tif"))
  focalOut <- terra::focal(youngConifer, w = focalMatrix, sum, na.rm = TRUE, expand = FALSE,
                           filename = outFile, overwrite = TRUE)
  rm(youngConifer)
  gc()

  #write old conifer
  repvals <- rep(NA, times = ncell(age))
  repvals[oldConifer] <- 1
  oldConifer <- setValues(age, repvals)
  rm(repvals)
  outFile <- file.path("outputs/raw", paste0("matureConifer", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(oldConifer, filename = outFile, datatype = "INT1U")
  outFile <- file.path("outputs", paste0("matureConifer_", dBaseYear,  "_focal", focalWindow, "_", tileNum, ".tif"))
  focalOut <- terra::focal(oldConifer, w = focalMatrix, sum, na.rm = TRUE, expand = FALSE,
                           filename = outFile, overwrite = TRUE)
  rm(oldConifer)
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
  Map(MatureConifer, age = ageList2020, canopyCover = canopyCoverList2020, percDecid = percDecidList2020,
      MoreArgs = list(dBaseYear = 2020, focalWindow = focalRadius))

  percDecidList1985 <- getYear(1985, percDecidList)
  ageList1985 <- getYear(1985, ageList)
  canopyCoverList1985 <- getYear(1985, canopyCoverList)
  Map(MatureConifer, age = ageList1985, canopyCover = canopyCoverList1985, percDecid = percDecidList1985,
      MoreArgs = list(dBaseYear = 1985, focalWindow = focalRadius))


}

rm(MatureConifer)
