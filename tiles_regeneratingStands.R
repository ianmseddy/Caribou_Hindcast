#5)	Identify Regenerating Stands: any stands > 20 years that aren’t conifer 50+ (ie none of the above classes)
ageList <- list.files(path = "GIS/tiles", pattern = "att_age", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
percDecidList <- list.files(path = "GIS/tiles", pattern = "prcD", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
posList <- list.files(path = "GIS/tiles", pattern = "pos", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
canopyCoverList <- list.files(path = "GIS/tiles", pattern = "att_closure", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
RegeneratingStands <- function(age, percDecid, focalWindow, pos, canopyCover, dBaseYear) {

  tileNum <- stringr::str_extract(age, pattern = "tile[0-9]+")

  #create focal matrix for use later
  age <- rast(age)
  focalMatrix <- terra::focalMat(x = age, d = focalWindow, type = "circle")

  #create dt for indexing
  dt <- data.table(pixelID = 1:ncell(age))

  dt$age <- age[]
  dt <- dt[age > 20]
  rm(age)
  gc()

  #pixels that are 50% or more confierous will fall into woodland, mature conifer
  #so keep all deciduous or young coniferous
  percDecid <- rast(percDecid)
  dt[, percDecid := percDecid[][dt$pixelID]]

  pos <- rast(pos)
  dt[, pos := pos[][dt$pixelID]]

  #dedicudous wetland is wetland, regardless of age and canopy cover
  dt <- dt[c(percDecid > 50 & pos != 5)]

  rm(pos)
  dt[, pos := NULL]

  canopyCover <- rast(canopyCover)
  dt[, cc := canopyCover[][dt$pixelID]]
  #assume age 50+ with cover below 30 is either a woodland or wetland and will not regenerate to forest
  #this excludes deciduous woodlands (along with e.g. grassland)
  dt <- dt[!c(age > 50 & cc < 30)]

  regeneratingStand <- dt[percDecid > 50 | c(percDecid <= 50 & age < 50),]$pixelID
  rm(dt)
  repvals <- rep(NA, times = ncell(percDecid))
  repvals[regeneratingStand] <- 1
  regeneratingStand <- setValues(percDecid, repvals)
  rm(repvals, percDecid)
  gc()

  outFile <- file.path("outputs/raw", paste0("regeneratingStand_", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(regeneratingStand, filename = outFile, datatype = "INT1U")

  outFile <- file.path("outputs", paste0("regeneratingStand_", dBaseYear,  "_focal", focalWindow, "_", tileNum, ".tif"))
  focalOut <- terra::focal(regeneratingStand, w = focalMatrix, sum, na.rm = TRUE, expand = FALSE,
                           filename = outFile, overwrite = TRUE)
  rm(regeneratingStand)
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
  posList2020 <- getYear(2020, posList)
  canopyCoverList2020 <- getYear(2020, canopyCoverList)
  Map(RegeneratingStands, age = ageList2020, percDecid = percDecidList2020,
      canopyCover = canopyCoverList2020, pos = posList2020,
      MoreArgs = list(dBaseYear = 2020, focalWindow = focalRadius))
}

rm(RegeneratingStands)
