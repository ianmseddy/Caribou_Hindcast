Wetland <- function(age, landPos, lcc, canopyCover, percDecid,
                    youngConifer, matureConifer, dBaseYear) {

  tileNum <- stringr::str_extract(age, pattern = "tile[0-9]+")

  landPos <- rast(landPos)
  #sanity check
  canopyCover1 <- rast(canopyCover)
  percDecid1 <- rast(percDecid)
  age1 <- rast(age)
  lcc1 <- rast(lcc)
  compareGeom(landPos, canopyCover1, percDecid1) #evidently you can't compare >3 rasters...
  compareGeom(landPos, age1, lcc1)
  rm(canopyCover1, percDecid1, age1, lcc1)

  # focalMatrix <- terra::focalMat(x = landPos, d = focalWindow, type = "circle")

  #create dt for indexing
  dt <- data.table(pixelID = 1:ncell(landPos))

  dt$landPos <- landPos[]
  dt <- dt[landPos == 5]
  rm(landPos)
  dt[, landPos := NULL]
  gc()

  #mature and young Conifer
  matureConifer <- rast(matureConifer)
  youngConifer <- rast(youngConifer)
  dt[, matCon := matureConifer[pixelID]]
  dt[, youngCon := youngConifer[pixelID]]
  dt <- dt[is.na(matCon) & is.na(youngCon)]
  dt[, c("matCon", "youngCon") := NULL]
  rm(matureConifer, youngConifer)


  #lcc
  lcc <- rast(lcc)
  dt[, lcc := lcc[][dt$pixelID]]
  dt <- dt[lcc != 8] #remove open water wetland
  dt[, lcc := NULL]
  rm(lcc)
  gc()


  #include majority conifer that is open woodland (age 50+, cc <30) and all 20+ deciduous
  #exclude majority conifer that is younger than 50 or c(age 50+ and cc >30)
  #this will include disturbed forest and regenerating forest and must be masked later
  #since wetland may or may not have an associated age...
  percDecid <- rast(percDecid)
  dt[, percDecid := percDecid[][dt$pixelID]]
  rm(percDecid)

  age <- rast(age)
  dt[, age := age[][dt$pixelID]]
  rm(age)

  cc <- rast(canopyCover)
  dt[, cc := cc[][dt$pixelID]]

  dt <- dt[!c(percDecid <= 75 & age >= 50 & cc >= 30)] #this preserves mature coniferous
  wetland <- dt$pixelID
  rm(dt)
  gc()

  repvals <- rep(NA, times = ncell(cc))
  repvals[wetland] <- 1
  wetland <- setValues(cc, repvals)
  rm(repvals, cc)
  gc()

  #write the binary output
  outFile <- file.path("outputs/raw", paste0("wetland", dBaseYear,"_", tileNum, ".tif"))
  writeRaster(wetland, filename = outFile, datatype = "INT1U", overwrite = TRUE)

  # outFile <- file.path("outputs", paste0("wetland_", dBaseYear,  "_focal", focalWindow, "_", tileNum, ".tif"))
  # focalOut <- terra::focal(wetland, w = focalMatrix, sum, na.rm = TRUE, expand = FALSE,
  #                          filename = outFile, overwrite = TRUE)
  rm(wetland)
  gc()

  for (i in 1:3) gc() #terra really hangs on for some reason

}

if (runAnalysis) {
  #2020 first

  landPosList2020 <- getAtt(Att = "land_pos", Year = 2020)
  ageList2020 <- getAtt(Att = "age", 2020)
  canopyCoverList2020 <- getAtt("att_closure", 2020)
  percDecidList2020 <- getAtt("prc", 2020)
  lccList2020 <- getAtt("VegType", 2020)
  youngConList2020 <- getAtt("youngCon", 2020, Path = "outputs/raw")
  MatureConList2020 <- getAtt("matureCon", 2020, Path = "outputs/raw")
  Map(Wetland, age = ageList2020, landPos = landPosList2020, lcc = lccList2020,
      matureConifer = MatureConList2020, youngConifer = youngConList2020,
      percDecid = percDecidList2020, canopyCover = canopyCoverList2020,
      MoreArgs = list(dBaseYear = 2020))


  #1985
  landPosList1985 <- getAtt(Att = "land_pos", Year = 2020)
  ageList1985 <- getAtt(Att = "age", 1985)
  canopyCoverList1985 <- getAtt("att_closure", 1985)
  percDecidList1985 <- getAtt("prc", 1985)
  lccList1985 <- getAtt("VegType", 1985)
  youngConList1985 <- getAtt("youngCon", 1985, Path = "outputs/raw")
  MatureConList1985 <- getAtt("matureCon", 1985, Path = "outputs/raw")
  Map(Wetland, age = ageList1985, landPos = landPosList1985, lcc = lccList1985,
      matureConifer = MatureConList1985, youngConifer = youngConList1985,
      percDecid = percDecidList1985, canopyCover = canopyCoverList1985,
      MoreArgs = list(dBaseYear = 1985))

}

rm(Wetland)

