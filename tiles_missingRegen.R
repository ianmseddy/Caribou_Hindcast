#this script is now unnecessary as the under20 pixels with no disturbance history
# are considered windthrow or other natural disturbances, and be classified under natural disturbance
# some are definitely roads, however. We need to find a road dataset with time series, anyway.


#this is to identify the forest pixels under 20 y.a. that were not disturbed from fire or harvest
# and are consequently missing from the other regenerating forest group
#this isn't possible for 1985 but I've left in a base year argument in case we map some intermediate year
#Because some non-forest is classified as age 0, we need to know landcover

# ageList <- list.files(path = "GIS/tiles", pattern = "att_age", full.names = TRUE) %>%
#   grep(., pattern = ".grd", value = TRUE)
# lccList <- list.files(path = "GIS/tiles", pattern = "VegType", full.names = TRUE) %>%
#   grep(., pattern = ".grd", value = TRUE)
# dYearList <- list.files(path = "GIS/tiles", pattern = "1985_2020_YRT2", full.names = TRUE) %>%
#   grep(., pattern = ".grd", value = TRUE)
#
#
# missingRegen <- function(age, lcc, dYear, dBaseYear = 2020) {
#
#   tileNum <- stringr::str_extract(age, pattern = "tile[0-9]+")
#
#   lcc <- rast(lcc)
#
#   #sanity check
#   age <- rast(age)
#   dYear <- rast(dYear)
#   compareGeom(age, dYear, lcc)
#
#
#   #create dt for indexing
#   dt <- data.table(pixelID = 1:ncell(lcc))
#   dt[, lcc := lcc[]]
#   dt <- dt[lcc %in% c(5,6,7),]
#   dt[, lcc := NULL]
#   gc()
#
#   dt[, age := age[][dt$pixelID]]
#   dt <- dt[age <= 20,] #remove open water wetland
#   rm(lcc, age)
#   gc()
#
#   #take them only if disturbance year is outside of the previous 20 or NA
#
#   dt[, dist := dYear[][dt$pixelID]]
#   dt <- dt[dist < dBaseYear - 20 | is.na(dist)]
#   gc()
#
#   dt <- dt$pixelID
#   gc()
#
#   repvals <- rep(NA, times = ncell(dYear))
#   repvals[dt] <- 1
#   missingRegen <- setValues(dYear, repvals)
#   rm(dYear, repvals)
#   gc()
#
#   #write the binary output
#   outFile <- file.path("outputs/raw", paste0("missingRegen", dBaseYear,"_", tileNum, ".tif"))
#   writeRaster(missingRegen, filename = outFile, datatype = "INT1U", overwrite = TRUE)
#
#   rm(missingRegen)
#
#   for (i in 1:3) gc() #terra really hangs on for some reason
#
# }
#
# if (runAnalysis) {
#
#   lccList2020 <- getYear(2020, lccList)
#   ageList2020 <- getYear(2020, ageList)
#   Map(missingRegen, age = ageList2020, lcc = lccList2020,
#     dYear = dYearList,  MoreArgs = list(dBaseYear = 2020))
#
#   #this class doesn't exist for the 1985 dataset as we assume all of these are burn or harvest
#
# }
#
# rm(Wetland)
#
