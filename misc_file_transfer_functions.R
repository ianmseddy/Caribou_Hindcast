#####uploading####
library(googledrive)
gDrivePath <- "PFC/Yan/Caribou Hindcast Results V2"

checkGPath <- function(gdriveFolder){
  checkPath <- drive_get(gdriveFolder)
  if (length(checkPath$name) == 0){
    thePath <- googledrive::drive_mkdir(gdriveFolder)
  }
  thePath <- as_dribble(gdriveFolder)
}

if (FALSE){

  toZip <- list.files("outputs/raw", full.names = TRUE)
  utils::zip(zipfile = "outputs/raw.zip",
             files = toZip,
             flags = "-j")
  rawPath <- as_dribble(file.path(gDrivePath, "raw habitat layers"))
  drive_put("outputs/raw.zip", path = rawPath)

  toZip <- list.files("outputs/weightedHabitat", full.names = TRUE)
  utils::zip(zipfile = "outputs/weightedHabitat.zip",
             files = toZip,
             flags = "-j")
  weightedPath <- as_dribble(file.path(gDrivePath, "weighted habitat"))
  drive_put("outputs/weightedHabitat.zip", path = weightedPath)

  toZip <- list.files("outputs/compositeHabitat", full.names = TRUE)
  utils::zip(zipfile = "outputs/compositeHabitat.zip",
             files = toZip,
             flags = "-j")
  compositePath <- as_dribble(file.path(gDrivePath, "composite habitat"))
  drive_put("outputs/compositeHabitat.zip", path = compositePath)

  toZip <- list.files("outputs/weightedDifference", full.names = TRUE)
  utils::zip(zipfile = "outputs/weightedDifference.zip",
             files = toZip,
             flags = "-j")
  weightedChangePath <- as_dribble(file.path(gDrivePath, "change in weighted habitat"))
  drive_put("outputs/weightedDifference.zip", path = weightedChangePath)

}




####downloading####

#moving files off google drive
results <- drive_ls(as_id("https://drive.google.com/drive/folders/1bqXed21z922H1cQv_1jTh8RN0BdyK0OY"),
                    recursive = TRUE)

FileID <- results[results$name == "weightedHabitat.zip",]$id
googledrive::drive_download(file = FileID,
                            path = "outputs/weightedHabitat.zip", overwrite = TRUE)
utils::unzip(zipfile = "outputs/weightedHabitat.zip", exdir = "outputs/weightedHabitat", overwrite = TRUE)


FileID <- results[results$name == "compositeHabitat.zip",]$id
googledrive::drive_download(file = FileID,
                            path = "outputs/compositeHabitat.zip", overwrite = TRUE)
utils::unzip(zipfile = "outputs/compositeHabitat.zip", exdir = "outputs/compositeHabitat", overwrite = TRUE)

FileID <- results[results$name == "nonHabitat_Masks.zip",]$id
googledrive::drive_download(file = FileID,
                            path = "outputs/nonHabitat_Masks.zip")
utils::unzip(zipfile = "outputs/nonHabitat_masks.zip", exdir = "outputs/masks")

FileID <- results[results$name == "weightedDifference.zip",]$id
googledrive::drive_download(file = FileID,
                            path = "outputs/weightedDifference.zip",
                            overwrite = TRUE)
utils::unzip(zipfile = "outputs/weightedDifference.zip",
             exdir = "outputs/weightedDifference",
             overwrite = TRUE)

FileID <- results[results$name == "focalHabitat1000.zip",]$id
googledrive::drive_download(file = FileID,
                            path = "outputs/focalHabitat1000.zip")
utils::unzip(zipfile = "outputs/focalHabitat1000.zip", exdir = "outputs/focalHabitat1000")

out <- googledrive::drive_get(path = gDrivePath)
out$name
