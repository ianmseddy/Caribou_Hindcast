#####uploading####
if (FALSE){

  toZip <- list.files("outputs/weightedDifference", full.names = TRUE)
  utils::zip(zipfile = "outputs/changeInWeightedHabitat.zip",
             files = toZip,
             flags = "-j")
  theFolder <- googledrive::drive_mkdir("PFC/Yan/Caribou Hindcast Results V2/change in weighted habtitat")
  thePath <- googledrive::as_dribble(theFolder)
  drive_put("outputs/changeInWeightedHabitat.zip", path = thePath)

  toZip <- list.files("outputs/masks", full.names = TRUE)
  utils::zip(zipfile = "outputs/nonHabitat_Masks.zip",
             files = toZip,
             flags = "-j")
  thePath <- googledrive::drive_mkdir("PFC/Yan/Caribou Hindcast Results V2/nonHabitat masks")
  thePath <- as_dribble(thePath)
  drive_put("outputs/nonHabitat_Masks.zip", path = thePath)
}




####downloading####

#moving files off google drive
results <- drive_ls(as_id("https://drive.google.com/drive/folders/1bqXed21z922H1cQv_1jTh8RN0BdyK0OY"),
                    recursive = TRUE)

FileID <- results[results$name == "weightedHabitat.zip",]$id
googledrive::drive_download(file = FileID,
                            path = "outputs/weightedHabitat.zip")
utils::unzip(zipfile = "outputs/weightedHabitat.zip", exdir = "outputs/weightedHabitat")

FileID <- results[results$name == "nonHabitat_Masks.zip",]$id
googledrive::drive_download(file = FileID,
                            path = "outputs/nonHabitat_Masks.zip")
utils::unzip(zipfile = "outputs/nonHabitat_masks.zip", exdir = "outputs/masks")

FileID <- results[results$name == "changeInWeightedHabitat.zip",]$id
googledrive::drive_download(file = FileID,
                            path = "outputs/changeInWeightedHabitat.zip")
utils::unzip(zipfile = "outputs/changeInWeightedHabitat.zip", exdir = "outputs/changeInWeightedHabitat")

FileID <- results[results$name == "focalHabitat1000.zip",]$id
googledrive::drive_download(file = FileID,
                            path = "outputs/focalHabitat1000.zip")
utils::unzip(zipfile = "outputs/focalHabitat1000.zip", exdir = "outputs/focalHabitat1000")



