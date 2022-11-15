#some wetland was created using age and crownclosure values that were greater than, instead of equal or greater than
#as such, a small amount was double-counted.This was subsequently corrected
#there is also a massive amount of regenerating forest (not majority conifer) that is not aligned with disturbance year
#This script could be vastly faster, but I wanted to track specific numbers
#so far this age/regen overlap appears to be ~5-15% per tile. Other classes are around 0-3%
#finally, Yan decided that burned and harvested wetland is still wetland, as opposed to logged/burned
#this is pretty rare, around 0.05-0.1% of these classes.
ensureExclusiveClasses <- function(inDir, tile, year, outDir){

  checkPath(outDir, create = TRUE)

  habitats <- list.files(inDir, pattern = tile, full.names = TRUE) %>%
    .[grep(., pattern = year)]
  habs <- lapply(habitats, FUN = rast)
  names(habs) <- stringr::str_remove(basename(habitats),
                                     pattern = paste0(year, "_" ,tile, ".tif"))

  #first, we need to add the missingRegen class if it exists (it doesn't for 1985)

  if (!is.null(habs$missingRegen)) {
    checkPath(outDir, create = TRUE)
    newfile = paste0(outDir, "/", "regeneratingStand", year, "_", tile, ".tif")
    #write to disk as it needs to be written anyway, and this removes the object from RAM
    habs$regeneratingStand <- sum(habs$regeneratingStand,
                                  habs$missingRegen, na.rm = TRUE,
                                  filename = newfile,
                                  overwrite = TRUE)
    if (any(na.omit(values(habs$regeneratingStand)) > 1)) {
      stop("review missingRegen and regeneratingStand")
      #this shouldn't be possible given one is under 20 y.a, the other is over 20 y.a
    }
    habs$missingRegen <- NULL
    gc()
    #if there are any twos, they should be 0 - though this shouldnt' happen...
  }

  #per discussion with Yan, disturbed wetland is wetland
  #wetland takes precedence over every other class
  #followed by the mapped disturbance layers
  #some pixels are in multilple classes as disturbance year and age are not always aligned

  #this is possibly unnecesary but tracking the number of pix seemed relevant
  checkClasses <- function(priorityClass, comp, msgClass) {

    sumRas <- sum(priorityClass, comp, na.rm = TRUE) #make sure this isn't summing every cell
    if (minmax(sumRas)[2] > 1) { #ie the max is greater than 2
      vals <- na.omit(sumRas[])
      badPercent <- length(vals[vals > 1])/length(vals) * 100
      message(badPercent, " % of ", msgClass, " is misclassified")
      comp <- mask(comp, mask = priorityClass, inverse = TRUE)
      rm(vals)
    }
    rm(sumRas)
    gc()
    return(comp)
  }

  newFileNames <- paste0(outDir, "/", names(habs), year, "_", tile, ".tif")
  names(newFileNames) <- names(habs)
  #to replace the list object with something on disk - trying to fix a memory leak (~40 GB/tile)

  #this is just to return the object after writing it
  writeToDisk <- function(filename, Rast){
    writeRaster(Rast, filename, overwrite = TRUE)
    rm(Rast)
    return(rast(filename))
  }


  #this must be done sequentially
  habs$harvest_0to5_ <- checkClasses(habs$wetland, comp = habs$harvest_0to5_, msgClass = "harvest_0to5_")
  habs$harvest_0to5_ <- writeToDisk(newFileNames["harvest_0to5_"], habs$harvest_0to5_)
  gc()
  habs$harvest_6to20_ <- checkClasses(habs$wetland, comp = habs$harvest_6to20_, msgClass = "harvest_6to20_")
  habs$harvest_6to20_ <- writeToDisk(newFileNames["harvest_6to20_"], habs$harvest_6to20_)
  gc()
  habs$naturalDisturbance <- checkClasses(habs$wetland, comp = habs$naturalDisturbance, msgClass = "naturalDisturbance")
  habs$naturalDisturbance <- writeToDisk(newFileNames["naturalDisturbance"], habs$naturalDisturbance)

  nextMask <- sum(habs$harvest_0to5_, habs$harvest_6to20_,
                  habs$naturalDisturbance, habs$wetland, na.rm = TRUE)
  #as wetland is now distinct from disturbance, we can add all these and mask the others...
  vals <- minmax(nextMask)
  gc()
  if (vals[2] > 1) {
    stop("disturbed classes were not mutually exclusive...")
    #this would only happen if the indexing was incorrect,
    #which should be caught by compareGeom at the beginning of every tiling function
  }

  #have to write 1 at a time to avoid all 4 in memory
  habs$matureConifer <- checkClasses(nextMask, habs$matureConifer, msgClass = "matureConifer")
  habs$matureConifer <- writeToDisk(newFileNames["matureConifer"], habs$matureConifer)
  gc()
  habs$youngConifer <- checkClasses(nextMask, habs$youngConifer, msgClass = "youngConifer")
  habs$youngConifer <- writeToDisk(newFileNames["youngConifer"], habs$youngConifer)
  gc()
  habs$openWoodland <- checkClasses(nextMask, habs$openWoodland, msgClass = "openWoodland")
  habs$openWoodland <- writeToDisk(newFileNames["openWoodland"], habs$openWoodland)
  gc()
  habs$regeneratingStand <- checkClasses(nextMask, habs$regeneratingStand, msgClass = "regeneratingStand")
  habs$regeneratingStand <- writeToDisk(newFileNames["regeneratingStand"], habs$regeneratingStand)

  rm(nextMask)
  gc()
  #final test to make sure everything is kosher

  lastCheck <- sum(habs$matureConifer, habs$youngConifer, habs$openWoodland, habs$regeneratingStand, na.rm = TRUE)
  if (minmax(lastCheck)[2] > 1) {
    stop("review this script!")
  }
  rm(lastCheck)
  gc()
  return(NULL)
}

#make output director on SSD
tiles <- paste0("tile", 1:6)
#this does the extra step of merging the 2020 "young but not disturbed" class
lapply(tiles, ensureExclusiveClasses, inDir = "outputs/raw", year = 1985, outDir = "D:/Ian/YanBoulanger/maskedHabitat")
lapply(tiles, ensureExclusiveClasses, inDir = "outputs/raw", year = 2020, outDir = "D:/Ian/YanBoulanger/maskedHabitat")
