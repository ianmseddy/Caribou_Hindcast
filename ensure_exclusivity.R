#some wetland was created using age and crownclosure values that were greater than, instead of equal or greater than
#as such, a small amount was double-counted.
#there is also a massive amount of regenerating forest (not majority conifer)
ensureExclusiveClasses <- function(inDir, tile, year, outDir){

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
  #this must be done sequentially
  habs$harvest_0to5_ <- checkClasses(habs$wetland, comp = habs$harvest_0to5_, msgClass = "harvest_05to5_")
  habs$harvest_6to20_ <- checkClasses(habs$wetland, comp = habs$harvest_6to20_, msgClass = "harvest_6to20_")
  habs$naturalDisturbance <- checkClasses(habs$wetland, comp = habs$naturalDisturbance, msgClass = "naturalDisturbance")

  #this saves time - and these should be mutually exclusive unless something horrible happened
  tempFile <- tempfile2(fileext = ".tif") #avoid annoying warning with normalize tempfile
  nextMask <- sum(habs$harvest_0to5_, habs$harvest_6to20_,
                  habs$naturalDisturbance, habs$wetland, na.rm = TRUE,
                  filename = tempFile)
  #as wetland is now distinct from disturbance, we can add all these and mask the others...

  vals <- minmax(nextMask)
  gc()
  if (vals[2] > 1) {
    stop("issue with disturbed classes")
    #this would only happen if the indexing was incorrect,
    #which should be caught by compareGeom at the beginning of every tiling function
  }
  habs$matureConifer <- checkClasses(nextMask, habs$matureConifer, msgClass = "matureConifer")
  habs$youngConifer <- checkClasses(nextMask, habs$youngConifer, msgClass = "youngConifer")
  habs$openWoodland <- checkClasses(nextMask, habs$openWoodland, msgClass = "openWoodland")
  habs$regeneratingStand <- checkClasses(nextMask, habs$regeneratingStand, msgClass = "regeneratingStand")

  #final test to make sure everything is kosher

  #TODO: check this
  lastCheck <- sum(habs$matureConifer, habs$youngConifer, habs$openWoodland, habs$regeneratingStand, na.rm = TRUE)
  if (minmax(lastCheck)[2] > 1) {
    stop("review this script!")
  }
  rm(lastCheck)
  gc()
  #don't forget to rewrite them!
  lapply(names(habs), FUN = function(name){
    checkPath(outDir, create = TRUE)
    newfile = paste0(outDir, "/", name, year, "_", tile, ".tif")
    writeRaster(habs[[name]], filename = newfile, overwrite = TRUE)
  })
  gc()
}

ensureExclusiveClasses("outputs/raw", tile = "tile1", year = 2020, outDir = "outputs/masked")
