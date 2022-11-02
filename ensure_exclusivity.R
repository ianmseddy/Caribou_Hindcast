#some wetland was created using age and crownclosure values that were greater than, instead of equal or greater than
#as such, a small amount was double-counted.
#there is also a massive amount of regenerating forest (not majority conifer)
ensureExclusiveClasses <- function(inDir, tile, year, outDir){
  browser()
  habitats <- list.files(inDir, pattern = tile, full.names = TRUE) %>%
    .[grep(., pattern = year)]
  habs <- lapply(habitats, FUN = rast)
  names(habs) <- stringr::str_remove(basename(habitats),
                                     pattern = paste0(year, "_" ,tile, ".tif"))

  #first, we need to add the missingRegen class if it exists (it doesn't for 1985)

  if (!is.null(habs$missingRegen)) {
    habs$regeneratingStand <- sum(habs$regeneratingStand,
                                   habs$missingRegen, na.rm = TRUE)
    habs[habs > 1] <- 1
    habs$missingRegen <- NULL
    gc()
    #if there are any twos, they should be 0 - though this shouldnt' happen...
  }

  #per discussion with Yan, disturbed wetland is wetland
  #wetland takes precedence over every other class
  #followed by the mapped disturbance layers
  #some pixels are in multilple classes as disturbance year and age are not always aligned

  checkClasses <- function(priorityClass, comp, msgClass) {
    sumRas <- sum(priorityClass, comp, na.rm = TRUE) #make sure this isn't summing every cell
    if (minMax(sumRas[2] > 1)) { #ie the max is greater than 2
      vals <- sumRas[]
      message(length(vals[vals > 1]), " misclassified pixels in ", msgClass)
      comp <- mask(comp, mask = priorityClass, inverse = TRUE)
      rm(vals)
    }
    rm(sumRas)
    gc()
    return(comp)
  }
  #this must be done sequentially
  habs$harvest_0to5_ <- checkClasses(habs$wetland, comp = habs$harvest_0to5_)
  habs$harvest_6to20_ <- checkClasses(habs$wetland, comp = habs$harvest_6to20_)
  habs$naturalDisturbance <- checkClasses(habs$wetland, comp = habs$naturalDisturbance)

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
  habs$na.rm <- TRUE
  #TODO: check this
  test <- do.call(terra::sum, habs)
  if (minmax(test)[2] > 1) {
    stop("review this script!")
  }
  habs$na.rm <- NULL
  rm(test)
  gc()

  #don't forget to rewrite them!
  lapply(names(habs), FUN = function(name, habs)){
    checkPath(outDir, create = TRUE)
    filename = paste0(outDir, "/", name, year, "_", tile, ".tif")
    writeRaster(habs[[name]], )
  }
  gc()
}

ensureExclusiveClasses("outputs/raw", tile = "tile1", year = 2020, outdir = "outputs/masked")
