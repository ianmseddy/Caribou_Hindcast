#some wetland was created using age and crownclosure values that were greater than, instead of equal or greater than
#as such, a small amount was double-counted.
#there is also a massive amount of regenerating forest (not majority conifer)
ensureExclusiveClasses <- function(outputDir, tile, year){

  habitats <- list.files(outputDir, pattern = tile, full.names = TRUE) %>%
    .[grep(., pattern = year)]
  habitatRasters <- lapply(habitats, FUN = rast)
  names(habitatRasters) <- stringr::str_remove(basename(habitats),
                                               pattern = paste0(year, "_" ,tile, ".tif"))

  #first, ensure the three disturbance classes are exclusive. They should be
  tempFile <- tempfile2(fileext = ".tif") #avoid annoying warning with normalize tempfile
  disturbanceSum <- sum(habitatRasters$harvest_0to5_1985, habitatRasters$harvest_6to20_1985,
                        habitatRasters$naturalDisturbance1985, na.rm = TRUE)
  disturbanceSum <- writeRaster(disturbanceSum, tempFile)
  gc()
  vals <- minmax(disturbanceSum)

  rm(disturbanceSum)
  gc()
  if (vals[2] > 1) {
    stop("issue with disturbed classes")
  }
  disturbanceSum <- sum(habitatRasters$harvest_0to5_, habitatRasters$harvest_6to20_,
                        habitatRasters$naturalDisturbance, na.rm = TRUE)
  vals <- minmax(disturbanceSum)
  gc()
  if (vals[2] > 1) {
    stop("issue with disturbed classes")
  }
  #these three take precedence over any other rasters
  #it is probably fastest to simply re-write every raster then to manually check and track
  #the other 5 should be okay
  tempSum <- sum(habitatRasters$matureConifer, habitatRasters$openWoodland,
                 habitatRasters$youngConifer, habitatRasters$regeneratingStand_,
                 habitatRasters$wetland, na.rm = TRUE)
  if (minmax(tempSum)[2] > 1) {
    stop("shit") #but actually we can just inverse = TRUE it. Wetland takes precendence
    #the others should be mutually exclusive unless something is horrifically wrong
  }

  #matureConifer should supersede wetland due to a msitake
  #wetland should supersede regeneratingStand
  #you need some kind of temporary regeneratingStand for age < 20 that must be added, still..


  toFix <- c("matureConifer", "youngConifer", "wetland", "openWoodland", "regeneratingStand")


  lapply(toFix, fun = function(rasName){
    newRas <- mask(habitatRasters[[rasName]], mask = disturbanceSum, inverse = TRUE)
    writeRaster(newRas, filename = sources(habitatRasters[[rasName]]))
  })

}
