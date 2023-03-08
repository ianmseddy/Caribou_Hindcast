checkPath("outputs/weightedDifference", create = TRUE) #for the difference between weighted habitat
checkPath("outputs/masks", create = TRUE) #to mask out irrelevant pixels like water, urban, grassland

habitatRasters <- list.files(path = "outputs/weightedHabitat", full.names = TRUE)
lapply(paste0("tile", 1:c(nx*ny)), function(tile){
  habs <- list.files(path = "outputs/weightedHabitat", full.names = TRUE, pattern = tile)
  hab1985 <- rast(grep(habs, pattern = "1985", value =TRUE))
  hab2020 <- rast(grep(habs, pattern = "2020", value = TRUE))

  terra::diff(c(hab1985, hab2020), lag = 1,
              filename = paste0("outputs/weightedDifference/weightedDiff_", tile, ".tif"),
              wopt = list(datatype = "INT2S"))
})

if (FALSE){
  # #the composite tiles are 8 GB each, so upload tiles separately)
  toZip <- list.files("outputs/weightedDifference", full.names = TRUE)
  utils::zip(zipfile = "outputs/changeInWeightedHabitat.zip",
             files = toZip,
             flags = "-j")
  thePath <- googledrive::as_dribble("PFC/Yan/Caribou Hindcast Results V2/change in weighted habtitat")
  drive_put("outputs/changeInWeightedHabitat.zip", path = thePath)
}



#make a not forest or wetland mask
lapply(paste0("tile",1:c(nx*ny)), function(tile){
  data <- list.files(path = "GIS/tiles", pattern = tile, full.names = TRUE)
  lcc2020 <- rast(grep("VegTypeClass_S_2020", data, value = TRUE))
  lcc1985 <- rast(grep("VegTypeClass_S_1985", data, value = TRUE))
  landPos <- rast(grep("land_pos", data, value = TRUE))

  # Vegetation type: bryoid (1), herbs(2) rock(3) shrub (4),
  # Treed Broadleaf (5) Treed Conifer (6) Treed Mixed (7) Water (8)
  #non-forest becomes 1
  lccRecl <- matrix(c(1, 4, 1, 5, 7, 0, 8, 8, 1),
                    ncol = 3, byrow = TRUE)
  #non-wetland becomes 1
  landPosRecl <- matrix(c(1, 4, 1, 5, 5, 0), ncol = 3, byrow = TRUE)
  #too much in tmp drive

  mask1 <- classify(lcc2020, lccRecl, right = NA)
  mask2 <- classify(lcc1985, lccRecl, right = NA)
  mask3 <- classify(landPos, landPosRecl, right = NA)
  #if pixels do not sum to 1, they are either forested in 1985 or 2020 (or both), or wetland
  out <- sum(mask1, mask2, mask3, na.rm = TRUE)
  gc()

  rm(mask1, mask2, mask3)
  #final mask
  maskMat <- matrix(c(0, 2, NA, 3, 3, 1), ncol = 3, byrow = TRUE)

  classify(out, maskMat,right = NA,
           filename = file.path("outputs/masks/",
                                paste0("mask_", tile, ".tif")),
           overwrite = TRUE)
  terra::tmpFiles(current = TRUE, orphan = TRUE, old = TRUE, remove = TRUE)
})

  #negative masks. Take pixels that sum to 3. blot those out in the original raster
if (FALSE){
  # #the composite tiles are 8 GB each, so upload tiles separately)
  toZip <- list.files("outputs/masks", full.names = TRUE)
  utils::zip(zipfile = "outputs/focalHabitat1000.zip",
             files = toZip,
             flags = "-j")
  thePath <- googledrive::as_dribble("PFC/Yan/Caribou Hindcast Results V2/focal habitat layers X 1000")
  drive_put("outputs/focalHabitat1000.zip", path = thePath)
}

