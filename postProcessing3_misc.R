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


####masking out non habitat####
#some areas are not habitat but are within 1 km of habitat. This information is lost with this approach..

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


####transition matrix

transitionsByTile <- lapply(paste0("tile",1:c(nx*ny)), function(tile){

  comps <- list.files(path = "outputs/composite/", pattern = tile, full.names = TRUE)
  comps <- comps[grep(comps, pattern = "tif$")] #due to arcGIS, some .tif.aux files exist
  hab1985 <- rast(grep(comps, pattern = 1985, value = TRUE))
  hab2020 <- rast(grep(comps, pattern = 2020, value = TRUE))

  dt <- data.table(hab1985 = values(hab1985, mat = FALSE),
                   hab2020 = values(hab2020, mat = FALSE))
  dt <- dt[, .N, .(hab1985, hab2020)] #make into a maxium 9 x 9 table

  dt[, tile := tile]
  gc()
  return(dt)

})

transitionsByTile <- rbindlist(transitionsByTile)
transitionsByTile <- transitionsByTile[!c(is.na(hab1985) & is.na(hab2020))]
transitionsDT <- transitionsByTile[, .(N = sum(N)), .(hab1985, hab2020)]
#join this inside lapply next time
classLegend <- data.table(value = c(NaN, 1:8),
                          names = c("non-habitat",
                                    "natural disturbance", "harvest 0 to 5",
                                    "harvest 6 to 20", "conifer 50-70",
                                    "conifer 70+", "open woodland",
                                    "wetland", "regenerating forest"))
transitionsDT <- transitionsDT[classLegend, on = c("hab1985" = "value")]
transitionsDT[, hab1985 := NULL]
setnames(transitionsDT, old = "names", new = "hab 1985")
transitionsDT <- transitionsDT[classLegend, on = c("hab2020" = "value")]
transitionsDT[, hab2020 := NULL]
setnames(transitionsDT, old = "names", new = "hab 2020")
transitionsDT[order(N, decreasing = TRUE)]
transitionsDT[, N_km2 := round(N/1111.11111, digits = 0)]
transitions <- dcast(transitionsDT, formula = `hab 1985` ~ `hab 2020`, fill = 0, value.var = "N_km2")

#####Composite Habitat#####
#fire = 1, young/old harvest = 2 and 3, young/mature conifer = 4 and 5,
#open woodland 6, wetland 7, regenerating forest = 8
install.packages("circlize")
temp <- copy(transitionsDT)
setcolorder(temp, neworder = c("hab 1985", "hab 2020", "N_km2"))
circlize:chordDiagramFromDataFrame(temp)
