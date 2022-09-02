#open woodland is forest that is older than 20, not mature conifer, and less than 30% cover

#remove age-zero

canopyClosure85List <- list.files(path = "GIS/tiles", pattern = "att_closure_S_1985", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
canopyClosure2020List <- list.files(path = "GIS/tiles", pattern = "att_closure_S_2020", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)

plot(rast(canopyClosure85List[1]))

tempList <- list.files(path = "GIS/tiles", pattern = "age", full.names = TRUE) %>%
  grep(., pattern = ".grd", value = TRUE)
tempList[[1]]
plot(rast(tempList[[1]]))

#are we double-counting

