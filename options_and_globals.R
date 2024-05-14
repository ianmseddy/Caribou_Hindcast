#tiling options

nx = 4 #referring to tile columns
ny = 2# referring to tile rows

#silly utility functions
getYear <- function(pat, List) { return(List[grep(pat, List)])}
getAtt <- function(Att, Year, Path = "GIS/tiles") {
  out <- (list.files(path = Path, pattern = Att, full.names = TRUE)) %>%
    getYear(Year, .)
  return(out)
}

reproducible::checkPath("cache", create = TRUE)
options("reproducible.cachePath" = "cache") #creates a cache folder
setDTthreads(4) #
runAnalysis <- TRUE #if TRUE, will remake the GIS layers
focalRadius <- 1000 #the radius to use for focal statistics, in metres
ncores <- round(parallel::detectCores()/4) #don't use every available core
#the only part of the project that is parallelized is running focal operations over
#the individual habitat layers of each tile (n = 8 habitats * ny * nx)

#I uploaded/downloaded files to my googledrive using this folder location
gDrivePath <- "PFC/Yan/Caribou Hindcast Results V2"
