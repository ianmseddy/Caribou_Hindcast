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
options("reproducible.cachePath" = "cache")
setDTthreads(4)
runAnalysis <- TRUE #if TRUE, will remake the GIS layers
focalRadius <- 1000 #the radius to use for focal statistics, in metres
