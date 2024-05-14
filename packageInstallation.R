#project set up

#first make a custom package library
pkgDir <- file.path(tools::R_user_dir("data"), "Caribou_Hindcast", "packages",
                    version$platform, substr(getRversion(), 1, 3))
#run this once
if (FALSE) {
  dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
  .libPaths(pkgDir, include.site = FALSE)
  message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))

  install.packages("Require")
  library(Require)
  Require("remotes", upgrade = FALSE)

  Require::setLibPaths()
  #at the moment this terra version throws obnoxious but harmless errors. Upgrading to source would solve the issue,
  #but the source version was unavailable when I began this project.
  Require::Require(packages = c("terra (>=1.6.7)", "sf", "data.table", "reproducible (>=1.2.10.9001)",
            "stringr", "raster", "googledrive", "magrittr", "parallel", "SpaDES.tools", "fasterize"),
          upgrade = FALSE)
}

.libPaths(pkgDir, include.site = FALSE)
library("terra")
library("sf")
library("data.table")
library("reproducible")
library("stringr")
library("googledrive") #uploading results
library("magrittr") #for piping, before |> came along
library("parallel") #for focal operations
library("SpaDES.tools") #for splitRaster into tiles
library("fasterize") #turning vectors into rasters

# packageVersion('terra') #1.6.7
# packageVersion('reproducible') 1.2.10.9001


#this is essential to ensuring GIS operations are identical across machines, so the tiles are the same
#updating terra resulted in a one-row difference in the projected raster, which meant everything had to be re-cropped
#so while the version does not have to be 1.6.7, it should be the same if run on multiple machines

