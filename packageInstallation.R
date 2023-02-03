#project set up
pkgDir <- file.path("packages",version$platform, getRversion()[, 1:2])
if (FALSE) {
  dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
  .libPaths(pkgDir, include.site = FALSE)
  message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))

  #while require is down
  # if (!require("Require")) install.packages("Require")
  unloadNamespace("reproducible")
  library(Require)

  # Require("checkpoint", upgrade = FALSE) #checkpoint is deprecated in a few months
  Require("remotes", upgrade = FALSE)
  # checkpoint("2021-01-01", r_version = "4.2.0", checkpoint_location = packageDir)

  #at the moment this terra version throws obnoxious but harmless errors. Upgrading to source would solve the issue,
  #but the source version was unavailable when I began this project.
  # Require::Require(packages = c("terra (>=1.6.7)", "sf", "data.table", "reproducible (>=1.2.10.9001)",
  #           "stringr", "raster", "googledrive", "magrittr", "parallel", "SpaDES.tools", "fasterize"),
  #         upgrade = FALSE)
}

library("terra")
library("sf")
library("data.table")
library("reproducible")
library("stringr")
library("raster")
library("googledrive")
library("magrittr")
library("parallel")
library("SpaDES.tools")
library("fasterize")

# packageVersion('terra') #1.6.7
# packageVersion('reproducible') 1.2.10.9001


#this is essential to ensuring GIS operations are identical across machines, so the tiles are the same
#updating terra resulted in a one-row difference in the projected raster, which meant everything had to be re-cropped
