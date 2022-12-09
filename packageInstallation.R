#project set up
pkgDir <- file.path("packages",version$platform, getRversion()[, 1:2])
dir.create(pkgDir, recursive = TRUE, showWarnings = FALSE)
.libPaths(pkgDir, include.site = FALSE)
message("Using libPaths:\n", paste(.libPaths(), collapse = "\n"))


#while require is down
if (!require("Require")) install.packages("Require")

Require("checkpoint", upgrade = FALSE)
Require("remotes", upgrade = FALSE)
# checkpoint("2021-01-01", r_version = "4.2.0", checkpoint_location = packageDir)

#at the moment this terra version throws obnoxious but harmless errors. Upgrading to source would solve the issue,
#but the source version was unavailable when I began this project.
Require::Require(packages = c("terra (>=1.6.7)", "sf", "data.table", "reproducible (>=1.2.10.9001)",
          "stringr", "raster", "googledrive", "magrittr", "parallel", "SpaDES.tools", "fasterize"),
        upgrade = FALSE)
# packageVersion('terra') #1.6.7
# packageVersion('reproducible') 1.2.10.9001
# packageVersion("raster") #3.5.15

#this is essential to ensuring GIS operations are identical across machines, so the tiles are the same
#updating terra resulted in a one-row difference in the projected raster, which meant everything had to be re-cropped
