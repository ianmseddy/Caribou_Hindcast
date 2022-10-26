#project set up
packageDir <- "packages"
if (!dir.exists(packageDir)) dir.create(packageDir)

.libPaths(packageDir)

if (!require("Require")) install.packages("Require")
Require("checkpoint", upgrade = FALSE)
Require("remotes", upgrade = FALSE)
# checkpoint("2021-01-01", r_version = "4.2.0", checkpoint_location = packageDir)

Require(c("terra (>=1.6.7)", "sf", "data.table", "reproducible (1.2.10.9001)",
          "stringr", "raster", "googledrive", "magrittr", "SpaDES.tools"), upgrade = FALSE)
# packageVersion('terra') #1.6.7
# packageVersion('reproducible') 1.2.10.9001
# packageVersion("raster") #3.5.15

#this is essential to ensuring GIS operations are identical across machines
