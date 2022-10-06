#project set up
packageDir <- "packages"
if (!dir.exists(packageDir)) dir.create(packageDir)

.libPaths(packageDir)

if (!require("Require")) install.packages("Require")
Require("checkpoint")
Require("remotes")
checkpoint("2021-01-01", r_version = "4.2.0", checkpoint_location = packageDir)

Require(c("terra", "sf", "data.table", "PredictiveEcology/reproducible (>= 1.2.10.9001)",
          "stringr", "raster", "googledrive", "SpaDES.tools"))

