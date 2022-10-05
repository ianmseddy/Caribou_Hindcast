#project set up
packageDir <- "packages"
if (!exists(packageDir)) dir.create(packageDir)

.libPaths(packageDir)
if (!require("Require")) install.packages("Require")
Require("checkpoint")
Require("remo")
checkpoint("2021-01-01", r_version = ..., checkpoint_location = packageDir)

library(Require)
Require(c("terra", "sf", "data.table", "PredictiveEcology/reproducible",
          "stringr", "raster", "googledrive", "SpaDES.tools"))
remotes::install_github("PredictiveEcology/reproducible@development")
