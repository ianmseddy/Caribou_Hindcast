#master script -  packageInstallation and options_and_globals must be run everytime
#the former for loading, the latter for variables like number of tiles
source("packageInstallation.R")
#only need data.table, terra, SpaDES.tools (for a buffered split raster),
#sf and fasterize (for rasterizing managed forest), magrittr (for piping, for now),
#and parallel (for parallelizing focal operations)

#determines number of tiles, buffering distance, etc
source("options_and_globals.R")

#create some folders for output
checkPath("outputs/raw", create = TRUE)
checkPath("GIS/tiles", create = TRUE)

# source("preProcessing_inputData.R") #this only needs to be run once - no sense loading these objects
#this will retrieve CanLaD and CanFIR data from googledrive links
#this is better run with supervision as it may fill temp drives during reprojecting, tiling, etc



#Leblond formula
# section 4.1 of Leblond et al 2014
# 0.25 * mature conifer  > 70 yo  --- uses CaNFIR_att_age,
# 0.19 * young mature conifer forests 50-70 years old
# 0.14 * wetlands
# 0.22 * open lichen woodlands (age 50+ conifers with < 30% cover)
# 0.06 * natural disturbances < 20
# 0.04 * cutblocks 5 yo or less
# 0.04 * cutblocks 6-20 - same weight as 5yo but s.d. is different
# 0.06 * regenerating stands > 20 years post disturbance (but that aren't mature conifers or wetlands)

#mature conifer should be mapped first, then wetland, then disturbances, then the rest
#wetland will overide any disturbance
#during the generation of disturbance tiles


# In order
#1)	Identify the two mature conifer classes, age 50-70 and 70+, with canopy closure > 25%
source("tiles_MatureConifers.R")

#2)	Identify wetlands
#per Mathieu, wetland was Alnus spp, open (ie non forest) or flooded. Alnus is an 'unproductive' land cover class
#therefore, 'mature conifer' that falls under wetland would still be mature conifer.
#Our wetland will be non-forest wetland, deciduous 20+, and age 50+ conifer with cover <30%


#TODO: #logged wetland may be better described as logged - wetland is very broad category - seeking input from Mathieu
#it was decided to ignore these
source("tiles_Wetland.R")

#3)	Identify the natural disturbances < 20 y.o
source("tiles_NaturalDisturbanceStands.R")

#note that we are inevitably overestimating harvest and underestimating fire
#as the NFDB only dates to 1973
#TODO: ignore wetland pixels in 1985 during this script, as in tiles_NaturalDisturbances
source("tiles_MappingDisturbances_1985.R")

#4)	Identify the two age classes of regenerating cutblocks (0-5 and 6-20) -
source("tiles_HarvestedStands.R")

#5)	Identify the Open Lichen Woodlands: conifers age 50+ with canopy closure < 25% that aren't wetlands
#this assumse that ages and disturbances are consistent
source("tiles_OpenWoodlands.R")

#6)	Identify Regenerating Stands: any stands that aren't one of the above classes.
#this class includes majority-conifer pixels that are 21-49 years of age, and deciduous stands age 20+ that aren't wetland.
#many age 20+ stands are also disturbed < 20 y.a, as the two datasets don't need to agree.
#TODO: read the disturbances tiles in, and mask the regenerating stand here. Saves running masking script
source("tiles_RegeneratingStands.R")



covariates2020 <- c("matureConifer", "youngConifer", "openWoodland", "regenerating",
                    "wetland", "harvest_0to5", "harvest_6to20", "naturalDisturbance")
#don't use lapply as we don't want 7 rasters

#setValues as an intermediate step causes the raster to be in memory, blowing up RAM use to > 100 GB
#in hindsight I should have multiplied the focal values before writing them to disk
# lapply(covariates2020, FUN = function(x){
#   output <- list.files(pattern = x, path = "outputs",full.names = TRUE) %>%
#     grep(., pattern = "2020", value = TRUE) %>%
#     lapply(., FUN = function(x) {raster(x) * 1000}) %>%
#     #the above line created 400 GB of rasters in my tempdrive.... lol
#     mergeRaster(.)
#   gc()
#   raster::writeRaster(x = output, filename = paste0("outputs/final/", x, 2020, "_focal.tif"),
#                       datatype = "INT2U", overwrite = TRUE)
#   #we can delete these newTiles later -
#   rm(output)
#   gc()
# })



####plotting ### turns out we had some GIS errors as one machine used terra 1.6
#this caused the cutline row and column to be included, so we have 2 extra cols/rows
# output <- list.files(path = "outputs/final", pattern = ".tif$", full.names = TRUE) %>%
#  lapply(., rast)
# sourceNames <- sapply(output, sources)
# covariateNames <- names(sort(sapply(covariates2020, FUN = grep, x = sourceNames)))
# names(output) <- covariateNames
# newMat <- crop(output$matureConifer, output$wetland)
# writeRaster(newMat, filename = "outputs/final/matureConifer2020_focal.tif", overwrite = TRUE)
# newYoung <- crop(output$youngConifer, output$wetland) #will have to rewrite these..
# writeRaster(newYoung, "outputs/final/youngConifer2020_focal.tif", overwrite = TRUE)

