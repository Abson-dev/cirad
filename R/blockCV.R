# loading the package
library(blockCV)

# import presence-absence species data
PA <- read.csv(system.file("extdata", "PA.csv", package = "blockCV"))
# coordinate reference system
Zone55s <- "+proj=utm +zone=55 +south +ellps=GRS80 +units=m +no_defs"
# make a sf object from data.frame
pa_data <- sf::st_as_sf(PA, coords = c("x", "y"), crs = Zone55s)
# buffering with presence-absence data
bf1 <- buffering(speciesData= pa_data,
                 species= "Species",
                 theRange= 70000,
                 spDataType = "PA",
                 progress = TRUE)

# import presence-background species data
PB <- read.csv(system.file("extdata", "PB.csv", package = "blockCV"))
# make a sf object from data.frame
pb_data <- sf::st_as_sf(PB, coords = c("x", "y"), crs = Zone55s)
# buffering with presence-background data
bf2 <- buffering(speciesData= pb_data,
                 species= "Species",
                 theRange= 70000,
                 spDataType = "PB",
                 addBG = TRUE, # add background data to testing folds
                 progress = TRUE)
# buffering with no species attribute
bf3 <- buffering(speciesData = pa_data,
                 theRange = 70000)


# load package data
awt <- raster::brick(system.file("extdata", "awt.grd", package = "blockCV"))
# import presence-absence species data
PA <- read.csv(system.file("extdata", "PA.csv", package = "blockCV"))
# make a sf object from data.frame
pa_data <- sf::st_as_sf(PA, coords = c("x", "y"), crs = raster::crs(awt))
# environmental clustering
eb <- envBlock(rasterLayer = awt,
               speciesData = pa_data,
               species = "Species", # name of the column with response
               k = 5,
               standardization = "standard",
               rasterBlock = TRUE)


if(interactive()){
  # load package data
  awt <- raster::brick(system.file("extdata", "awt.grd", package = "blockCV"))
  # import presence-absence species data
  PA <- read.csv(system.file("extdata", "PA.csv", package = "blockCV"))
  # make a sf object from data.frame
  pa_data <- sf::st_as_sf(PA, coords = c("x", "y"), crs = raster::crs(awt))
  # spatial blocking by specified range and random assignment
  sb <- spatialBlock(speciesData = pa_data,
                     species = "Species",
                     rasterLayer = awt,
                     theRange = 70000,
                     k = 5,
                     selection = "random",
                     iteration = 100)
  foldExplorer(sb, awt, pa_data)
  # buffering with presence-absence data
  bf <- buffering(speciesData= pa_data,
                  species= "Species", # to count the number of presences and absences
                  theRange= 70000,
                  spDataType = "PA",
                  progress = TRUE)
  foldExplorer(bf, awt, pa_data)
  # environmental clustering
  eb <- envBlock(rasterLayer = awt,
                 speciesData = pa_data,
                 species = "Species",
                 k = 5)
  foldExplorer(eb, awt, pa_data)
}
if(interactive()){
  # load package data
  awt <- raster::brick(system.file("extdata", "awt.grd", package = "blockCV"))
  # import presence-absence species data
  PA <- read.csv(system.file("extdata", "PA.csv", package = "blockCV"))
  # make a sf object from data.frame
  pa_data <- sf::st_as_sf(PA, coords = c("x", "y"), crs = raster::crs(awt))
  rangeExplorer(rasterLayer = awt) # the only mandatory input
  # add species data to add them on the map
  rangeExplorer(rasterLayer = awt,
                speciesData = pa_data,
                species = "Species",
                rangeTable = NULL,
                minRange = 30000, # limit the search domain
                maxRange = 100000)
}


# load the example raster data
awt <- raster::brick(system.file("extdata", "awt.grd", package = "blockCV"))
plot(awt)
# run the model in parallel
range1 <- spatialAutoRange(rasterLayer = awt,
                           sampleNumber = 5000, # number of cells to be used
                           doParallel = TRUE,
                           nCores = 2, # if NULL, it uses half of the CPU cores
                           plotVariograms = FALSE,
                           showPlots = TRUE)
# run the model with no parallel
range3 <- spatialAutoRange(rasterLayer = awt,
                           sampleNumber = 5000,
                           doParallel = FALSE,
                           progress = TRUE)
# show the result
summary(range1)
##
# load package data
library(sf)
awt <- raster::brick(system.file("extdata", "awt.grd", package = "blockCV"))
# import presence-absence species data
PA <- read.csv(system.file("extdata", "PA.csv", package = "blockCV"))
# make a sf object from data.frame
pa_data <- sf::st_as_sf(PA, coords = c("x", "y"), crs = raster::crs(awt))
# spatial blocking by specified range and random assignment
sb1 <- spatialBlock(speciesData = pa_data,
                    species = "Species",
                    theRange = 70000,
                    k = 5,
                    selection = "random",
                    iteration = 100,
                    numLimit = NULL,
                    biomod2Format = TRUE,
                    xOffset = 0.3, # shift the blocks horizontally
                    yOffset = 0)
# spatial blocking by row/column and systematic fold assignment
sb2 <- spatialBlock(speciesData = pa_data,
                    species = "Species",
                    rasterLayer = awt,
                    rows = 5,
                    cols = 8,
                    k = 5,
                    selection = "systematic",
                    biomod2Format = TRUE)
















# spatial blocking by specified range and random assignment
sb <- spatialBlock(speciesData = pa_data, # sf or SpatialPoints
                   species = "Species", # the response column (binomial or multi-class)
                   rasterLayer = myrasters, # a raster for backgoround (optional)
                   theRange = 70000, # size of the blocks
                   k = 5, # the number of folds
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   biomod2Format = TRUE)

# investigate spatial autocorrelation in raster covariates
# this helps to choose a suitable size for spatial blocks
spatialAutoRange(rasterLayer = myrasters, # raster file
                 sampleNumber = 5000, # number of cells to be used
                 doParallel = TRUE,
                 showPlots = TRUE)

# alternatively, you can manually choose the size of spatial blocks 
rangeExplorer(rasterLayer = myrasters,
              speciesData = pa_data, # response data (optional)
              species = "Species" # the responcse column (optional)
              minRange = 30000, # limit the search domain
              maxRange = 100000)