library(SDMSelect)
library(dplyr)
Base_Espece<-Species
Base_Espece$Faidherbia_albida<-if_else(Base_Espece$Species =="Faidherbia albida",1,0)
#Balanites aegyptiaca
Base_Espece$Balanites_aegyptiaca<-if_else(Base_Espece$Species =="Balanites aegyptiaca",1,0)
#Anogeissus leiocarpus
Base_Espece$Anogeissus_leiocarpus<-if_else(Base_Espece$Species =="Anogeissus leiocarpus",1,0)
#Adansonia digitata
Base_Espece$Adansonia_digitata<-if_else(Base_Espece$Species =="Adansonia digitata",1,0)
#Acacia nilotica
Base_Espece$Acacia_nilotica<-if_else(Base_Espece$Species =="Acacia nilotica",1,0)
##############
l1<-list.files("D:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\",patt="\\.tif")
l1<-sprintf("D:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\%s",l1)
worlClim<-stack(l1)
covar<-names(worlClim)
ext<-extent(Species)
worldClim.crop<-crop(worlClim,ext)

#data<-CovarExtract(x=Base_Espece,cov.paths = l1)
library(sf)
data_df<-st_drop_geometry(Base_Espece)
class(data_df)
# data_Faidherbia<-data_df %>%
#   filter(Faidherbia_albida==1)
# Faidherbia<-data_Faidherbia %>%
#   select(xcoord,ycoord)

# data.prepared<-Prepare_dataset(x=data, var = 1,cov =covar, datatype = "PA", na.rm = TRUE )
# plot(worldClim.crop,1)
# points(Faidherbia,col='blue')
#############zones
# data_Faidherbia_1<-data_Faidherbia %>%
#   filter(Zone==1)
# Faidherbia_1<-data_Faidherbia_1 %>%
#   select(xcoord,ycoord)
# plot(worldClim.crop,1)
# plot(zone_etude, add=TRUE)
# points(Faidherbia_1,col='blue')
#######################################
##########???les espèces dans les différentes zones
Base_Espece_Zone1<-data_sf %>%
  filter(Zone == 1)
Base_Espece_Zone2<-data_sf %>%
  filter(Zone == 2)
Base_Espece_Zone4<-data_sf %>%
  filter(Zone == 3)
Base_Espece_Zone4<-data_sf %>%
  filter(Zone == 4)
###################Modélisation dans la zone 1, avec les variables de worldClim
########Faidherbia albida
Base_Faidherbia_Z1<-Base_Espece_Zone1 %>%
  select(xcoord,ycoord,Faidherbia_albida)
names(Base_Faidherbia_Z1)<-c("lon","lat","Faidherbia")
#Transform data as SpatialPointDataFrame
sp::coordinates(Base_Faidherbia_Z1) <-~lon+lat
sp::proj4string(Base_Faidherbia_Z1) <-"+proj=longlat +datum=WGS84"
#extract covariables, combine with dataset 
data<-CovarExtract(x=Base_Faidherbia_Z1,cov.paths = l1)
names(data)
#show observations positions
par(mar=c(1,1,1,1))
raster::plot(worldClim.crop,"wc2.0_bio_30s_01")
sp::plot(zone_etude, add=T)
sp::plot(data,col=c("red","blue")[data@data$Faidherbia + 1],pch=20, cex=c(0.5,1)[data@data$Faidherbia + 1],add=T)
#prepare the spatial dataset for modelling
data.prepared<-Prepare_dataset(x=data,var=1,cov = 2:ncol(data),datatype = "PA",na.rm = TRUE)
tmpdir<-"C:\\Users\\Hp\\OneDrive\\redactions"
thd <- spatialcor_dist(
  x = data.prepared, longlat = !is.projected(data),
  binomial = TRUE, saveWD = tmpdir,
  plot = TRUE,
  figname = "Correlogram"
)
# Create a regular grid from dataset
RefRaster <- RefRasterize(x = data, res = round(thd[2]))
# Use rectangular grid to resample dataset
data.new <- Prepare_dataset(
  x = data.prepared, var = 1, cov = 2:ncol(data),
  RefRaster = RefRaster, datatype = "PA", na.rm = TRUE
)

# Plot data.new
par(mar = c(1,1,1,1))
raster::plot(worldClim.crop, "wc2.0_bio_30s_01")
sp::plot(zone_etude, add = TRUE)
sp::plot(data.new, 
         col = c("red", "blue")[data.new@data$dataY + 1],
         pch = 20, cex = c(0.5, 1)[data@data$Faidherbia + 1],
         add = TRUE)
#covariables corrélations
corSpearman <- Param_corr(
  x = data.new, rm = 1, thd = 0.7, visual = FALSE,
  plot = TRUE, img.size = 20, saveWD = tmpdir)

#Find the best combination of covariables
modelselect_opt(RESET = TRUE)
modelselect_opt$Max_nb_Var <- 3
modelselect_opt$datatype <- "PA"

res.file <- findBestModel(x = data.new, datatype = "PA", 
                          corSpearman = corSpearman, 
                          saveWD = tmpdir, 
                          verbose = 1)
#Order models according to quality of prediction
# Order models and find the bests
BestModels <- ModelOrder(saveWD = tmpdir, plot = TRUE)
#Predictions of the best model
Num.Best <- BestModels$VeryBestModels_crossV$Num[1]
res.file <- ModelResults(saveWD = tmpdir, plot = TRUE, 
                         Num = Num.Best)
#Species distribution mapping
#Probability of presence
pred.r <- Map_predict(object = covariates, saveWD = tmpdir, Num = Num.Best)

predr <- system.file("SDM_Selection", "predr.tif", package = "SDMSelect")
pred.r <- raster::stack(predr)
predrNames <- system.file("SDM_Selection", "predrNames.rds", package = "SDMSelect")
names(pred.r) <- readr::read_rds(predrNames)
# Get partial raster data to plot in ggplot
pred.r.gg <- gplot_data(pred.r)
# Plot
ggplot() +
  geom_tile(
    data = dplyr::filter(pred.r.gg, variable == "resp.fit"), 
    aes(x = x, y = y, fill = value)) +
  scale_fill_gradient("Probability", low = 'yellow', high = 'blue') +
  coord_equal()
#Uncertainties
rasterVis::gplot(raster::dropLayer(pred.r, which(!names(pred.r) %in% c("Q5", "Q95")))) +
  geom_tile(aes(fill = value)) +
  facet_grid(~variable) +
  scale_fill_gradient("Probability", low = 'yellow', high = 'blue') +
  coord_equal()

rasterVis::gplot(raster::dropLayer(pred.r, which(!names(pred.r) %in% c("IQR")))) +
  geom_tile(aes(fill = value)) +
  facet_grid(~variable) +
  scale_fill_gradient("Absolute\nDispersion", low = 'white', high = 'red') +
  coord_equal()

rasterVis::gplot(raster::dropLayer(pred.r, which(!names(pred.r) %in% c("IQR.M")))) +
  geom_tile(aes(fill = value)) +
  facet_grid(~variable) +
  scale_fill_gradient("Relative\nDispersion\nto median", low = 'white', high = 'red') +
  coord_equal()
#Separating presence from absences
model_selected <- model_select(
  saveWD = tmpdir,
  new.data = data.new,
  Num = Num.Best)
BestThd <- model_selected$Seuil

BestThdFile <- system.file("SDM_Selection", "BestThd.rds", package = "SDMSelect")
BestThd <- readr::read_rds(BestThdFile)

rasterVis::gplot(raster::raster(pred.r, "resp.fit")) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2("Probability\nof\nPresence",
                       low = 'white', mid = 'yellow',  high = 'blue',
                       midpoint = BestThd) +
  coord_equal()

rasterVis::gplot(raster::raster(pred.r, "ProbaSup")) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2("Probability\nto be over\nThreshold", 
                       low = 'white', mid = 'yellow', high = 'forestgreen',
                       midpoint = 50) +
  coord_equal()

#Mask
rasterVis::gplot(raster::dropLayer(pred.r, which(!grepl("mask", names(pred.r))))) +
  geom_tile(aes(fill = factor(value))) +
  facet_grid(~variable) +
  scale_fill_manual("mask", values = c("0" = "red", "1" = "forestgreen")) +
  coord_equal()
