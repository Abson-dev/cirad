######### ici je fais uniquement la modélisation de Faidherbia albida
library(SDMSelect)
library(dplyr)
Base_Espece<-Species #Species<-st_read(filename,quiet = T), voir le fichier data_preparation
#ou faire
#1) filename<-paste0("D:\\Stage_SDM\\SDM\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
#2)library(sf)
#3) Species<-st_read(filename,quiet = T)
Base_Espece$Faidherbia_albida<-if_else(Base_Espece$Species =="Faidherbia albida","1","0")
Base_Espece$Faidherbia_albida<-as.factor(Base_Espece$Faidherbia_albida)
#################################################
########???importation des données de worldclim
###1)prendre les fichiers .tif(raster) qui se trouvent dans le dossier indiquer
l1<-list.files("D:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\",patt="\\.tif")
l1<-sprintf("D:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\%s",l1)
###2) utiliser la fonction stack pour rendre RasterStack
worlClim<-stack(l1)
###3)connaitre l'étendue de notre zone d'étude (ext)
ext<-extent(Species)
###4)utiliser crop pour mettre a la même zone d'étude les données de worlclim
worldClim.crop<-crop(worlClim,ext)
plot(worldClim.crop$bio1)
library(sf)
#####supprimer la géométrie afin de pouvoir faire quelques manipulations
data_df<-st_drop_geometry(Base_Espece)
class(data_df)
##########???les espèces dans les différentes zones
Base_Espece_Zone1<-data_df %>%
  filter(Zone == 1)
Base_Espece_Zone2<-data_df %>%
  filter(Zone == 2)
Base_Espece_Zone4<-data_df %>%
  filter(Zone == 3)
Base_Espece_Zone4<-data_df %>%
  filter(Zone == 4)
###################Modélisation dans la zone 1, avec les variables de worldClim
Base_Faidherbia_Z1<-Base_Espece_Zone1 %>%
  select(xcoord,ycoord,Faidherbia_albida)
names(Base_Faidherbia_Z1)<-c("lon","lat","Faidherbia")
class(Base_Faidherbia_Z1)
FZ1<-Base_Faidherbia_Z1
view(FZ1)
#Transform data as SpatialPointDataFrame
sp::coordinates(FZ1) <-~lon+lat
sp::proj4string(FZ1) <-"+proj=longlat +datum=WGS84"
#extract covariables, combine with dataset 
dataFZ1<-CovarExtract(x=FZ1,cov.paths = l1) # en utilsisant SDMSelect
############## exporter dataFZ1 en .shp
library(maptools)
library(rgdal) 
tmpdir<-"C:\\Users\\Hp\\OneDrive\\redactions"
writeOGR(obj=dataFZ1,dsn=tmpdir,layer="dataFZ1",driver="ESRI Shapefile")
############### importer dataFZ1.shp
filename_PA_Z1<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataFZ1.shp")
PA_FZ1<-shapefile(filename_PA_Z1)
map1<-st_as_sf(PA_FZ1)
map1$Faidherbia<-if_else(map1$Faidherbia ==1,"présence","absence")
map1$Faidherbia<-as.factor(map1$Faidherbia)
# mf1<-map1
# mf1$Faidherbia<-if_else(mf1$Faidherbia =="présence","1","0")
# mf1$Faidherbia<-as.factor(mf1$Faidherbia)
# dataF1<-dataFZ1@data
# dataF1$Faidherbia<-as.factor(dataF1$Faidherbia)
# Base_Faidherbia_Z1<-Base_Espece_Zone1 %>%
#   select(xcoord,ycoord,Faidherbia_albida)
# names(Base_Faidherbia_Z1)<-c("lon","lat","Faidherbia")
# joint1<-Base_Faidherbia_Z1 %>%
#   select(lon,lat)
# rasterFZ1<-cbind(dataF1,joint1)
# library(viridis)
# p <- ggplot(rasterFZ1, aes(lon,lat)) +
#   geom_raster(data= rasterFZ1,aes(fill = bio1)) +
#   coord_equal() + scale_fill_viridis(name = "Estimation") +
#   geom_point( size=3,color = rgb(0.2,0.2,0.2,0.1))
#####chercher l'information sur la zone d'étude 1(zone 1)
zone_etude1<-shapefile("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\shpzones\\Zone_1_BON.shp")
z1<-st_as_sf(zone_etude1)
###########répresentation graphique des présence/absence de Faidherbia albida dans la zone 1
plotPAZ1<-ggplot(map1)   +
  geom_sf(aes(color = Faidherbia)) +
  geom_sf(data = z1, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Faidherbia Model\\PA_FaidherbiaZ1.png",plotPAZ1)
#############################################################

########Model Faidherbia albida

joint1<-Base_Faidherbia_Z1 %>%
  select(lon,lat)



#Enregistrements en double pour l'espèce
dups <- duplicated(Base_Faidherbia_Z1[, 1:3])
class(dups)
table(dups)
#???pas de doublon
#Vérification croisée

#sp::proj4string(zone_etude1) <-"+proj=longlat +datum=WGS84"














###################Model avec SDMSelect
#prepare the spatial dataset for modelling
data.prepared<-Prepare_dataset(x=data,var=1,cov = 2:ncol(data),datatype = "PA",na.rm = TRUE)
tmpdir<-"C:\\Users\\Hp\\OneDrive\\redactions\\SDMSelect"
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
raster::plot(worldClim.crop, "bio1")
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
covariates<-Prepare_covarStack(cov.paths = l1)
pred.r <- Map_predict(object = worlClim$bio2, saveWD = tmpdir, Num = Num.Best)

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
##############################end SDMSelect
