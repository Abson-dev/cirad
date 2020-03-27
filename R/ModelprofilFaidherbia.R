########"Modélisation de Faidherbia albida dans la zone 1
##zone 1
library(SDMSelect)
library(dplyr)
library(sf)
library(raster)
library(rasterVis)
library(RStoolbox)
library(maptools)
library(rgdal)
library(spdep)
library(ggplot2)
library(ggspatial)
library(randomForest)
#############"
zone_etude1<-shapefile("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\shpzones\\Zone_1_BON.shp")
z1<-st_as_sf(zone_etude1)
#########
filename<-paste0("D:\\Stage_SDM\\SDM\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Species<-st_read(filename,quiet = T)
Base_Espece<-Species
Base_Espece$Faidherbia_albida<-if_else(Base_Espece$Species =="Faidherbia albida","1","0")
Base_Espece$Faidherbia_albida<-as.factor(Base_Espece$Faidherbia_albida)
#####supprimer la géométrie afin de pouvoir faire quelques manipulations
data_df<-st_drop_geometry(Base_Espece)
##########???les espèces dans les différentes zones
Base_Espece_Zone1<-data_df %>%
  filter(Zone == 1)
Base_Faidherbia_Z1<-Base_Espece_Zone1 %>%
  select(xcoord,ycoord,Faidherbia_albida)
names(Base_Faidherbia_Z1)<-c("lon","lat","Faidherbia")
FZ1<-Base_Faidherbia_Z1
#Transform data as SpatialPointDataFrame
sp::coordinates(FZ1) <-~lon+lat
sp::proj4string(FZ1) <-"+proj=longlat +datum=WGS84"
########???importation des données de worldclim
###1)prendre les fichiers .tif(raster) qui se trouvent dans le dossier indiquer
l1<-list.files("D:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\",patt="\\.tif")
l1<-sprintf("D:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\%s",l1)
dataFZ1<-CovarExtract(x=FZ1,cov.paths = l1) # en utilsisant SDMSelect
DataModelFZ1<-dataFZ1@data
View(DataModelFZ1)
table(DataModelFZ1$Faidherbia)



############## exporter dataFZ1 en .shp
tmpdir<-"C:\\Users\\Hp\\Desktop\\Model"
##zone 1
writeOGR(obj=dataFZ1,dsn=tmpdir,layer="dataFZ1",driver="ESRI Shapefile")
##zone 1
filename_PA_Z1<-paste0("C:\\Users\\Hp\\Desktop\\Model","\\dataFZ1.shp")
PA_FZ1<-shapefile(filename_PA_Z1)
map1<-st_as_sf(PA_FZ1)
map1$Faidherbia<-if_else(map1$Faidherbia ==1,"présence","absence")
map1$Faidherbia<-as.factor(map1$Faidherbia)

###########répresentation graphique des présence/absence de Faidherbia albida dans les zones
plotPAZ1<-ggplot(map1)   +
  geom_sf(aes(color = Faidherbia)) +
  geom_sf(data = z1, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

#Extraire un seul RasterLayer d'un RasterBrick (ou RasterStack).
  #r <- raster(b, layer=2)
###)connaitre l'étendue de notre zone d'étude (ext)
ext<-raster::extent(Species)
# class      : Extent 
# xmin       : -16.53864 
# xmax       : -16.35454 
# ymin       : 14.45461 
# ymax       : 14.63543 
worlClim<-stack(l1)
###)utiliser crop pour mettre a la même zone d'étude les données de worlclim
worldClim.crop<-crop(worlClim,ext)
names(worldClim.crop)
# [1] "bio1"  "bio10" "bio11" "bio12" "bio13" "bio14" "bio15" "bio16"
# [9] "bio17" "bio18" "bio19" "bio2"  "bio3"  "bio4"  "bio5"  "bio6" 
# [17] "bio7"  "bio8"  "bio9"
bio1 <- raster(worldClim.crop, layer=1)
levelplot(bio1,contour=F)
# bounds <- list("sp.polygons", z1)
# spplot(bio1, sp.layout=bounds)
bio10 <- raster(worldClim.crop, layer=2)
levelplot(bio10,contour=F)
# r <- raster(worldClim.crop, layer=1)
# values(r) <- 1:ncell(r)
# r <- mask(r, z1)
# plot(r)
# plot(z1, add=TRUE, lwd=1) 


ggR(bio1, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio1", colours = terrain.colors(100))  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)  +
  geom_sf(data = z1, colour = "blue", fill = NA)
#en tenant compte du mask
# ggR(r, geom_raster = TRUE,ggLayer = F) +
#   scale_fill_gradientn(name = "bio1", colours = terrain.colors(100))  +
#   theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
#   annotation_north_arrow(location = "bl", which_north = "true",
#                          pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
#                          style = north_arrow_fancy_orienteering)  +
#   geom_sf(data = z1, colour = "blue", fill = NA)
 
##################Modélisation
#Examinons d'abord un modèle d'arbres de classification et de régression (CART).
# library(rpart)
# cart <- rpart(Faidherbia~., data=DataModelFZ1)
# printcp(cart)
# plotcp(cart)
# plot(cart, uniform=TRUE, main="Regression Tree")
# # text(cart, use.n=TRUE, all=TRUE, cex=.8)
# text(cart, cex=.8, digits=1)

fpa <- as.factor(DataModelFZ1[, 'Faidherbia'])
crf <- randomForest(DataModelFZ1[, 2:ncol(DataModelFZ1)], fpa)
crf
class(crf)
plot(crf)
varImpPlot(crf)
#bio4,bio2,bio3,bio18,bio15,bio10,bio11,bio1
######################constitution des données d'entrainement et test
Faidherbia_pres<-Base_Faidherbia_Z1 %>%
  filter(Faidherbia==1)
Faidherbia_pres<-Faidherbia_pres[,1:2]
presvals<-extract(predictors,Faidherbia_pres)
Faidherbia_abs<-Base_Faidherbia_Z %>%
  filter(Faidherbia==0)
Faidherbia_abs<-Faidherbia_abs[,1:2]
corela<-corrr::correlate(DataModelFZ1[,2:20])

set.seed(0)
group<-kfold(Faidherbia_pres,5)
Faidherbia_pres_train<-Faidherbia_pres[group!=1,]
dim(Faidherbia_pres_train)
Faidherbia_pres_test<-Faidherbia_pres[group==1,]
dim(Faidherbia_pres_test)

ext<-extent(-16.53864,-16.35454,14.45461,14.63543)
# class      : Extent 
# xmin       : -16.53864 
# xmax       : -16.35454 
# ymin       : 14.45461 
# ymax       : 14.63543 
set.seed(0)
pred_nf<-predictors

backg<-Faidherbia_abs
colnames(backg)=c("lon","lat")
group<-kfold(backg,5)
backg_train<-backg[group!=1,]
backg_test<-backg[group==1,]

#######*logistic regression
gm1<-glm(Faidherbia ~bio4 + bio2 + bio9 + bio18 + bio15 + bio10 + bio11 + bio1 + bio16 + bio8, family = binomial(link = "logit"),data = DataModelFZ1)
xtabs(gm1)
summary(gm1)
###################
maxent()
