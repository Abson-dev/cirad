########"Modélisation de Faidherbia albida dans la zone 1
##zone 1

#########
filename<-paste0("F:\\Stage_SDM\\SDM\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Species<-st_read(filename,quiet = T)
Base_Espece<-Species
Base_Espece$Faidherbia_albida<-if_else(Base_Espece$Species =="Faidherbia albida","1","0")
Base_Espece$Faidherbia_albida<-as.factor(Base_Espece$Faidherbia_albida)
#####supprimer la géométrie afin de pouvoir faire quelques manipulations
data_df<-st_drop_geometry(Base_Espece)
##########???les espèces dans les différentes zones
Base_Espece_Zone2<-data_df %>%
  filter(Zone == 2)
Base_Faidherbia_Z2<-Base_Espece_Zone2[,c("xcoord","ycoord","Faidherbia_albida")] 
names(Base_Faidherbia_Z2)<-c("lon","lat","Faidherbia")
FZ2<-Base_Faidherbia_Z2
#Transform data as SpatialPointDataFrame
sp::coordinates(FZ2) <-~lon+lat
sp::proj4string(FZ2) <-"+proj=longlat +datum=WGS84"
########???importation des données de worldclim
###1)prendre les fichiers .tif(raster) qui se trouvent dans le dossier indiquer
l1<-list.files("F:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\",patt="\\.tif")
l1<-sprintf("F:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\%s",l1)
dataFZ2<-CovarExtract(x=FZ2,cov.paths = l1) # en utilsisant SDMSelect
DataModelFZ2<-dataFZ2@data
View(DataModelFZ2)
table(DataModelFZ2$Faidherbia)



############## exporter dataFZ2 en .shp
tmpdir<-"C:\\Users\\Hp\\Desktop\\Model"
##zone 1
writeOGR(obj=dataFZ2,dsn=tmpdir,layer="dataFZ2",driver="ESRI Shapefile")
##zone 1
filename_PA_Z2<-paste0("C:\\Users\\Hp\\Desktop\\Model","\\dataFZ2.shp")
PA_FZ2<-shapefile(filename_PA_Z2)
map2<-st_as_sf(PA_FZ2)
map2$Faidherbia<-if_else(map2$Faidherbia ==1,"présence","absence")
map2$Faidherbia<-as.factor(map2$Faidherbia)

###########répresentation graphique des présence/absence de Faidherbia albida dans les zones
plotPAZ2<-ggplot(map2)   +
  geom_sf(aes(color = Faidherbia)) +
  geom_sf(data = z2, colour = "black", fill = NA)  +
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
writeRaster(bio1, filename=file.path(tmpdir, "bio1.tif"), format="GTiff", overwrite=TRUE)
##
bio10 <- raster(worldClim.crop, layer=2)
writeRaster(bio10, filename=file.path(tmpdir, "bio10.tif"), format="GTiff", overwrite=TRUE)
bio11 <- raster(worldClim.crop, layer=3)
writeRaster(bio11, filename=file.path(tmpdir, "bio11.tif"), format="GTiff", overwrite=TRUE)
bio12 <- raster(worldClim.crop, layer=4)
writeRaster(bio12, filename=file.path(tmpdir, "bio12.tif"), format="GTiff", overwrite=TRUE)
bio13 <- raster(worldClim.crop, layer=5)
writeRaster(bio13, filename=file.path(tmpdir, "bio13.tif"), format="GTiff", overwrite=TRUE)
bio14 <- raster(worldClim.crop, layer=6)
writeRaster(bio14, filename=file.path(tmpdir, "bio14.tif"), format="GTiff", overwrite=TRUE)
bio15 <- raster(worldClim.crop, layer=7)
writeRaster(bio15, filename=file.path(tmpdir, "bio15.tif"), format="GTiff", overwrite=TRUE)
bio16 <- raster(worldClim.crop, layer=8)
writeRaster(bio16, filename=file.path(tmpdir, "bio16.tif"), format="GTiff", overwrite=TRUE)
bio17 <- raster(worldClim.crop, layer=9)
writeRaster(bio17, filename=file.path(tmpdir, "bio17.tif"), format="GTiff", overwrite=TRUE)
bio18 <- raster(worldClim.crop, layer=10)
writeRaster(bio18, filename=file.path(tmpdir, "bio18.tif"), format="GTiff", overwrite=TRUE)
bio19 <- raster(worldClim.crop, layer=11)
writeRaster(bio19, filename=file.path(tmpdir, "bio19.tif"), format="GTiff", overwrite=TRUE)
bio2 <- raster(worldClim.crop, layer=12)
writeRaster(bio2, filename=file.path(tmpdir, "bio2.tif"), format="GTiff", overwrite=TRUE)
bio3 <- raster(worldClim.crop, layer=13)
writeRaster(bio3, filename=file.path(tmpdir, "bio3.tif"), format="GTiff", overwrite=TRUE)
bio4 <- raster(worldClim.crop, layer=14)
writeRaster(bio4, filename=file.path(tmpdir, "bio4.tif"), format="GTiff", overwrite=TRUE)
bio5 <- raster(worldClim.crop, layer=15)
writeRaster(bio5, filename=file.path(tmpdir, "bio5.tif"), format="GTiff", overwrite=TRUE)
bio6 <- raster(worldClim.crop, layer=16)
writeRaster(bio6, filename=file.path(tmpdir, "bio6.tif"), format="GTiff", overwrite=TRUE)
bio7 <- raster(worldClim.crop, layer=17)
writeRaster(bio7, filename=file.path(tmpdir, "bio7.tif"), format="GTiff", overwrite=TRUE)
bio8 <- raster(worldClim.crop, layer=18)
writeRaster(bio8, filename=file.path(tmpdir, "bio8.tif"), format="GTiff", overwrite=TRUE)
bio9 <- raster(worldClim.crop, layer=19)
writeRaster(bio9, filename=file.path(tmpdir, "bio9.tif"), format="GTiff", overwrite=TRUE)
levelplot(output,contour=F)
raster<-bio1  
buffer<-z2 #yellow buffer
# SpatialPolygons(zone_etude2)
output<-mask(raster, buffer)
plot(output)
# bounds <- list("sp.polygons", z1)
# spplot(bio1, sp.layout=bounds)
bio10 <- raster(worldClim.crop, layer=2)
levelplot(bio10,contour=F)
# r <- raster(worldClim.crop, layer=1)
# values(r) <- 1:ncell(r)
# r <- mask(r, z1)
plot(output)
plot(z2$geometry, add=TRUE, lwd=1) 


ggR(bio1, geom_raster = TRUE,ggLayer = F) +
  scale_fill_gradientn(name = "bio1", colours = terrain.colors(100))  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)  +
  geom_sf(data = z2, colour = "blue", fill = NA)
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
# cart <- rpart(Faidherbia~., data=DataModelFZ2)
# printcp(cart)
# plotcp(cart)
# plot(cart, uniform=TRUE, main="Regression Tree")
# # text(cart, use.n=TRUE, all=TRUE, cex=.8)
# text(cart, cex=.8, digits=1)

fpa <- as.factor(DataModelFZ2[, 'Faidherbia'])
crf <- randomForest(DataModelFZ2[, 2:ncol(DataModelFZ2)], fpa)
crf
class(crf)
plot(crf)
varImpPlot(crf)
#bio4,bio2,bio3,bio18,bio15,bio10,bio11,bio1
######################constitution des données d'entrainement et test
Faidherbia_pres<-Base_Faidherbia_Z2 %>%
  filter(Faidherbia==1)
Faidherbia_pres<-Faidherbia_pres[,1:2]
presvals<-extract(worldClim.crop,Faidherbia_pres)
Faidherbia_abs<-Base_Faidherbia_Z2 %>%
  filter(Faidherbia==0)
Faidherbia_abs<-Faidherbia_abs[,1:2]
absvals<-extract(worldClim.crop,Faidherbia_abs)
#corela<-corrr::correlate(DataModelFZ2[,2:20])
######################
# load package data
awt <- bio1
# import presence-absence species data
PA <- Base_Faidherbia_Z2
# make a sf object from data.frame
pa_data <- sf::st_as_sf(PA, coords = c("lon", "lat"), crs = raster::crs(awt))
# spatial blocking by specified range and random assignment
sb1 <- spatialBlock(speciesData = pa_data,
                    species = "Faidherbia",
                    blocks =z2,
                    theRange = 700,
                    k = 5,
                    selection = "random",
                    iteration = 100,
                    numLimit = NULL,
                    biomod2Format = TRUE,
                    xOffset = 0.3, # shift the blocks horizontally
                    yOffset = 0)
foldExplorer(sb1, awt, pa_data)
#############""
sb2 <- spatialBlock(speciesData = pa_data,
                    species = "Faidherbia",
                    rasterLayer = awt,
                    rows = 5,
                    cols = 8,
                    k = 5,
                    selection = "systematic",
                    biomod2Format = TRUE)
##########################"

if(interactive()){
  # load package data
  awt <- bio1
  # import presence-absence species data
  PA <- Base_Faidherbia_Z2
  # make a sf object from data.frame
  pa_data <- sf::st_as_sf(PA, coords = c("lon", "lat"), crs = raster::crs(awt))
  rangeExplorer(rasterLayer = awt) # the only mandatory input
  # add species data to add them on the map
  rangeExplorer(rasterLayer = awt,
                speciesData = pa_data,
                species = "Faidherbia",
                rangeTable = NULL,
                minRange = 30000, # limit the search domain
                maxRange = 100000)
}
##########
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
gm1<-glm(Faidherbia ~bio4 + bio2 + bio9 + bio18 + bio15 + bio10 + bio11 + bio1 + bio16 + bio8, family = binomial(link = "logit"),data = DataModelFZ2)
xtabs(gm1)
summary(gm1)
###################
maxent()
###############################Modélisation dans la zone 2