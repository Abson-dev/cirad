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
plot(worldClim.crop$bio6)
library(rasterVis)
library(RStoolbox)
ggR(worldClim.crop, 1, geom_raster=TRUE, stretch = "hist",hue=0.5) +
  scale_fill_gradientn(colours = terrain.colors(100), name = "elevation") +
  theme(axis.text = element_text(size=5), 
        axis.text.y = element_text(angle=90),
        axis.title=element_blank())
library(sf)
#####supprimer la géométrie afin de pouvoir faire quelques manipulations
data_df<-st_drop_geometry(Base_Espece)
class(data_df)
##########???les espèces dans les différentes zones
Base_Espece_Zone1<-data_df %>%
  filter(Zone == 1)
Base_Espece_Zone2<-data_df %>%
  filter(Zone == 2)
Base_Espece_Zone3<-data_df %>%
  filter(Zone == 3)
Base_Espece_Zone4<-data_df %>%
  filter(Zone == 4)
###################Faidherbia albida dans les zones
#zone d'étude
Base_Faidherbia_Z<-data_df %>%
  select(xcoord,ycoord,Faidherbia_albida)
names(Base_Faidherbia_Z)<-c("lon","lat","Faidherbia")
####zone 1
Base_Faidherbia_Z1<-Base_Espece_Zone1 %>%
  select(xcoord,ycoord,Faidherbia_albida)
names(Base_Faidherbia_Z1)<-c("lon","lat","Faidherbia")
FZ1<-Base_Faidherbia_Z1
####zone 2
Base_Faidherbia_Z2<-Base_Espece_Zone2 %>%
  select(xcoord,ycoord,Faidherbia_albida)
names(Base_Faidherbia_Z2)<-c("lon","lat","Faidherbia")
FZ2<-Base_Faidherbia_Z2
####zone 3
Base_Faidherbia_Z3<-Base_Espece_Zone3 %>%
  select(xcoord,ycoord,Faidherbia_albida)
names(Base_Faidherbia_Z3)<-c("lon","lat","Faidherbia")
FZ3<-Base_Faidherbia_Z3
####???zone 4
Base_Faidherbia_Z4<-Base_Espece_Zone4 %>%
  select(xcoord,ycoord,Faidherbia_albida)
names(Base_Faidherbia_Z4)<-c("lon","lat","Faidherbia")
FZ4<-Base_Faidherbia_Z4
##################fin zones
#Transform data as SpatialPointDataFrame
###zone 1
sp::coordinates(FZ1) <-~lon+lat
sp::proj4string(FZ1) <-"+proj=longlat +datum=WGS84"
###zone 2
sp::coordinates(FZ2) <-~lon+lat
sp::proj4string(FZ2) <-"+proj=longlat +datum=WGS84"
###zone 3
sp::coordinates(FZ3) <-~lon+lat
sp::proj4string(FZ3) <-"+proj=longlat +datum=WGS84"
###zone 4
sp::coordinates(FZ4) <-~lon+lat
sp::proj4string(FZ4) <-"+proj=longlat +datum=WGS84"
##############"fin zones
#extract covariables, combine with dataset 
###zone 1
dataFZ1<-CovarExtract(x=FZ1,cov.paths = l1) # en utilsisant SDMSelect
###zone 2
dataFZ2<-CovarExtract(x=FZ2,cov.paths = l1)
###zone 3
dataFZ3<-CovarExtract(x=FZ3,cov.paths = l1)
###zone 4
dataFZ4<-CovarExtract(x=FZ4,cov.paths = l1)
#############fins zones
############## exporter dataFZ1 en .shp
library(maptools)
library(rgdal) 
tmpdir<-"C:\\Users\\Hp\\OneDrive\\redactions"
##zone 1
writeOGR(obj=dataFZ1,dsn=tmpdir,layer="dataFZ1",driver="ESRI Shapefile")
##zone 2
writeOGR(obj=dataFZ2,dsn=tmpdir,layer="dataFZ2",driver="ESRI Shapefile")
##zone 3
writeOGR(obj=dataFZ3,dsn=tmpdir,layer="dataFZ3",driver="ESRI Shapefile")
##zone 4
writeOGR(obj=dataFZ4,dsn=tmpdir,layer="dataFZ4",driver="ESRI Shapefile")
############### importer dataFZ1.shp,dataFZ2.shp,dataFZ3.shp,dataFZ4.shp
##zone 1
filename_PA_Z1<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataFZ1.shp")
PA_FZ1<-shapefile(filename_PA_Z1)
map1<-st_as_sf(PA_FZ1)
map1$Faidherbia<-if_else(map1$Faidherbia ==1,"présence","absence")
map1$Faidherbia<-as.factor(map1$Faidherbia)
##zone 2
filename_PA_Z2<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataFZ2.shp")
PA_FZ2<-shapefile(filename_PA_Z2)
map2<-st_as_sf(PA_FZ2)
map2$Faidherbia<-if_else(map2$Faidherbia ==1,"présence","absence")
map2$Faidherbia<-as.factor(map2$Faidherbia)
##zone 3
filename_PA_Z3<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataFZ3.shp")
PA_FZ3<-shapefile(filename_PA_Z3)
map3<-st_as_sf(PA_FZ3)
map3$Faidherbia<-if_else(map3$Faidherbia ==1,"présence","absence")
map3$Faidherbia<-as.factor(map3$Faidherbia)
##zone 4
filename_PA_Z4<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataFZ4.shp")
PA_FZ4<-shapefile(filename_PA_Z4)
map4<-st_as_sf(PA_FZ4)
map4$Faidherbia<-if_else(map4$Faidherbia ==1,"présence","absence")
map4$Faidherbia<-as.factor(map4$Faidherbia)
##########.shp des zones 
##zone 1
zone_etude1<-shapefile("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\shpzones\\Zone_1_BON.shp")
z1<-st_as_sf(zone_etude1)

##zone 2
zone_etude2<-shapefile("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\shpzones\\Zone_2_BON.shp")
z2<-st_as_sf(zone_etude2)
##zone 3
zone_etude3<-shapefile("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\shpzones\\Zone_3_BON.shp")
z3<-st_as_sf(zone_etude3)
##zone 4
zone_etude4<-shapefile("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\shpzones\\Zone_4_BON.shp")
z4<-st_as_sf(zone_etude4)
#########"fin zones
###########répresentation graphique des présence/absence de Faidherbia albida dans les zones
##zone 1
plotPAZ1<-ggplot(map1)   +
  geom_sf(aes(color = Faidherbia)) +
  geom_sf(data = z1, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Faidherbia Model\\PA_FaidherbiaZ1.png",plotPAZ1)
##zone 2
plotPAZ2<-ggplot(map2)   +
  geom_sf(aes(color = Faidherbia)) +
  geom_sf(data = z2, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Faidherbia Model\\PA_FaidherbiaZ2.png",plotPAZ2)
##zone 3
plotPAZ3<-ggplot(map3)   +
  geom_sf(aes(color = Faidherbia)) +
  geom_sf(data = z3, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Faidherbia Model\\PA_FaidherbiaZ3.png",plotPAZ3)
##zone 4
plotPAZ4<-ggplot(map4)   +
  geom_sf(aes(color = Faidherbia)) +
  geom_sf(data = z4, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Faidherbia Model\\PA_FaidherbiaZ4.png",plotPAZ4)
#############################################################
#Enregistrements en double pour l'espèce
dups <- duplicated(Base_Faidherbia_Z1[, 1:3])
class(dups)
table(dups)
#???pas de doublon
################ Autocorrélation spatiale
########"
library(spdep)
#zone d'étude
Base_FZ<-Base_Faidherbia_Z
Base_FZ$Faidherbia<-as.factor(Base_FZ$Faidherbia)
###conversion en facteur
Faidherbia <- Base_FZ$Faidherbia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_FZ) <-~lon+lat
sp::proj4string(Base_FZ) <-"+proj=longlat +datum=WGS84"
voisins<- knn2nb(knearneigh(Base_FZ,k=5))

#Mise en oeuvre du test
joincount.test(Faidherbia,listw2U(nb2listw(voisins)))
print(joincount.multi(Faidherbia,listw2U(nb2listw(voisins))))
xtable(joincount.multi(Faidherbia,listw2U(nb2listw(voisins))))
###zone 1
Base_FZ1<-Base_Faidherbia_Z1
Base_FZ1$Faidherbia<-as.factor(Base_FZ1$Faidherbia)
###conversion en facteur
Faidherbia <- Base_FZ1$Faidherbia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_FZ1) <-~lon+lat
sp::proj4string(Base_FZ1) <-"+proj=longlat +datum=WGS84"
voisins<- knn2nb(knearneigh(Base_FZ1,k=5))

#Mise en oeuvre du test
joincount.test(Faidherbia,listw2U(nb2listw(voisins)))
print(joincount.multi(Faidherbia,listw2U(nb2listw(voisins))))
xtable(joincount.multi(Faidherbia,listw2U(nb2listw(voisins))))
#???Jointcount_FZ1<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(61.25,8.25,27.00),Esperance=c(56.6562,5.1562,34.6875),Variance=c(3.0378,1.4507,6.0339),z_value=c(2.6357,2.5686,-3.1296))
#localG()
###zone 3
Base_FZ3<-Base_Faidherbia_Z3
Base_FZ3$Faidherbia<-as.factor(Base_FZ3$Faidherbia)
###conversion en facteur
Faidherbia <- Base_FZ3$Faidherbia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_FZ3) <-~lon+lat
sp::proj4string(Base_FZ3) <-"+proj=longlat +datum=WGS84"
voisins3<- knn2nb(knearneigh(Base_FZ3,k=5))
#Mise en oeuvre du test
joincount.test(Faidherbia,listw2U(nb2listw(voisins3)))
print(joincount.multi(Faidherbia,listw2U(nb2listw(voisins3))))
xtable(joincount.multi(Faidherbia,listw2U(nb2listw(voisins3))))
#???Jointcount_FZ3<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(598.25,437.50,488.25),Esperance=c(459.018,310.018,754.964),Variance=c(57.554,51.683,158.471),z_value=c(18.353,17.733,-21.187))
#localG()
###zone 2
Base_FZ2<-Base_Faidherbia_Z2
Base_FZ2$Faidherbia<-as.factor(Base_FZ2$Faidherbia)
###conversion en facteur
Faidherbia <- Base_FZ2$Faidherbia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_FZ2) <-~lon+lat
sp::proj4string(Base_FZ2) <-"+proj=longlat +datum=WGS84"
voisins2<- knn2nb(knearneigh(Base_FZ2,k=5))
#Mise en oeuvre du test
joincount.test(Faidherbia,listw2U(nb2listw(voisins2)))
print(joincount.multi(Faidherbia,listw2U(nb2listw(voisins2))))
xtable(joincount.multi(Faidherbia,listw2U(nb2listw(voisins2))))
Jointcount_FZ2<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(553.75,329.00,440.25),Esperance=c(441.647,235.647,645.706),Variance=c(50.698,42.594,134.574),z_value=c(15.744,14.304,-17.711))
#localG()
###zone 4
Base_FZ4<-Base_Faidherbia_Z4
Base_FZ4$Faidherbia<-as.factor(Base_FZ4$Faidherbia)
###conversion en facteur
Faidherbia <- Base_FZ4$Faidherbia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_FZ4) <-~lon+lat
sp::proj4string(Base_FZ4) <-"+proj=longlat +datum=WGS84"
voisins4<- knn2nb(knearneigh(Base_FZ4,k=5))
#Mise en oeuvre du test
xtable(joincount.test(Faidherbia,listw2U(nb2listw(voisins4))))
print(joincount.multi(Faidherbia,listw2U(nb2listw(voisins4))))
#Jointcount_FZ4<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(598.25,437.50,488.25),Esperance=c(459.018,310.018,754.964),Variance=c(57.554,51.683,158.471),z_value=c(18.353,17.733,-21.187))
#localG()
library(xtable)
xtable(joincount.multi(Faidherbia,listw2U(nb2listw(voisins4))))


#############1 avec le corrélogramme de SDMSelect
######zone 1
data.prepared1<-Prepare_dataset(x=dataFZ1,var=1,cov = 2:ncol(dataFZ1),datatype = "PA",na.rm = TRUE)
######zone 2
data.prepared2<-Prepare_dataset(x=dataFZ2,var=1,cov = 2:ncol(dataFZ2),datatype = "PA",na.rm = TRUE)
library(gstat)



























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
