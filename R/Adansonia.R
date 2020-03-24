###################Adansonia digitata dans les zones
#Adansonia digitata
Base_Espece$Adansonia_digitata<-as.factor(if_else(Base_Espece$Species =="Adansonia leiocarpus","1","0"))
#zone d'étude
Base_Adansonia_Z<-data_df %>%
  select(xcoord,ycoord,Adansonia_digitata)
names(Base_Adansonia_Z)<-c("lon","lat","Adansonia")
####zone 1
Base_Adansonia_Z1<-Base_Espece_Zone1 %>%
  select(xcoord,ycoord,Adansonia_digitata)
names(Base_Adansonia_Z1)<-c("lon","lat","Adansonia")
AdZ1<-Base_Adansonia_Z1
####zone 2
Base_Adansonia_Z2<-Base_Espece_Zone2 %>%
  select(xcoord,ycoord,Adansonia_digitata)
names(Base_Adansonia_Z2)<-c("lon","lat","Adansonia")
AdZ2<-Base_Adansonia_Z2
####zone 3
Base_Adansonia_Z3<-Base_Espece_Zone3 %>%
  select(xcoord,ycoord,Adansonia_digitata)
names(Base_Adansonia_Z3)<-c("lon","lat","Adansonia")
AdZ3<-Base_Adansonia_Z3
####???zone 4
Base_Adansonia_Z4<-Base_Espece_Zone4 %>%
  select(xcoord,ycoord,Adansonia_digitata)
names(Base_Adansonia_Z4)<-c("lon","lat","Adansonia")
AdZ4<-Base_Adansonia_Z4
##################fin zones
#Transform data as SpatialPointDataFrame
###zone 1
sp::coordinates(AdZ1) <-~lon+lat
sp::proj4string(AdZ1) <-"+proj=longlat +datum=WGS84"
###zone 2
sp::coordinates(AdZ2) <-~lon+lat
sp::proj4string(AdZ2) <-"+proj=longlat +datum=WGS84"
###zone 3
sp::coordinates(AdZ3) <-~lon+lat
sp::proj4string(AdZ3) <-"+proj=longlat +datum=WGS84"
###zone 4
sp::coordinates(AdZ4) <-~lon+lat
sp::proj4string(AdZ4) <-"+proj=longlat +datum=WGS84"
##############"fin zones
#extract covariables, combine with dataset 
###zone 1
dataAdZ1<-CovarExtract(x=AdZ1,cov.paths = l1) # en utilsisant SDMSelect
###zone 2
dataAdZ2<-CovarExtract(x=AdZ2,cov.paths = l1)
###zone 3
dataAdZ3<-CovarExtract(x=AdZ3,cov.paths = l1)
###zone 4
dataAdZ4<-CovarExtract(x=AdZ4,cov.paths = l1)
#############fins zones
##zone 1
writeOGR(obj=dataAdZ1,dsn=tmpdir,layer="dataAdZ1",driver="ESRI Shapefile")
##zone 2
writeOGR(obj=dataAdZ2,dsn=tmpdir,layer="dataAdZ2",driver="ESRI Shapefile")
##zone 3
writeOGR(obj=dataAdZ3,dsn=tmpdir,layer="dataAdZ3",driver="ESRI Shapefile")
##zone 4
writeOGR(obj=dataAdZ4,dsn=tmpdir,layer="dataAdZ4",driver="ESRI Shapefile")
############### importer dataAdZ1.shp,dataAdZ2.shp,dataAdZ3.shp,dataAdZ4.shp
##zone 1
filename_PA_A_Z1<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAdZ1.shp")
library(raster)
PA_AdZ1<-shapefile(filename_PA_A_Z1)
map_Ad_1<-st_as_sf(PA_AdZ1)
map_Ad_1$Adansonia<-if_else(map_Ad_1$Adansonia ==1,"présence","absence")
map_Ad_1$Adansonia<-as.factor(map_Ad_1$Adansonia)
##zone 2
filename_PA_A_Z2<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAdZ2.shp")
PA_AdZ2<-shapefile(filename_PA_A_Z2)
map_Ad_2<-st_as_sf(PA_AdZ2)
map_Ad_2$Adansonia<-if_else(map_Ad_2$Adansonia ==1,"présence","absence")
map_Ad_2$Adansonia<-as.factor(map_Ad_2$Adansonia)
##zone 3
filename_PA_A_Z3<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAdZ3.shp")
PA_AdZ3<-shapefile(filename_PA_A_Z3)
map_Ad_3<-st_as_sf(PA_AdZ3)
map_Ad_3$Adansonia<-if_else(map_Ad_3$Adansonia ==1,"présence","absence")
map_Ad_3$Adansonia<-as.factor(map_Ad_3$Adansonia)
##zone 4
filename_PA_A_Z4<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAdZ4.shp")
PA_AdZ4<-shapefile(filename_PA_A_Z4)
map_Ad_4<-st_as_sf(PA_AdZ4)
map_Ad_4$Adansonia<-if_else(map_Ad_4$Adansonia ==1,"présence","absence")
map_Ad_4$Adansonia<-as.factor(map_Ad_4$Adansonia)
##########.shp des zones déjà importer
###########répresentation graphique des présence/absence de Adansonia  dans les zones
##zone 1
library(ggplot2)
library(ggspatial)
plotPA_Ad_Z1<-ggplot(map_Ad_1)   +
  geom_sf(aes(color = Adansonia)) +
  geom_sf(data = z1, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Adansonia Model\\PA_AdansoniAdZ1.png",plotPA_Ad_Z1)
##zone 2
plotPA_Ad_Z2<-ggplot(map_Ad_2)   +
  geom_sf(aes(color = Adansonia)) +
  geom_sf(data = z2, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Adansonia Model\\PA_AdansoniAdZ2.png",plotPA_Ad_Z2)
##zone 3
plotPA_Ad_Z3<-ggplot(map_Ad_3)   +
  geom_sf(aes(color = Adansonia)) +
  geom_sf(data = z3, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Adansonia Model\\PA_AdansoniAdZ3.png",plotPA_Ad_Z3)
##zone 4
plotPA_Ad_Z4<-ggplot(map_Ad_4)   +
  geom_sf(aes(color = Adansonia)) +
  geom_sf(data = z4, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Adansonia Model\\PA_AdansoniAdZ4.png",plotPA_Ad_Z4)
#############################################################
#Enregistrements en double pour l'espèce
dups <- duplicated(Base_Adansonia_Z1[, 1:3])
class(dups)
table(dups)
#???pas de doublon
################ Autocorrélation spatiale
########"
library(spdep)
#zone d'étude
Base_AdZ<-Base_Adansonia_Z
Base_AdZ$Adansonia<-as.factor(Base_AdZ$Adansonia)
###conversion en facteur
Adansonia <- Base_AdZ$Adansonia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_AdZ) <-~lon+lat
sp::proj4string(Base_AdZ) <-"+proj=longlat +datum=WGS84"
voisins<- knn2nb(knearneigh(Base_AdZ,k=5))

#Mise en oeuvre du test
joincount.test(Adansonia,listw2U(nb2listw(voisins)))
print(joincount.multi(Adansonia,listw2U(nb2listw(voisins))))
xtable(joincount.multi(Adansonia,listw2U(nb2listw(voisins))))
###zone 1
Base_AdZ1<-Base_Adansonia_Z1
Base_AdZ1$Adansonia<-as.factor(Base_AdZ1$Adansonia)
###conversion en facteur
Adansonia <- Base_AdZ1$Adansonia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_AdZ1) <-~lon+lat
sp::proj4string(Base_AdZ1) <-"+proj=longlat +datum=WGS84"
voisins<- knn2nb(knearneigh(Base_AdZ1,k=5))

#Mise en oeuvre du test
joincount.test(Adansonia,listw2U(nb2listw(voisins)))
print(joincount.multi(Adansonia,listw2U(nb2listw(voisins))))
xtable(joincount.multi(Adansonia,listw2U(nb2listw(voisins))))
#???Jointcount_FZ1<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(61.25,8.25,27.00),Esperance=c(56.6562,5.1562,34.6875),Variance=c(3.0378,1.4507,6.0339),z_value=c(2.6357,2.5686,-3.1296))
#localG()
###zone 3
Base_AdZ3<-Base_Adansonia_Z3
Base_AdZ3$Adansonia<-as.factor(Base_AdZ3$Adansonia)
###conversion en facteur
Adansonia <- Base_AdZ3$Adansonia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_AdZ3) <-~lon+lat
sp::proj4string(Base_AdZ3) <-"+proj=longlat +datum=WGS84"
voisins3<- knn2nb(knearneigh(Base_AdZ3,k=5))
#Mise en oeuvre du test
joincount.test(Adansonia,listw2U(nb2listw(voisins3)))
print(joincount.multi(Adansonia,listw2U(nb2listw(voisins3))))
xtable(joincount.multi(Adansonia,listw2U(nb2listw(voisins3))))
#???Jointcount_FZ3<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(598.25,437.50,488.25),Esperance=c(459.018,310.018,754.964),Variance=c(57.554,51.683,158.471),z_value=c(18.353,17.733,-21.187))
#localG()
###zone 2
Base_AdZ2<-Base_Adansonia_Z2
Base_AdZ2$Adansonia<-as.factor(Base_AdZ2$Adansonia)
###conversion en facteur
Adansonia <- Base_AdZ2$Adansonia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_AdZ2) <-~lon+lat
sp::proj4string(Base_AdZ2) <-"+proj=longlat +datum=WGS84"
voisins2<- knn2nb(knearneigh(Base_AdZ2,k=5))
#Mise en oeuvre du test
joincount.test(Adansonia,listw2U(nb2listw(voisins2)))
print(joincount.multi(Adansonia,listw2U(nb2listw(voisins2))))
xtable(joincount.multi(Adansonia,listw2U(nb2listw(voisins2))))
Jointcount_FZ2<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(553.75,329.00,440.25),Esperance=c(441.647,235.647,645.706),Variance=c(50.698,42.594,134.574),z_value=c(15.744,14.304,-17.711))
#localG()
###zone 4
Base_AdZ4<-Base_Adansonia_Z4
Base_AdZ4$Adansonia<-as.factor(Base_AdZ4$Adansonia)
###conversion en facteur
Adansonia <- Base_AdZ4$Adansonia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_AdZ4) <-~lon+lat
sp::proj4string(Base_AdZ4) <-"+proj=longlat +datum=WGS84"
voisins4<- knn2nb(knearneigh(Base_AdZ4,k=5))
#Mise en oeuvre du test
joincount.test(Adansonia,listw2U(nb2listw(voisins4)))
xtable(joincount.multi(Adansonia,listw2U(nb2listw(voisins4))))
#Jointcount_FZ4<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(598.25,437.50,488.25),Esperance=c(459.018,310.018,754.964),Variance=c(57.554,51.683,158.471),z_value=c(18.353,17.733,-21.187))
#localG()
library(xtable)
xtable(joincount.multi(Adansonia,listw2U(nb2listw(voisins4))))


#############1 avec le corrélogramme de SDMSelect