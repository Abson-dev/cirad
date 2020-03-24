###################Acacia nilotica dans les zones
#Acacia nilotica
Base_Espece$Acacia_nilotica<-as.factor(if_else(Base_Espece$Species =="Acacia nilotica","1","0"))
#zone d'étude
Base_Acacia_Z<-data_df %>%
  select(xcoord,ycoord,Acacia_nilotica)
names(Base_Acacia_Z)<-c("lon","lat","Acacia")
####zone 1
Base_Acacia_Z1<-Base_Espece_Zone1 %>%
  select(xcoord,ycoord,Acacia_nilotica)
names(Base_Acacia_Z1)<-c("lon","lat","Acacia")
AcZ1<-Base_Acacia_Z1
####zone 2
Base_Acacia_Z2<-Base_Espece_Zone2 %>%
  select(xcoord,ycoord,Acacia_nilotica)
names(Base_Acacia_Z2)<-c("lon","lat","Acacia")
AcZ2<-Base_Acacia_Z2
####zone 3
Base_Acacia_Z3<-Base_Espece_Zone3 %>%
  select(xcoord,ycoord,Acacia_nilotica)
names(Base_Acacia_Z3)<-c("lon","lat","Acacia")
AcZ3<-Base_Acacia_Z3
####???zone 4
Base_Acacia_Z4<-Base_Espece_Zone4 %>%
  select(xcoord,ycoord,Acacia_nilotica)
names(Base_Acacia_Z4)<-c("lon","lat","Acacia")
AcZ4<-Base_Acacia_Z4
##################fin zones
#Transform data as SpatialPointDataFrame
###zone 1
sp::coordinates(AcZ1) <-~lon+lat
sp::proj4string(AcZ1) <-"+proj=longlat +datum=WGS84"
###zone 2
sp::coordinates(AcZ2) <-~lon+lat
sp::proj4string(AcZ2) <-"+proj=longlat +datum=WGS84"
###zone 3
sp::coordinates(AcZ3) <-~lon+lat
sp::proj4string(AcZ3) <-"+proj=longlat +datum=WGS84"
###zone 4
sp::coordinates(AcZ4) <-~lon+lat
sp::proj4string(AcZ4) <-"+proj=longlat +datum=WGS84"
##############"fin zones
#extract covariables, combine with dataset 
###zone 1
dataAcZ1<-CovarExtract(x=AcZ1,cov.paths = l1) # en utilsisant SDMSelect
###zone 2
dataAcZ2<-CovarExtract(x=AcZ2,cov.paths = l1)
###zone 3
dataAcZ3<-CovarExtract(x=AcZ3,cov.paths = l1)
###zone 4
dataAcZ4<-CovarExtract(x=AcZ4,cov.paths = l1)
#############fins zones
##zone 1
writeOGR(obj=dataAcZ1,dsn=tmpdir,layer="dataAcZ1",driver="ESRI Shapefile")
##zone 2
writeOGR(obj=dataAcZ2,dsn=tmpdir,layer="dataAcZ2",driver="ESRI Shapefile")
##zone 3
writeOGR(obj=dataAcZ3,dsn=tmpdir,layer="dataAcZ3",driver="ESRI Shapefile")
##zone 4
writeOGR(obj=dataAcZ4,dsn=tmpdir,layer="dataAcZ4",driver="ESRI Shapefile")
############### importer dataAcZ1.shp,dataAcZ2.shp,dataAcZ3.shp,dataAcZ4.shp
##zone 1
filename_PA_A_Z1<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAcZ1.shp")
PA_AcZ1<-shapefile(filename_PA_A_Z1)
map_Ac_1<-st_as_sf(PA_AcZ1)
map_Ac_1$Acacia<-if_else(map_Ac_1$Acacia ==1,"présence","absence")
map_Ac_1$Acacia<-as.factor(map_Ac_1$Acacia)
##zone 2
filename_PA_A_Z2<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAcZ2.shp")
PA_AcZ2<-shapefile(filename_PA_A_Z2)
map_Ac_2<-st_as_sf(PA_AcZ2)
map_Ac_2$Acacia<-if_else(map_Ac_2$Acacia ==1,"présence","absence")
map_Ac_2$Acacia<-as.factor(map_Ac_2$Acacia)
##zone 3
filename_PA_A_Z3<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAcZ3.shp")
PA_AcZ3<-shapefile(filename_PA_A_Z3)
map_Ac_3<-st_as_sf(PA_AcZ3)
map_Ac_3$Acacia<-if_else(map_Ac_3$Acacia ==1,"présence","absence")
map_Ac_3$Acacia<-as.factor(map_Ac_3$Acacia)
##zone 4
filename_PA_A_Z4<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAcZ4.shp")
PA_AcZ4<-shapefile(filename_PA_A_Z4)
map_Ac_4<-st_as_sf(PA_AcZ4)
map_Ac_4$Acacia<-if_else(map_Ac_4$Acacia ==1,"présence","absence")
map_Ac_4$Acacia<-as.factor(map_Ac_4$Acacia)
##########.shp des zones déjà importer
###########répresentation graphique des présence/absence de Acacia  dans les zones
##zone 1
plotPA_Ac_Z1<-ggplot(map_Ac_1)   +
  geom_sf(aes(color = Acacia)) +
  geom_sf(data = z1, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Acacia Model\\PA_AcaciAcZ1.png",plotPA_Ac_Z1)
##zone 2
plotPA_Ac_Z2<-ggplot(map_Ac_2)   +
  geom_sf(aes(color = Acacia)) +
  geom_sf(data = z2, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Acacia Model\\PA_AcaciAcZ2.png",plotPA_Ac_Z2)
##zone 3
plotPA_Ac_Z3<-ggplot(map_Ac_3)   +
  geom_sf(aes(color = Acacia)) +
  geom_sf(data = z3, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Acacia Model\\PA_AcaciAcZ3.png",plotPA_Ac_Z3)
##zone 4
plotPA_Ac_Z4<-ggplot(map_Ac_4)   +
  geom_sf(aes(color = Acacia)) +
  geom_sf(data = z4, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Acacia Model\\PA_AcaciAcZ4.png",plotPA_Ac_Z4)
#############################################################
#Enregistrements en double pour l'espèce
dups <- duplicated(Base_Acacia_Z1[, 1:3])
class(dups)
table(dups)
#???pas de doublon
################ Autocorrélation spatiale
########"
library(spdep)
#zone d'étude
Base_AcZ<-Base_Acacia_Z
Base_AcZ$Acacia<-as.factor(Base_AcZ$Acacia)
###conversion en facteur
Acacia <- Base_AcZ$Acacia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_AcZ) <-~lon+lat
sp::proj4string(Base_AcZ) <-"+proj=longlat +datum=WGS84"
voisins<- knn2nb(knearneigh(Base_AcZ,k=5))

#Mise en oeuvre du test
joincount.test(Acacia,listw2U(nb2listw(voisins)))
print(joincount.multi(Acacia,listw2U(nb2listw(voisins))))
xtable(joincount.multi(Acacia,listw2U(nb2listw(voisins))))
###zone 1
Base_AcZ1<-Base_Acacia_Z1
Base_AcZ1$Acacia<-as.factor(Base_AcZ1$Acacia)
###conversion en facteur
Acacia <- Base_AcZ1$Acacia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_AcZ1) <-~lon+lat
sp::proj4string(Base_AcZ1) <-"+proj=longlat +datum=WGS84"
voisins<- knn2nb(knearneigh(Base_AcZ1,k=5))

#Mise en oeuvre du test
joincount.test(Acacia,listw2U(nb2listw(voisins)))
print(joincount.multi(Acacia,listw2U(nb2listw(voisins))))
xtable(joincount.multi(Acacia,listw2U(nb2listw(voisins))))
#???Jointcount_FZ1<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(61.25,8.25,27.00),Esperance=c(56.6562,5.1562,34.6875),Variance=c(3.0378,1.4507,6.0339),z_value=c(2.6357,2.5686,-3.1296))
#localG()
###zone 3
Base_AcZ3<-Base_Acacia_Z3
Base_AcZ3$Acacia<-as.factor(Base_AcZ3$Acacia)
###conversion en facteur
Acacia <- Base_AcZ3$Acacia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_AcZ3) <-~lon+lat
sp::proj4string(Base_AcZ3) <-"+proj=longlat +datum=WGS84"
voisins3<- knn2nb(knearneigh(Base_AcZ3,k=5))
#Mise en oeuvre du test
joincount.test(Acacia,listw2U(nb2listw(voisins3)))
print(joincount.multi(Acacia,listw2U(nb2listw(voisins3))))
xtable(joincount.multi(Acacia,listw2U(nb2listw(voisins3))))
#???Jointcount_FZ3<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(598.25,437.50,488.25),Esperance=c(459.018,310.018,754.964),Variance=c(57.554,51.683,158.471),z_value=c(18.353,17.733,-21.187))
#localG()
###zone 2
Base_AcZ2<-Base_Acacia_Z2
Base_AcZ2$Acacia<-as.factor(Base_AcZ2$Acacia)
###conversion en facteur
Acacia <- Base_AcZ2$Acacia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_AcZ2) <-~lon+lat
sp::proj4string(Base_AcZ2) <-"+proj=longlat +datum=WGS84"
voisins2<- knn2nb(knearneigh(Base_AcZ2,k=5))
#Mise en oeuvre du test
joincount.test(Acacia,listw2U(nb2listw(voisins2)))
print(joincount.multi(Acacia,listw2U(nb2listw(voisins2))))
xtable(joincount.multi(Acacia,listw2U(nb2listw(voisins2))))
Jointcount_FZ2<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(553.75,329.00,440.25),Esperance=c(441.647,235.647,645.706),Variance=c(50.698,42.594,134.574),z_value=c(15.744,14.304,-17.711))
#localG()
###zone 4
Base_AcZ4<-Base_Acacia_Z4
Base_AcZ4$Acacia<-as.factor(Base_AcZ4$Acacia)
###conversion en facteur
Acacia <- Base_AcZ4$Acacia
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_AcZ4) <-~lon+lat
sp::proj4string(Base_AcZ4) <-"+proj=longlat +datum=WGS84"
voisins4<- knn2nb(knearneigh(Base_AcZ4,k=5))
#Mise en oeuvre du test
joincount.test(Acacia,listw2U(nb2listw(voisins4)))
xtable(joincount.multi(Acacia,listw2U(nb2listw(voisins4))))
#Jointcount_FZ4<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(598.25,437.50,488.25),Esperance=c(459.018,310.018,754.964),Variance=c(57.554,51.683,158.471),z_value=c(18.353,17.733,-21.187))
#localG()
library(xtable)
xtable(joincount.multi(Acacia,listw2U(nb2listw(voisins4))))


#############1 avec le corrélogramme de SDMSelect