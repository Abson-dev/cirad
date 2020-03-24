#Balanites aegyptiaca
Base_Espece$Balanites_aegyptiaca<-as.factor(if_else(Base_Espece$Species =="Balanites aegyptiaca","1","0"))
###################Balanites albida dans les zones
#zone d'étude
Base_Balanites_Z<-data_df %>%
  select(xcoord,ycoord,Balanites_aegyptiaca)
names(Base_Balanites_Z)<-c("lon","lat","Balanites")
####zone 1
Base_Balanites_Z1<-Base_Espece_Zone1 %>%
  select(xcoord,ycoord,Balanites_aegyptiaca)
names(Base_Balanites_Z1)<-c("lon","lat","Balanites")
BZ1<-Base_Balanites_Z1
####zone 2
Base_Balanites_Z2<-Base_Espece_Zone2 %>%
  select(xcoord,ycoord,Balanites_aegyptiaca)
names(Base_Balanites_Z2)<-c("lon","lat","Balanites")
BZ2<-Base_Balanites_Z2
####zone 3
Base_Balanites_Z3<-Base_Espece_Zone3 %>%
  select(xcoord,ycoord,Balanites_aegyptiaca)
names(Base_Balanites_Z3)<-c("lon","lat","Balanites")
BZ3<-Base_Balanites_Z3
####???zone 4
Base_Balanites_Z4<-Base_Espece_Zone4 %>%
  select(xcoord,ycoord,Balanites_aegyptiaca)
names(Base_Balanites_Z4)<-c("lon","lat","Balanites")
BZ4<-Base_Balanites_Z4
##################fin zones
#Transform data as SpatialPointDataFrame
###zone 1
sp::coordinates(BZ1) <-~lon+lat
sp::proj4string(BZ1) <-"+proj=longlat +datum=WGS84"
###zone 2
sp::coordinates(BZ2) <-~lon+lat
sp::proj4string(BZ2) <-"+proj=longlat +datum=WGS84"
###zone 3
sp::coordinates(BZ3) <-~lon+lat
sp::proj4string(BZ3) <-"+proj=longlat +datum=WGS84"
###zone 4
sp::coordinates(BZ4) <-~lon+lat
sp::proj4string(BZ4) <-"+proj=longlat +datum=WGS84"
##############"fin zones
#extract covariables, combine with dataset 
###zone 1
dataBZ1<-CovarExtract(x=BZ1,cov.paths = l1) # en utilsisant SDMSelect
###zone 2
dataBZ2<-CovarExtract(x=BZ2,cov.paths = l1)
###zone 3
dataBZ3<-CovarExtract(x=BZ3,cov.paths = l1)
###zone 4
dataBZ4<-CovarExtract(x=BZ4,cov.paths = l1)
#############fins zones
##zone 1
writeOGR(obj=dataBZ1,dsn=tmpdir,layer="dataBZ1",driver="ESRI Shapefile")
##zone 2
writeOGR(obj=dataBZ2,dsn=tmpdir,layer="dataBZ2",driver="ESRI Shapefile")
##zone 3
writeOGR(obj=dataBZ3,dsn=tmpdir,layer="dataBZ3",driver="ESRI Shapefile")
##zone 4
writeOGR(obj=dataBZ4,dsn=tmpdir,layer="dataBZ4",driver="ESRI Shapefile")
############### importer dataBZ1.shp,dataBZ2.shp,dataBZ3.shp,dataBZ4.shp
##zone 1
filename_PA_B_Z1<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataBZ1.shp")
PA_BZ1<-shapefile(filename_PA_B_Z1)
map_B_1<-st_as_sf(PA_BZ1)
map_B_1$Balanites<-if_else(map_B_1$Balanites ==1,"présence","absence")
map_B_1$Balanites<-as.factor(map_B_1$Balanites)
##zone 2
filename_PA_B_Z2<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataBZ2.shp")
PA_BZ2<-shapefile(filename_PA_B_Z2)
map_B_2<-st_as_sf(PA_BZ2)
map_B_2$Balanites<-if_else(map_B_2$Balanites ==1,"présence","absence")
map_B_2$Balanites<-as.factor(map_B_2$Balanites)
##zone 3
filename_PA_B_Z3<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataBZ3.shp")
PA_BZ3<-shapefile(filename_PA_B_Z3)
map_B_3<-st_as_sf(PA_BZ3)
map_B_3$Balanites<-if_else(map_B_3$Balanites ==1,"présence","absence")
map_B_3$Balanites<-as.factor(map_B_3$Balanites)
##zone 4
filename_PA_B_Z4<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataBZ4.shp")
PA_BZ4<-shapefile(filename_PA_B_Z4)
map_B_4<-st_as_sf(PA_BZ4)
map_B_4$Balanites<-if_else(map_B_4$Balanites ==1,"présence","absence")
map_B_4$Balanites<-as.factor(map_B_4$Balanites)
##########.shp des zones déjà importer
###########répresentation graphique des présence/absence de Balanites aegyptiaca dans les zones
##zone 1
plotPA_B_Z1<-ggplot(map_B_1)   +
  geom_sf(aes(color = Balanites)) +
  geom_sf(data = z1, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Balanites Model\\PA_BalanitesZ1.png",plotPA_B_Z1)
##zone 2
plotPA_B_Z2<-ggplot(map_B_2)   +
  geom_sf(aes(color = Balanites)) +
  geom_sf(data = z2, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Balanites Model\\PA_BalanitesZ2.png",plotPA_B_Z2)
##zone 3
plotPA_B_Z3<-ggplot(map_B_3)   +
  geom_sf(aes(color = Balanites)) +
  geom_sf(data = z3, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Balanites Model\\PA_BalanitesZ3.png",plotPA_B_Z3)
##zone 4
plotPA_B_Z4<-ggplot(map_B_4)   +
  geom_sf(aes(color = Balanites)) +
  geom_sf(data = z4, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Balanites Model\\PA_BalanitesZ4.png",plotPA_B_Z4)
#############################################################
#Enregistrements en double pour l'espèce
dups <- duplicated(Base_Balanites_Z1[, 1:3])
class(dups)
table(dups)
#???pas de doublon
################ Autocorrélation spatiale
########"
library(spdep)
#zone d'étude
Base_BZ<-Base_Balanites_Z
Base_BZ$Balanites<-as.factor(Base_BZ$Balanites)
###conversion en facteur
Balanites <- Base_BZ$Balanites
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_BZ) <-~lon+lat
sp::proj4string(Base_BZ) <-"+proj=longlat +datum=WGS84"
voisins<- knn2nb(knearneigh(Base_BZ,k=5))

#Mise en oeuvre du test
joincount.test(Balanites,listw2U(nb2listw(voisins)))
print(joincount.multi(Balanites,listw2U(nb2listw(voisins))))
xtable(joincount.multi(Balanites,listw2U(nb2listw(voisins))))
###zone 1
Base_BZ1<-Base_Balanites_Z1
Base_BZ1$Balanites<-as.factor(Base_BZ1$Balanites)
###conversion en facteur
Balanites <- Base_BZ1$Balanites
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_BZ1) <-~lon+lat
sp::proj4string(Base_BZ1) <-"+proj=longlat +datum=WGS84"
voisins<- knn2nb(knearneigh(Base_BZ1,k=5))

#Mise en oeuvre du test
joincount.test(Balanites,listw2U(nb2listw(voisins)))
print(joincount.multi(Balanites,listw2U(nb2listw(voisins))))
xtable(joincount.multi(Balanites,listw2U(nb2listw(voisins))))
#???Jointcount_FZ1<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(61.25,8.25,27.00),Esperance=c(56.6562,5.1562,34.6875),Variance=c(3.0378,1.4507,6.0339),z_value=c(2.6357,2.5686,-3.1296))
#localG()
###zone 3
Base_BZ3<-Base_Balanites_Z3
Base_BZ3$Balanites<-as.factor(Base_BZ3$Balanites)
###conversion en facteur
Balanites <- Base_BZ3$Balanites
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_BZ3) <-~lon+lat
sp::proj4string(Base_BZ3) <-"+proj=longlat +datum=WGS84"
voisins3<- knn2nb(knearneigh(Base_BZ3,k=5))
#Mise en oeuvre du test
joincount.test(Balanites,listw2U(nb2listw(voisins3)))
print(joincount.multi(Balanites,listw2U(nb2listw(voisins3))))
xtable(joincount.multi(Balanites,listw2U(nb2listw(voisins3))))
#???Jointcount_FZ3<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(598.25,437.50,488.25),Esperance=c(459.018,310.018,754.964),Variance=c(57.554,51.683,158.471),z_value=c(18.353,17.733,-21.187))
#localG()
###zone 2
Base_BZ2<-Base_Balanites_Z2
Base_BZ2$Balanites<-as.factor(Base_BZ2$Balanites)
###conversion en facteur
Balanites <- Base_BZ2$Balanites
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_BZ2) <-~lon+lat
sp::proj4string(Base_BZ2) <-"+proj=longlat +datum=WGS84"
voisins2<- knn2nb(knearneigh(Base_BZ2,k=5))
#Mise en oeuvre du test
joincount.test(Balanites,listw2U(nb2listw(voisins2)))
print(joincount.multi(Balanites,listw2U(nb2listw(voisins2))))
xtable(joincount.multi(Balanites,listw2U(nb2listw(voisins2))))
Jointcount_FZ2<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(553.75,329.00,440.25),Esperance=c(441.647,235.647,645.706),Variance=c(50.698,42.594,134.574),z_value=c(15.744,14.304,-17.711))
#localG()
###zone 4
Base_BZ4<-Base_Balanites_Z4
Base_BZ4$Balanites<-as.factor(Base_BZ4$Balanites)
###conversion en facteur
Balanites <- Base_BZ4$Balanites
#Création des listes de voisins et matrices de poids
sp::coordinates(Base_BZ4) <-~lon+lat
sp::proj4string(Base_BZ4) <-"+proj=longlat +datum=WGS84"
voisins4<- knn2nb(knearneigh(Base_BZ4,k=5))
#Mise en oeuvre du test
joincount.test(Balanites,listw2U(nb2listw(voisins4)))
xtable(joincount.multi(Balanites,listw2U(nb2listw(voisins4))))
#Jointcount_FZ4<-data.frame(Liaison=c("Absence:Absence:","Présence:Présence","Présence:Absence"),Jointcount=c(598.25,437.50,488.25),Esperance=c(459.018,310.018,754.964),Variance=c(57.554,51.683,158.471),z_value=c(18.353,17.733,-21.187))
#localG()
library(xtable)
xtable(joincount.multi(Balanites,listw2U(nb2listw(voisins4))))


#############1 avec le corrélogramme de SDMSelect


library(gstat)