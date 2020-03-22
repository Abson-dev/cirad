###################Anogeissus albida dans les zones
####zone 1
Base_Anogeissus_Z1<-Base_Espece_Zone1 %>%
  select(xcoord,ycoord,Anogeissus_leiocarpus)
names(Base_Anogeissus_Z1)<-c("lon","lat","Anogeissus")
AZ1<-Base_Anogeissus_Z1
####zone 2
Base_Anogeissus_Z2<-Base_Espece_Zone2 %>%
  select(xcoord,ycoord,Anogeissus_leiocarpus)
names(Base_Anogeissus_Z2)<-c("lon","lat","Anogeissus")
AZ2<-Base_Anogeissus_Z2
####zone 3
Base_Anogeissus_Z3<-Base_Espece_Zone3 %>%
  select(xcoord,ycoord,Anogeissus_leiocarpus)
names(Base_Anogeissus_Z3)<-c("lon","lat","Anogeissus")
AZ3<-Base_Anogeissus_Z3
####???zone 4
Base_Anogeissus_Z4<-Base_Espece_Zone4 %>%
  select(xcoord,ycoord,Anogeissus_leiocarpus)
names(Base_Anogeissus_Z4)<-c("lon","lat","Anogeissus")
AZ4<-Base_Anogeissus_Z4
##################fin zones
#Transform data as SpatialPointDataFrame
###zone 1
sp::coordinates(AZ1) <-~lon+lat
sp::proj4string(AZ1) <-"+proj=longlat +datum=WGS84"
###zone 2
sp::coordinates(AZ2) <-~lon+lat
sp::proj4string(AZ2) <-"+proj=longlat +datum=WGS84"
###zone 3
sp::coordinates(AZ3) <-~lon+lat
sp::proj4string(AZ3) <-"+proj=longlat +datum=WGS84"
###zone 4
sp::coordinates(AZ4) <-~lon+lat
sp::proj4string(AZ4) <-"+proj=longlat +datum=WGS84"
##############"fin zones
#extract covariables, combine with dataset 
###zone 1
dataAZ1<-CovarExtract(x=AZ1,cov.paths = l1) # en utilsisant SDMSelect
###zone 2
dataAZ2<-CovarExtract(x=AZ2,cov.paths = l1)
###zone 3
dataAZ3<-CovarExtract(x=AZ3,cov.paths = l1)
###zone 4
dataAZ4<-CovarExtract(x=AZ4,cov.paths = l1)
#############fins zones
##zone 1
writeOGR(obj=dataAZ1,dsn=tmpdir,layer="dataAZ1",driver="ESRI Shapefile")
##zone 2
writeOGR(obj=dataAZ2,dsn=tmpdir,layer="dataAZ2",driver="ESRI Shapefile")
##zone 3
writeOGR(obj=dataAZ3,dsn=tmpdir,layer="dataAZ3",driver="ESRI Shapefile")
##zone 4
writeOGR(obj=dataAZ4,dsn=tmpdir,layer="dataAZ4",driver="ESRI Shapefile")
############### importer dataAZ1.shp,dataAZ2.shp,dataAZ3.shp,dataAZ4.shp
##zone 1
filename_PA_A_Z1<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAZ1.shp")
PA_AZ1<-shapefile(filename_PA_A_Z1)
map_A_1<-st_as_sf(PA_AZ1)
map_A_1$Anogeissus<-if_else(map_A_1$Anogeissus ==1,"présence","absence")
map_A_1$Anogeissus<-as.factor(map_A_1$Anogeissus)
##zone 2
filename_PA_A_Z2<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAZ2.shp")
PA_AZ2<-shapefile(filename_PA_A_Z2)
map_A_2<-st_as_sf(PA_AZ2)
map_A_2$Anogeissus<-if_else(map_A_2$Anogeissus ==1,"présence","absence")
map_A_2$Anogeissus<-as.factor(map_A_2$Anogeissus)
##zone 3
filename_PA_A_Z3<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAZ3.shp")
PA_AZ3<-shapefile(filename_PA_A_Z3)
map_A_3<-st_as_sf(PA_AZ3)
map_A_3$Anogeissus<-if_else(map_A_3$Anogeissus ==1,"présence","absence")
map_A_3$Anogeissus<-as.factor(map_A_3$Anogeissus)
##zone 4
filename_PA_A_Z4<-paste0("C:\\Users\\Hp\\OneDrive\\redactions","\\dataAZ4.shp")
PA_AZ4<-shapefile(filename_PA_A_Z4)
map_A_4<-st_as_sf(PA_AZ4)
map_A_4$Anogeissus<-if_else(map_A_4$Anogeissus ==1,"présence","absence")
map_A_4$Anogeissus<-as.factor(map_A_4$Anogeissus)
##########.shp des zones déjà importer
###########répresentation graphique des présence/absence de Anogeissus  dans les zones
##zone 1
plotPA_A_Z1<-ggplot(map_A_1)   +
  geom_sf(aes(color = Anogeissus)) +
  geom_sf(data = z1, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Anogeissus Model\\PA_AnogeissusZ1.png",plotPA_A_Z1)
##zone 2
plotPA_A_Z2<-ggplot(map_A_2)   +
  geom_sf(aes(color = Anogeissus)) +
  geom_sf(data = z2, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Anogeissus Model\\PA_AnogeissusZ2.png",plotPA_A_Z2)
##zone 3
plotPA_A_Z3<-ggplot(map_A_3)   +
  geom_sf(aes(color = Anogeissus)) +
  geom_sf(data = z3, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Anogeissus Model\\PA_AnogeissusZ3.png",plotPA_A_Z3)
##zone 4
plotPA_A_Z4<-ggplot(map_A_4)   +
  geom_sf(aes(color = Anogeissus)) +
  geom_sf(data = z4, colour = "black", fill = NA)  +
  theme_bw() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\Anogeissus Model\\PA_AnogeissusZ4.png",plotPA_A_Z4)
#############################################################