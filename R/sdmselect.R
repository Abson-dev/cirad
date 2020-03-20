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
Base_Espece_Zone1<-data_sf %>%
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