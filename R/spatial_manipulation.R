#
install.packages("raster")
library(raster)
filename<-paste0("E:\\Stage_SDM\\SDM\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK.shp")
filename
Species_shape<-shapefile(filename)
Species_raster<-raster("E:\\Stage_SDM\\SDM\\Data\\BD_Arbre\\arbres_diohine_mai2018_par_Zone_OK.shp")
install.packages("spdep")
library(spdep)
#wr<-poly2nb(Species_shape,row.names = Species_shape$ID_GPS,queen = F)
library(sf)
map<-st_as_sf(Base_Espece)
ggplot(map) + geom_sf(aes(fill=Zone)) + theme_bw()
######################"
#######################" les données sur WorlClim
install.packages("sp")
library(sp)
install.packages("rgdal")
library(rgdal)
library(raster)
install.packages("spData")
library(spData)
devtools::install_github("Nowosad/spDataLarge")
library(spDataLarge)   # load larger geographic data
#Répertoire, fichier et noms de variable
# Chemin d'accès au dossier contenant les 19 variables bioclimatiques
setwd("E:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\")

l1<-list.files("E:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\",patt="\\.tif")
l1<-sprintf("E:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\%s",l1)
worlClim<-stack(l1)
worlClim2<-raster("E:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\wc2.0_bio_30s_01.tif")
names(worlClim)
plot(worlClim)
plot(worlClim$wc2.0_bio_30s_17)
#############################################
worldRaster <- raster(system.file("external/bioclim/current/bio3.grd", package = "biomod2"))
worldRaster[!is.na(worldRaster)] <- 0
plot(worldRaster)
library(rgdal)
e<-extent(334191.9, 354055.6, 1598513, 1618456)
shp2raster <- function(shp, mask.raster, label, value, transform = FALSE, proj.from = NA,
                       proj.to = NA, map = TRUE) {
  require(raster, rgdal)
  
  # use transform==TRUE if the polygon is not in the same coordinate system as
  # the output raster, setting proj.from & proj.to to the appropriate
  # projections
  if (transform == TRUE) {
    proj4string(shp) <- proj.from
    shp <- spTransform(shp, proj.to)
  }
  
  # convert the shapefile to a raster based on a standardised background
  # raster
  r <- rasterize(shp, mask.raster)
  # set the cells associated with the shapfile to the specified value
  r[!is.na(r)] <- value
  # merge the new raster with the mask raster and export to the working
  # directory as a tif file
  r <- mask(merge(r, mask.raster), mask.raster, filename = label, format = "GTiff",
            overwrite = T)
  
  # plot map of new raster
  if (map == TRUE) {
    plot(r, main = label, axes = F, box = F)
  }
  
  names(r) <- label
  return(r)
}
####
# extract all Australian polygons and convert to a world raster where cells
# associated with Australia have a value of 1 and everything else has a
# value of 0.
shp2raster(shp = zone_etude ,mask.raster = worldRaster, label = "Where Amy currently lives", transform = FALSE, value = 1)
#######################################
filename_zone<-paste0("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4","\\Diohine_Echanti_Classif.shp")
filename_zone
zone_etude<-shapefile(filename_zone)
zone_raster<-raster("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\Diohine_Echanti_Classif.shp")
library(sf)
library("ggspatial")
install.packages("tmap")
library(tmap)
map<-st_as_sf(zone_etude)
strat_zone<-ggplot(map)  + geom_sf(aes(fill=Zone),colour="green") + 
  theme_gray() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\strat_zone.png",strat_zone)
t_sol<-ggplot(map)  + geom_sf(aes(fill=Type_Sol),colour="green") + 
  theme_gray() + annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\t_sol.png",t_sol)



