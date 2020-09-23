
ggR_P<-function(r){
  samp <- raster::sampleRegular(r, 5e+05, asRaster = TRUE)
  map_df <- raster::as.data.frame(samp, xy = TRUE, centroids = TRUE, 
                                  na.rm = TRUE)
  colnames(map_df) <- c("Easting", "Northing", "MAP")
  basePlot1 <- ggplot2::ggplot() + ggplot2::geom_raster(data = map_df, 
                                                        ggplot2::aes_string(y = "Northing", x = "Easting", fill = "MAP"))
  basePlot1<-basePlot1 + ggplot2::theme_bw() + ggplot2::labs(x = "Longitude", y = "Latitude") +
    ggtitle(label = names(r)) + 
    theme(plot.title = element_text(hjust = 0.5, size = 10)) + 
    ggplot2::scale_fill_gradientn(name = "Probability \n of occurence", colours = rev(terrain.colors(10)))
  return(basePlot1)
}

PASpecies<-function(rasterLayer){
  samp <- raster::sampleRegular(rasterLayer, 5e+05, asRaster = TRUE)
  map_df <- raster::as.data.frame(samp, xy = TRUE, centroids = TRUE, 
                                  na.rm = TRUE)
  colnames(map_df) <- c("Easting", "Northing", "MAP")
  map_df$MAP<-factor(map_df$MAP)
  basePlot <- ggplot2::ggplot() + ggplot2::geom_raster(data = map_df, 
                                                       ggplot2::aes_string(y = "Northing", x = "Easting", fill = "MAP"))
  basePlot<-basePlot + ggplot2::theme_bw() + ggplot2::labs(x = "Longitude", y = "Latitude") + ggtitle(label = names(rasterLayer)) + theme(plot.title = element_text(hjust = 0.5, size = 10))+scale_fill_manual(values=c("white","green"),name="Specie",labels=c("Absence","Presence"))
  
  return(basePlot)
}

TimesRasters<-function(x,y){
  z<-x * y
  names(z)<-names(x)
  return(z)
}

l1<-list.files("C:\\Users\\DELLDRAMOMO\\Desktop\\Sorties\\Niakhar\\",patt="\\.tif")
l1<-sprintf("C:\\Users\\DELLDRAMOMO\\Desktop\\Sorties\\Niakhar\\%s",l1)


ggR_P(raster(l1[1]))

PASpecies(raster(l1[2]))

samp <- raster::sampleRegular(raster(l1[2]), 5e+05, asRaster = TRUE)
map_df2 <- raster::as.data.frame(samp, xy = TRUE, centroids = TRUE, 
                                na.rm = FALSE)
colnames(map_df) <- c("Easting", "Northing", "MAP")

r<-TimesRasters(raster(l1[1]),raster(l1[2]))
ggR_P(r)
l1
tmpdir<-"C:\\Users\\DELLDRAMOMO\\Desktop\\Stage_SDM\\Sorties_Cartes_de_Proba\\Niakhar\\Probabilités\\"
writeRaster(r, filename=file.path(tmpdir, paste0(names(r),'.tif')), format="GTiff", overwrite=TRUE)

paste0(names(r),'.tif')
for (i in 0:10) {
  r<-TimesRasters(raster(l1[2*i+1]),raster(l1[2*(i+1)]))
  writeRaster(r, filename=file.path(tmpdir, paste0(names(r),'.tif')), format="GTiff", overwrite=TRUE)
  
}
##########################################################
l1<-list.files("C:\\Users\\DELLDRAMOMO\\Desktop\\Stage_SDM\\Sorties_Cartes_de_Proba\\Niakhar\\Probabilités\\",patt="\\.tif")
l1<-sprintf("C:\\Users\\DELLDRAMOMO\\Desktop\\Stage_SDM\\Sorties_Cartes_de_Proba\\Niakhar\\Probabilités\\%s",l1)
samp <- raster::sampleRegular(raster(l1[1]), 5e+05, asRaster = TRUE)
map_df <- raster::as.data.frame(samp, xy = TRUE, centroids = TRUE, 
                            na.rm = TRUE)
Proba<-map_df
for (i in 2:length(l1)){
  samp <- raster::sampleRegular(raster(l1[i]), 5e+05, asRaster = TRUE)
  map_df <- raster::as.data.frame(samp, xy = TRUE, centroids = TRUE, 
                                  na.rm = TRUE) %>% 
    select(3)
  Proba<-cbind(Proba,map_df)
  
}
############

Proba2<-Proba[,3:ncol(Proba)]

#Proba2<-apply(Proba2, 2, function(x) ifelse(x==0,0,1))
Proba2<-cbind(Proba[,1:2],Proba2)

Espece<-reshape(Proba2,direction = "long",idvar = c("x","y"),varying = list(3:ncol(Proba2)),v.names = "Presence" ,timevar = "Espece",times = c("Acacia_seyalNiakhar", "Acaciavar", "Adansoniavar", 
                                                                                                                                               "Anogeissusvar", "AzadirachtaNiakhar", "Balanitesvar", "BauhiniaNiakhar", 
                                                                                                                                               "BorassusNiakhar", "CeltisNiakhar", "DiospirosNiakhar", "Faidherbiavar", 
                                                                                                                                               "PiliostigmaNiakhar", "ProsopisNiakhar", "SclerocaryaNiakhar", 
                                                                                                                                               "TamarindusNiakhar", "ZiziphusNiakhar")) %>% 
  filter(Presence!=0)

x_y<-Espece %>% 
  subset(round(x,5)==-16.39735 & round(y,5)==14.63812)

Base_Proba<-Proba %>% 
  select(3:ncol(Proba)) %>% 
  apply(2,function(x) round(x*100,0))

Base_Proba<-as.data.frame(Base_Proba)

names(Base_Proba)<-c("Acacia seyal","Acacia nilotica","Adansonia digitata","Anogeissus leiocarpus","Azadirachta indica",
                     "Balanites aegyptiaca","Bauhinia rufescens","Borassus aethiopium","Celtis integrifolia",
                     "Diospiros mespiliformis","Faidherbia albida","Piliostigma reticulatum","Prosopis juliflora",
                     "Sclerocarya birrea","Tamarindus indica","Ziziphus mauritiana")
Base_Proba<-cbind(Proba[,1:2],Base_Proba)

write.csv2(Base_Proba,"Base_Proba_Niakhar.csv")
#################################"
#####################Calcul des indices
#Base_indices$Sites<-paste0("Site",1:nrow(Base_Proba)) 
Base_indices<-Base_Proba
Base_indices$Sites<-1:nrow(Base_Proba) 
Base_indices<-Base_indices[,3:ncol(Base_indices)]

####Section 1 : species richness
library(plyr)
library(vegan)
# brillouin <- function(x) {
#     N <- sum(x)
#     (log(factorial(N)) - sum(log(factorial(x))))/N
#   }
IndicesNiakhar<-ddply(Base_indices,~Sites,function(x) {
     data.frame(RICHNESS=sum(x[-17]>0),
                ABUNDANCE=sum(x[-17]),
                RAREFY=rarefy(x[-17], sample=10, MARGIN=1),
                SHANNON=diversity(x[-17], index="shannon"),
                #BRILLOUIN=brillouin(x[-17]),
                SIMPSON=diversity(x[-17], index="simpson"),
                TRUE_SHANNON=exp(diversity(x[-17], index="shannon")))
   })


Base_I<-Base_indices[,-17]
library(vegan)
data(dune.env)
data(dune)

ind<-diversityresult(Base_I, y=NULL, index =c("Shannon", "Simpson", "inverseSimpson", "Logalpha", "Berger",
                                        "richness", "abundance", "Jevenness", "Eevenness",
                                        "jack1", "jack2", "chao", "boot"), method=c("pooled", "each site", "mean", "sd", "max", "jackknife"), 
                sortit=FALSE, digits=5)

ind_Eevenness<-diversityresult(Base_I, y = NULL, factor = NULL, level = NULL,
                index=c("Eevenness"),
                method=c( "each site"),
                sortit = FALSE, digits = 8)



ind_Jevenness<-diversityresult(Base_I, y = NULL, factor = NULL, level = NULL,
                               index=c("Jevenness"),
                               method=c( "each site"),
                               sortit = FALSE, digits = 8)

IndicesNiakhar<-cbind(IndicesNiakhar,ind_Eevenness,ind_Jevenness)

inverseSimpson<-diversityresult(Base_I, y = NULL, factor = NULL, level = NULL,
                                index=c("inverseSimpson"),
                                method=c( "each site"),
                                sortit = FALSE, digits = 8)
IndicesNiakhar<-cbind(IndicesNiakhar,inverseSimpson)

x_y<-Base_Proba[,1:2]
IndicesNiakhar<-IndicesNiakhar[,-1]
IndicesNiakhar<-cbind(x_y,IndicesNiakhar)

write.csv2(IndicesNiakhar,"IndicesNiakhar.csv")
