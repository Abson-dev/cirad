install.packages("dplyr")
library(dplyr)
#Recodage en présence absence
#présence=1 et absence=0
#
Base_Espece<-Species
Base_Espece$Faidherbia_albida<-if_else(Base_Espece$Species =="Faidherbia albida","1","0")
#Balanites aegyptiaca
Base_Espece$Balanites_aegyptiaca<-if_else(Base_Espece$Species =="Balanites aegyptiaca","1","0")
#Anogeissus leiocarpus
Base_Espece$Anogeissus_leiocarpus<-if_else(Base_Espece$Species =="Anogeissus leiocarpus","1","0")
#Adansonia digitata
Base_Espece$Adansonia_digitata<-if_else(Base_Espece$Species =="Adansonia digitata","1","0")
#Acacia nilotica
Base_Espece$Acacia_nilotica<-if_else(Base_Espece$Species =="Acacia nilotica","1","0")
library(sf)
Base_Espece_df<-st_drop_geometry(Base_Espece)
install.packages("funModeling")
library(funModeling)
def_espece<-df_status(Base_Espece_df)
Base_Espece_df$Faidherbia_albida<-as.factor(Base_Espece_df$Faidherbia_albida)
Base_Espece_df$Balanites_aegyptiaca<-as.factor(Base_Espece_df$Balanites_aegyptiaca)
Base_Espece_df$Anogeissus_leiocarpus<-as.factor(Base_Espece_df$Anogeissus_leiocarpus)
Base_Espece_df$Adansonia_digitata<-as.factor(Base_Espece_df$Adansonia_digitata)
Base_Espece_df$Acacia_nilotica<-as.factor(Base_Espece_df$Acacia_nilotica)
def_espece<-df_status(Base_Espece_df)
Base_Espece_df$Zone<-as.factor(Base_Espece_df$Zone)
def_espece<-df_status(Base_Espece_df)
##################
#cross_plot suivant les zones
cross_plot_Faidherbia<-cross_plot(Base_Espece_df,input="Zone",target = "Faidherbia_albida",path_out="C:\\Users\\Hp\\OneDrive\\redactions")
cross_plot_Balanites<-cross_plot(Base_Espece_df,input="Zone",target = "Balanites_aegyptiaca",path_out="C:\\Users\\Hp\\OneDrive\\redactions")
cross_plot_Anogeissus<-cross_plot(Base_Espece_df,input="Zone",target = "Anogeissus_leiocarpus",path_out="C:\\Users\\Hp\\OneDrive\\redactions")
cross_plot_Adansonia<-cross_plot(Base_Espece_df,input="Zone",target = "Adansonia_digitata",path_out="C:\\Users\\Hp\\OneDrive\\redactions")
cross_plot_Acacia<-cross_plot(Base_Espece_df,input="Zone",target = "Acacia_nilotica",path_out="C:\\Users\\Hp\\OneDrive\\redactions")
################################.
#cross_plot suivant NDVI_mean
cross_plot_Faidherbia_10<-cross_plot(Base_Espece_df, input="NDVI_mean", target="Faidherbia_albida",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
Base_Espece_df$NDVI_mean_groupe=equal_freq(var=Base_Espece_df$NDVI_mean, n_bins = 3)  
summary(Base_Espece_df$NDVI_mean_groupe) 
cross_plot_Faidherbia_3<-cross_plot(Base_Espece_df, input="NDVI_mean_groupe", target="Faidherbia_albida", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
###???
cross_plot_Balanites_10<-cross_plot(Base_Espece_df, input="NDVI_mean", target="Balanites_aegyptiaca",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Balanites_3<-cross_plot(Base_Espece_df, input="NDVI_mean_groupe", target="Balanites_aegyptiaca", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Anogeissus_10<-cross_plot(Base_Espece_df, input="NDVI_mean", target="Anogeissus_leiocarpus",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Anogeissus_3<-cross_plot(Base_Espece_df, input="NDVI_mean_groupe", target="Anogeissus_leiocarpus", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Adansonia_10<-cross_plot(Base_Espece_df, input="NDVI_mean", target="Adansonia_digitata",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Adansonia_3<-cross_plot(Base_Espece_df, input="NDVI_mean_groupe", target="Adansonia_digitata", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Acacia_10<-cross_plot(Base_Espece_df, input="NDVI_mean", target="Acacia_nilotica",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Acacia_3<-cross_plot(Base_Espece_df, input="NDVI_mean_groupe", target="Acacia_nilotica", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
#cross_plot suivant AET_mean
cross_plot_Faidherbia_10<-cross_plot(Base_Espece_df, input="AET_mean", target="Faidherbia_albida",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
Base_Espece_df$AET_mean_groupe=equal_freq(var=Base_Espece_df$AET_mean, n_bins = 3)  
summary(Base_Espece_df$AET_mean_groupe) 
cross_plot_Faidherbia_3<-cross_plot(Base_Espece_df, input="AET_mean_groupe", target="Faidherbia_albida", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
###???
cross_plot_Balanites_10<-cross_plot(Base_Espece_df, input="AET_mean", target="Balanites_aegyptiaca",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Balanites_3<-cross_plot(Base_Espece_df, input="AET_mean_groupe", target="Balanites_aegyptiaca", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Anogeissus_10<-cross_plot(Base_Espece_df, input="AET_mean", target="Anogeissus_leiocarpus",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Anogeissus_3<-cross_plot(Base_Espece_df, input="AET_mean_groupe", target="Anogeissus_leiocarpus", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Adansonia_10<-cross_plot(Base_Espece_df, input="AET_mean", target="Adansonia_digitata",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Adansonia_3<-cross_plot(Base_Espece_df, input="AET_mean_groupe", target="Adansonia_digitata", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Acacia_10<-cross_plot(Base_Espece_df, input="AET_mean", target="Acacia_nilotica",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Acacia_3<-cross_plot(Base_Espece_df, input="AET_mean_groupe", target="Acacia_nilotica", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
#cross_plot suivant Woody_mean
cross_plot_Faidherbia_10<-cross_plot(Base_Espece_df, input="Woody_mean", target="Faidherbia_albida",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
Base_Espece_df$Woody_mean_groupe=equal_freq(var=Base_Espece_df$Woody_mean, n_bins = 3)  
summary(Base_Espece_df$Woody_mean_groupe) 
cross_plot_Faidherbia_3<-cross_plot(Base_Espece_df, input="Woody_mean_groupe", target="Faidherbia_albida", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
###???
cross_plot_Balanites_10<-cross_plot(Base_Espece_df, input="Woody_mean", target="Balanites_aegyptiaca",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Balanites_3<-cross_plot(Base_Espece_df, input="Woody_mean_groupe", target="Balanites_aegyptiaca", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Anogeissus_10<-cross_plot(Base_Espece_df, input="Woody_mean", target="Anogeissus_leiocarpus",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Anogeissus_3<-cross_plot(Base_Espece_df, input="Woody_mean_groupe", target="Anogeissus_leiocarpus", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Adansonia_10<-cross_plot(Base_Espece_df, input="Woody_mean", target="Adansonia_digitata",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Adansonia_3<-cross_plot(Base_Espece_df, input="Woody_mean_groupe", target="Adansonia_digitata", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Acacia_10<-cross_plot(Base_Espece_df, input="Woody_mean", target="Acacia_nilotica",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Acacia_3<-cross_plot(Base_Espece_df, input="Woody_mean_groupe", target="Acacia_nilotica", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
#cross_plot suivant Slope_mean
cross_plot_Faidherbia_10<-cross_plot(Base_Espece_df, input="Slope_mean", target="Faidherbia_albida",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
Base_Espece_df$Slope_mean_groupe=equal_freq(var=Base_Espece_df$Slope_mean, n_bins = 3)  
summary(Base_Espece_df$Slope_mean_groupe) 
cross_plot_Faidherbia_3<-cross_plot(Base_Espece_df, input="Slope_mean_groupe", target="Faidherbia_albida", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
###???
cross_plot_Balanites_10<-cross_plot(Base_Espece_df, input="Slope_mean", target="Balanites_aegyptiaca",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Balanites_3<-cross_plot(Base_Espece_df, input="Slope_mean_groupe", target="Balanites_aegyptiaca", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Anogeissus_10<-cross_plot(Base_Espece_df, input="Slope_mean", target="Anogeissus_leiocarpus",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Anogeissus_3<-cross_plot(Base_Espece_df, input="Slope_mean_groupe", target="Anogeissus_leiocarpus", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Adansonia_10<-cross_plot(Base_Espece_df, input="Slope_mean", target="Adansonia_digitata",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Adansonia_3<-cross_plot(Base_Espece_df, input="Slope_mean_groupe", target="Adansonia_digitata", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Acacia_10<-cross_plot(Base_Espece_df, input="Slope_mean", target="Acacia_nilotica",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
cross_plot_Acacia_3<-cross_plot(Base_Espece_df, input="Slope_mean_groupe", target="Acacia_nilotica", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
#cross_plot suivant Type_Sol
cross_plot_Faidherbia_10<-cross_plot(Base_Espece_df, input="Type_Sol", target="Faidherbia_albida",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
# Base_Espece_df$Type_Sol_groupe=equal_freq(var=Base_Espece_df$Type_Sol, n_bins = 3)  
# summary(Base_Espece_df$Type_Sol_groupe) 
#cross_plot_Faidherbia_3<-cross_plot(Base_Espece_df, input="Type_Sol_groupe", target="Faidherbia_albida", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
###???
cross_plot_Balanites_10<-cross_plot(Base_Espece_df, input="Type_Sol", target="Balanites_aegyptiaca",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
#cross_plot_Balanites_3<-cross_plot(Base_Espece_df, input="Type_Sol_groupe", target="Balanites_aegyptiaca", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Anogeissus_10<-cross_plot(Base_Espece_df, input="Type_Sol", target="Anogeissus_leiocarpus",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
#cross_plot_Anogeissus_3<-cross_plot(Base_Espece_df, input="Type_Sol_groupe", target="Anogeissus_leiocarpus", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Adansonia_10<-cross_plot(Base_Espece_df, input="Type_Sol", target="Adansonia_digitata",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
#cross_plot_Adansonia_3<-cross_plot(Base_Espece_df, input="Type_Sol_groupe", target="Adansonia_digitata", auto_binning = F,path_out="C:\\Users\\Hp\\OneDrive\\redactions")
###
cross_plot_Acacia_10<-cross_plot(Base_Espece_df, input="Type_Sol", target="Acacia_nilotica",path_out="C:\\Users\\Hp\\OneDrive\\redactions") 
#cross_plot_Acacia_3<-cross_plot(Base_Espece_df, input="Type_Sol", target="Acacia_nilotica")
###
##########################################"
##Type de sol et zone 
library(ggplot2)
install.packages("ggmosaic")
library(ggmosaic)
mosaic<-ggplot(data=Base_Espece_df) + 
  geom_mosaic(aes(x=product(Zone,Type_Sol),fill=Zone, conds=product(Faidherbia_albida)),na.rm=T, divider=mosaic("v"))
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\mosaic.png",mosaic)
