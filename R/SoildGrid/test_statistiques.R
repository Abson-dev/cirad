library(tidyverse)
library(SDMSelect)
library(dplyr)
library(sf)
library(raster)
library(rasterVis)
library(RStoolbox)
library(maptools)
library(rgdal)
library(spdep)
library(ggplot2)
library(ggspatial)
library(blockCV)
library(randomForest)
library(rJava)
library(dismo)
library(ggpubr)
library(funModeling)
library(tidyselect)
library(ggcorrplot)
### Ecological Niche Factor Analysis(ENFA)
library(adehabitatHS)
library(ggarrange)
library(CENFA)
library(questionr)

rm(list = ls()) #Effacement de toutes les données en mémoire
graphics.off() #Effacement de tous les graphiques en mémoire
# import presence-absence species data
filename<-paste0("C:\\Users\\DELLDRAMOMO\\OneDrive\\cirad\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Species<-st_read(filename,quiet = T)
#
Base_Espece<-Species
Base_Espece$Faidherbia_albida<-if_else(Base_Espece$Species =="Faidherbia albida","Présence","Absence")
#Balanites aegyptiaca
Base_Espece$Balanites_aegyptiaca<-if_else(Base_Espece$Species =="Balanites aegyptiaca","Présence","Absence")
#Anogeissus leiocarpus
Base_Espece$Anogeissus_leiocarpus<-if_else(Base_Espece$Species =="Anogeissus leiocarpus","Présence","Absence")
#Adansonia digitata
Base_Espece$Adansonia_digitata<-if_else(Base_Espece$Species =="Adansonia digitata","Présence","Absence")
#Acacia nilotica
Base_Espece$Acacia_nilotica<-if_else(Base_Espece$Species =="Acacia nilotica","Présence","Absence")

Base_Espece_df<-st_drop_geometry(Base_Espece)
# Base_Espece_df$Faidherbia_albida<-as.factor(Base_Espece_df$Faidherbia_albida)
# Base_Espece_df$Balanites_aegyptiaca<-as.factor(Base_Espece_df$Balanites_aegyptiaca)
# Base_Espece_df$Anogeissus_leiocarpus<-as.factor(Base_Espece_df$Anogeissus_leiocarpus)
# Base_Espece_df$Adansonia_digitata<-as.factor(Base_Espece_df$Adansonia_digitata)
# Base_Espece_df$Acacia_nilotica<-as.factor(Base_Espece_df$Acacia_nilotica)

Base_Espece_df<-Base_Espece_df %>%
  rename(Sol=Type_Sol)

####### Chi2 avec a variable sol
Faidherbia_albida<-table(Base_Espece_df$Sol, Base_Espece_df$Faidherbia_albida)
Balanites_aegyptiaca<-table(Base_Espece_df$Sol, Base_Espece_df$Balanites_aegyptiaca)
Anogeissus_leiocarpus<-table(Base_Espece_df$Sol, Base_Espece_df$Anogeissus_leiocarpus)
Adansonia_digitata<-table(Base_Espece_df$Sol, Base_Espece_df$Adansonia_digitata)
Acacia_nilotica<-table(Base_Espece_df$Sol, Base_Espece_df$Acacia_nilotica)
library(xtable)
xtable(lprop(Faidherbia_albida))
chi2<-chisq.test(Faidherbia_albida)
FaidherbiaSol<-data.frame(Statistique=chi2$statistic,P_value=chi2$p.value)
row.names(FaidherbiaSol)<-"Faidherbia"
xtable(chisq.residuals(Faidherbia_albida))
#mosaicplot(Faidherbia_albida)
mosaicplot(Faidherbia_albida,las=3,shade = T)
##???
xtable(lprop(Balanites_aegyptiaca))
chi2<-chisq.test(Balanites_aegyptiaca)
BalanitesSol<-data.frame(Statistique=chi2$statistic,P_value=chi2$p.value)
row.names(BalanitesSol)<-"Balanites"
xtable(chisq.residuals(Balanites_aegyptiaca))
#mosaicplot(Balanites_aegyptiaca)
mosaicplot(Balanites_aegyptiaca,las=3,shade = T)
#######"
xtable(lprop(Anogeissus_leiocarpus))
chi2<-chisq.test(Anogeissus_leiocarpus)
AnogeissusSol<-data.frame(Statistique=chi2$statistic,P_value=chi2$p.value)
row.names(AnogeissusSol)<-"Anogeissus"
xtable(chisq.residuals(Anogeissus_leiocarpus))
#mosaicplot(Anogeissus_leiocarpus)
mosaicplot(Anogeissus_leiocarpus,las=3,shade = T)
#########"
xtable(lprop(Adansonia_digitata))
chi2<-chisq.test(Adansonia_digitata)
AdansoniaSol<-data.frame(Statistique=chi2$statistic,P_value=chi2$p.value)
row.names(AdansoniaSol)<-"Adansonia"
xtable(chisq.residuals(Adansonia_digitata))
#mosaicplot(Adansonia_digitata)
mosaicplot(Adansonia_digitata,las=3,shade = T)
###############
xtable(lprop(Acacia_nilotica))
chi2<-chisq.test(Acacia_nilotica)
AcaciaSol<-data.frame(Statistique=chi2$statistic,P_value=chi2$p.value)
row.names(AcaciaSol)<-"Acacia"
xtable(chisq.residuals(Acacia_nilotica))
#mosaicplot(Acacia_nilotica)
mosaicplot(Acacia_nilotica,las=3,shade = T)
###
Chi2test<-rbind(FaidherbiaSol,BalanitesSol,AnogeissusSol,AdansoniaSol,AcaciaSol)
xtable(Chi2test)
library(ggmosaic)
mf<-ggplot(data = Base_Espece_df) +
  geom_mosaic(aes(x = product(Faidherbia_albida, Sol), fill=Sol), na.rm=TRUE) + 
  xlab("") + ylab("")
ba<-ggplot(data = Base_Espece_df) +
  geom_mosaic(aes(x = product(Balanites_aegyptiaca, Sol), fill=Sol), na.rm=TRUE) + 
  xlab("") + ylab("")

ano<-ggplot(data = Base_Espece_df) +
  geom_mosaic(aes(x = product(Anogeissus_leiocarpus, Sol), fill=Sol), na.rm=TRUE) + 
  xlab("") + ylab("")

ada<-ggplot(data = Base_Espece_df) +
  geom_mosaic(aes(x = product(Adansonia_digitata, Sol), fill=Sol), na.rm=TRUE) + 
  xlab("") + ylab("")

aca<-ggplot(data = Base_Espece_df) +
  geom_mosaic(aes(x = product(Acacia_nilotica, Sol), fill=Sol), na.rm=TRUE) + 
  xlab("") + ylab("")
ggarrange(mf,ba,ano,ada,aca,
          common.legend = T)
####### Chi2 avec a variable zone 
Base_Espece_df$Zone<-as.factor(Base_Espece_df$Zone)
Faidherbia_albida<-table(Base_Espece_df$Zone, Base_Espece_df$Faidherbia_albida)
Balanites_aegyptiaca<-table(Base_Espece_df$Zone, Base_Espece_df$Balanites_aegyptiaca)
Anogeissus_leiocarpus<-table(Base_Espece_df$Zone, Base_Espece_df$Anogeissus_leiocarpus)
Adansonia_digitata<-table(Base_Espece_df$Zone, Base_Espece_df$Adansonia_digitata)
Acacia_nilotica<-table(Base_Espece_df$Zone, Base_Espece_df$Acacia_nilotica)
library(xtable)
xtable(lprop(Faidherbia_albida))
chi2<-chisq.test(Faidherbia_albida)
FaidherbiaZone<-data.frame(Statistique=chi2$statistic,P_value=chi2$p.value)
row.names(FaidherbiaZone)<-"Faidherbia"
xtable(chisq.residuals(Faidherbia_albida))
#mosaicplot(Faidherbia_albida)
mosaicplot(Faidherbia_albida,las=3,shade = T)
##???
xtable(lprop(Balanites_aegyptiaca))
chi2<-chisq.test(Balanites_aegyptiaca)
BalanitesZone<-data.frame(Statistique=chi2$statistic,P_value=chi2$p.value)
row.names(BalanitesZone)<-"Balanites"
xtable(chisq.residuals(Balanites_aegyptiaca))
#mosaicplot(Balanites_aegyptiaca)
mosaicplot(Balanites_aegyptiaca,las=3,shade = T)
#######"
xtable(lprop(Anogeissus_leiocarpus))
chi2<-chisq.test(Anogeissus_leiocarpus)
AnogeissusZone<-data.frame(Statistique=chi2$statistic,P_value=chi2$p.value)
row.names(AnogeissusZone)<-"Anogeissus"
xtable(chisq.residuals(Anogeissus_leiocarpus))
#mosaicplot(Anogeissus_leiocarpus)
mosaicplot(Anogeissus_leiocarpus,las=3,shade = T)
#########"
xtable(lprop(Adansonia_digitata))
chi2<-chisq.test(Adansonia_digitata)
AdansoniaZone<-data.frame(Statistique=chi2$statistic,P_value=chi2$p.value)
row.names(AdansoniaZone)<-"Adansonia"
xtable(chisq.residuals(Adansonia_digitata))
#mosaicplot(Adansonia_digitata)
mosaicplot(Adansonia_digitata,las=3,shade = T)
###############
xtable(lprop(Acacia_nilotica))
chi2<-chisq.test(Acacia_nilotica)
AcaciaZone<-data.frame(Statistique=chi2$statistic,P_value=chi2$p.value)
row.names(AcaciaZone)<-"Acacia"
xtable(chisq.residuals(Acacia_nilotica))
#mosaicplot(Acacia_nilotica)
mosaicplot(Acacia_nilotica,las=3,shade = T)
###
Chi2test<-rbind(FaidherbiaZone,BalanitesZone,AnogeissusZone,AdansoniaZone,AcaciaZone)
xtable(Chi2test)
###########Test G
library(MASS)
loglm(~1+2,Faidherbia_albida)
FaidherbiaSol<-data.frame(Statistique=53.30802,P_value=2.656542e-12)
row.names(FaidherbiaSol)<-"Faidherbia"
loglm(~1+2,Balanites_aegyptiaca)
BalanitesSol<-data.frame(Statistique=201.0388,P_value=0)
row.names(BalanitesSol)<-"Balanites"
loglm(~1+2,Anogeissus_leiocarpus)
AnogeissusSol<-data.frame(Statistique=32.61,P_value=8.292169e-08)
row.names(AnogeissusSol)<-"Anogeissus"
loglm(~1+2,Adansonia_digitata)
AdansoniaSol<-data.frame(Statistique=33.87632,P_value=4.404037e-08)
row.names(AdansoniaSol)<-"Adansonia"
loglm(~1+2,Acacia_nilotica)
AcaciaSol<-data.frame(Statistique=0.1165319,P_value=0.9433990)
row.names(AcaciaSol)<-"Acacia"
Gtest<-rbind(FaidherbiaSol,BalanitesSol,AnogeissusSol,AdansoniaSol,AcaciaSol)
xtable(Gtest)
library(ggmosaic)
mf<-ggplot(data = Base_Espece_df) +
  geom_mosaic(aes(x = product(Faidherbia_albida, Zone), fill=Zone), na.rm=TRUE) + 
  xlab("") + ylab("")
ba<-ggplot(data = Base_Espece_df) +
  geom_mosaic(aes(x = product(Balanites_aegyptiaca, Zone), fill=Zone), na.rm=TRUE) + 
  xlab("") + ylab("")

ano<-ggplot(data = Base_Espece_df) +
  geom_mosaic(aes(x = product(Anogeissus_leiocarpus, Zone), fill=Zone), na.rm=TRUE) + 
  xlab("") + ylab("")

ada<-ggplot(data = Base_Espece_df) +
  geom_mosaic(aes(x = product(Adansonia_digitata, Zone), fill=Zone), na.rm=TRUE) + 
  xlab("") + ylab("")

aca<-ggplot(data = Base_Espece_df) +
  geom_mosaic(aes(x = product(Acacia_nilotica, Zone), fill=Zone), na.rm=TRUE) + 
  xlab("") + ylab("")
ggarrange(mf,ba,ano,ada,aca,
          common.legend = T)
###########Test G
library(MASS)
loglm(~1+2,Faidherbia_albida)
FaidherbiaZone<-data.frame(Statistique=49.66146,P_value=9.431844e-11)
row.names(FaidherbiaZone)<-"Faidherbia"
loglm(~1+2,Balanites_aegyptiaca)
BalanitesZone<-data.frame(Statistique=120.8587,P_value=0)
row.names(BalanitesZone)<-"Balanites"
loglm(~1+2,Balanites_aegyptiaca)
loglm(~1+2,Anogeissus_leiocarpus)
AnogeissusZone<-data.frame(Statistique=38.44637,P_value=2.273681e-08)
row.names(AnogeissusZone)<-"Anogeissus"
loglm(~1+2,Adansonia_digitata)
AdansoniaZone<-data.frame(Statistique=5.095460,P_value=0.1649390)
row.names(AdansoniaZone)<-"Adansonia"
loglm(~1+2,Acacia_nilotica)
AcaciaZone<-data.frame(Statistique=5.376993,P_value=0.1461837)
row.names(AcaciaZone)<-"Acacia"
Gtest<-rbind(FaidherbiaZone,BalanitesZone,AnogeissusZone,AdansoniaZone,AcaciaZone)
xtable(Gtest)
################# Test Exact Fisher

