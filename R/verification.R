library(tidyverse)
library(funModeling)
Base_zone1<- Base_Espece_df %>%
  filter(Zone==1)

#cross_plot(Base_zone1,input="Zone",target = "Faidherbia_albida")
cross_plot(Base_zone1,input="Type_Sol",target = "Faidherbia_albida",path_out="C:\\Users\\Hp\\OneDrive\\redactions\\ok.png")
cross_plot(Base_zone1,input="Type_Sol",target = "Balanites_aegyptiaca")
cross_plot(Base_zone1,input="Type_Sol",target = "Anogeissus_leiocarpus")
cross_plot(Base_zone1,input="Type_Sol",target = "Adansonia_digitata")
cross_plot(Base_zone1,input="Type_Sol",target = "Acacia_nilotica")
Base_zone2<- Base_Espece_df %>%
  filter(Zone==2)
cross_plot(Base_zone2,input="Type_Sol",target = "Faidherbia_albida",plot_type = "percentual")
cross_plot(Base_zone2,input="Type_Sol",target = "Balanites_aegyptiaca",plot_type = "quantity")
cross_plot(Base_zone2,input="Type_Sol",target = "Anogeissus_leiocarpus")
cross_plot(Base_zone2,input="Type_Sol",target = "Adansonia_digitata")
cross_plot(Base_zone2,input="Type_Sol",target = "Acacia_nilotica")
Base_zone3<- Base_Espece_df %>%
  filter(Zone==3)
cross_plot(Base_zone3,input="Type_Sol",target = "Faidherbia_albida")
cross_plot(Base_zone3,input="Type_Sol",target = "Balanites_aegyptiaca")
cross_plot(Base_zone3,input="Type_Sol",target = "Anogeissus_leiocarpus")
cross_plot(Base_zone3,input="Type_Sol",target = "Adansonia_digitata")
cross_plot(Base_zone3,input="Type_Sol",target = "Acacia_nilotica")
Base_zone4<- Base_Espece_df %>%
  filter(Zone==4)
cross_plot(Base_zone4,input="Type_Sol",target = "Faidherbia_albida")
cross_plot(Base_zone4,input="Type_Sol",target = "Balanites_aegyptiaca")
cross_plot(Base_zone4,input="Type_Sol",target = "Anogeissus_leiocarpus")
cross_plot(Base_zone4,input="Type_Sol",target = "Adansonia_digitata")
cross_plot(Base_zone4,input="Type_Sol",target = "Acacia_nilotica")

#Test de Chi2
Ta<-Base_zone4 %>% 
  filter(Type_Sol %in% c("Ferrugineux tropicaux","Hydromorphe"))
Table <- xtabs(~Faidherbia_albida+Type_Sol, data = Ta)
Test <- chisq.test(Table, correct=FALSE) # test de Chi²
Test
