#packages de manipulation des données

rm(list = ls()) #Effacement de toutes les données en mémoire
graphics.off() #Effacement de tous les graphiques en mémoire
source("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\Code R\\tables_gt.R")
install.packages("tidyverse")
library(tidyverse)

install.packages("fuzzySim")
library(fuzzySim)
install.packages("dismo")
library(dismo)
# worlClim<-getData("worldclim",var="bio", res=10)
# 
# plot(worlClim)
install.packages("raster")
library(raster)
filename<-paste0("D:\\Stage_SDM\\SDM\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
filename
 Species_shape<-shapefile(filename)
 class(Species)
# names(Species)
install.packages("sf")
library(sf)
Species<-st_read(filename,quiet = T)
class(Species)
extent(Species)
names(Species)
Species_df<-st_drop_geometry(Species)
###########Etat de santé de la base
install.packages("fundModeling")
library(funModeling)
df_stat<-df_status(Species_df)
class(df_stat)
#voir la suite dans le fichier tables_gt.R
################"
group_by_Species<-Species_df %>% group_by(Species) %>% 
  summarise(Effectif=n()) %>% 
  mutate(Pourcentage=round(Effectif*100/9258,2)) %>%
  arrange(desc(Pourcentage))
class(group_by_Species)
group_by_Species_df<-as.data.frame(group_by_Species)
class(group_by_Species_df)
Total<-data.frame(Species="Total",Effectif=9258,Pourcentage=100)
class(Total)
espece<-rbind(group_by_Species_df,Total)
#ensuite voir le fichier tables_df.R pour la réprésentation de la table
###########################################
install.packages("ggpubr")
library(ggpubr)
######
group_by_Species_df_1_2<- group_by_Species_df %>%
  filter(Effectif >0 & Effectif <=2)
table(group_by_Species_df_1_2$Effectif)

#Graphiques en points: alternative au graphique en barres quand il y a beaucoup de catégories à représenter.
  ggdotchart(group_by_Species_df_1_2,x="Species",y="Effectif",color="Effectif",sorting = "ascending",add="segments",sort.by.groups=TRUE,ggtheme = theme_pubr())
#################################################
  group_by_Species_df_3_4<- group_by_Species_df %>%
    filter(Effectif >2 & Effectif <=4)
  table(group_by_Species_df_3_4$Effectif)
  #Graphiques en points: alternative au graphique en barres quand il y a beaucoup de catégories à représenter.
  ggdotchart(group_by_Species_df_3_4,x="Species",y="Effectif",color="Effectif",sorting = "ascending",add="segments",sort.by.groups=TRUE,ggtheme = theme_pubr())
  ################################################# 
  #################################################
  group_by_Species_df_5_10<- group_by_Species_df %>%
    filter(Effectif >4 & Effectif <=10)
  table(group_by_Species_df_5_10$Effectif)
  #Graphiques en points: alternative au graphique en barres quand il y a beaucoup de catégories à représenter.
  g1<-ggdotchart(group_by_Species_df_5_10,
                 x="Species",y="Effectif",color="Effectif",
                 sorting = "ascending",add="segments",
                 sort.by.groups=TRUE,ggtheme = theme_pubr())
  #choix de dégradés de couleurs
  g1<-g1 + gradient_color(c("orange","blue","green"))
  #Modifier les titres et étiquettes; fonction ggpart()
  g2<- ggpar(g1,legend.title = "Effectif",legend = "right")
  #################################################
  group_by_Species_df_11_77<- group_by_Species_df %>%
    filter(Effectif >10 & Effectif <=77)
  table(group_by_Species_df_11_77$Effectif)
  #Graphiques en points: alternative au graphique en barres quand il y a beaucoup de catégories à représenter.
  g1<-ggdotchart(group_by_Species_df_11_77,
                 x="Species",y="Effectif",color="Effectif",
                 sorting = "ascending",add="segments",
                 sort.by.groups=TRUE,ggtheme = theme_pubr())
  #choix de dégradés de couleurs
  g1<-g1 + gradient_color(c("orange","blue","green"))
  #Modifier les titres et étiquettes; fonction ggpart()
  g2<- ggpar(g1,legend.title = "Effectif",legend = "right")
  #################################################
  group_by_Species_df_107_215<- group_by_Species_df %>%
    filter(Effectif >77 & Effectif <=215)
  table(group_by_Species_df_107_215$Effectif)
  #Graphiques en points: alternative au graphique en barres quand il y a beaucoup de catégories à représenter.
  g1<-ggdotchart(group_by_Species_df_107_215,
                 x="Species",y="Effectif",color="Effectif",
                 sorting = "ascending",add="segments",
                 sort.by.groups=TRUE,ggtheme = theme_pubr())
  #choix de dégradés de couleurs
  g1<-g1 + gradient_color(c("orange","blue","green"))
  #Modifier les titres et étiquettes; fonction ggpart()
  g2<- ggpar(g1,legend.title = "Effectif",legend = "right")
  g2
  #################################################
  group_by_Species_df_plus_215<- group_by_Species_df %>%
    filter(Effectif >215)
  table(group_by_Species_df_plus_215$Effectif)
  #Graphiques en points: alternative au graphique en barres quand il y a beaucoup de catégories à représenter.
  g1<-ggdotchart(group_by_Species_top_n,
                 x="Species",y="Effectif",color="Effectif",
                 sorting = "ascending",add="segments",
                 sort.by.groups=TRUE,ggtheme = theme_pubr())
  #choix de dégradés de couleurs
  g1<-g1 + gradient_color(c("orange","blue","green"))
  #Modifier les titres et étiquettes; fonction ggpart()
  g2<- ggpar(g1,legend.title = "Effectif",legend = "right")
  g2
  #################################################
##################################################
  # les n(ici n=5) espèces les plus peuplées
group_by_Species_top_n<-group_by_Species %>% 
  top_n(n=5,wt=Effectif) %>%
  arrange(desc(Pourcentage)) 
  
Data_Species<-left_join(group_by_Species_top_n,Species)
Data_Species<-select(Data_Species,-c("pourcentage","Effectif"))
map<-st_as_sf(Data_Species)
ggplot(map) + geom_sf(aes(fill=Zone)) + theme_bw()
####################################################
#####Zone
#nombre d'espèces par zone
zoneS<-table(Species_df$Zone)
zoneS<-as.data.frame(zoneS)
names(zoneS)
zoneS<-zoneS %>%
  rename(Zone=Var1,Nombre =Freq)
zoneS<- zoneS %>%
  mutate(pourcentage=round(Nombre*100/9258,2))
library(ggplot2)
#Pie chart##############
install.packages("extrafont")
library(extrafont)
loadfonts(device="win")
data_pie <- data.frame(Zones = c("Zone 1", "Zone 2", "Zone 3", "Zone 4"), 
                     values = c(.0208, .2858, .3292, .3641))
data_pie$Zones <- factor(data_pie$Zones, levels = rev(data_pie$Zones))
arbre_zone<-ggplot(data = data_pie, mapping = aes(x = factor(1), y = values, fill = Zones)) +
  geom_bar(width=1, stat = "identity") +
  coord_polar(theta = "y") + 
  scale_fill_brewer(type = "seq",direction = -1, palette= "YlGnBu", guide = F) +
  geom_text(aes(x = c(1.3, 1.3, 1.3, 1.3), 
                y = values/2 + c(0, cumsum(values)[-length(values)]), 
                label=paste(Zones,"\n",values*100, "%")), family = "Consolas") +
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())
library(ggplot2)
#enlévé le polar
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\arbre_zone.png",arbre_zone)
######################################
# les n(ici n=5) espèces les plus peuplées par zone
group_by_Species_top_n<-group_by_Species %>% 
  top_n(n=5,wt=Effectif) %>%
  arrange(desc(Pourcentage))
z<-as.data.frame(table(group_by_Species_top_n$Species))
Data_Species<-left_join(group_by_Species_top_n,Species_df)
#Data_Species<-select(Data_Species,-Pourcentage)
table(Data_Species$Species)
install.packages("magick")
library(magick)
Faidherbia<-image_read('faidherbia_albida.jpg')
image_browse(Faidherbia) 
acacia_nilotica<-image_read('acacia_nilotica.jpg')
adansonia_digitata<-image_read('adansonia_digitata.jpg')
anogeissus_leicarpus<-image_read('anogeissus_leicarpus.jpg')
balanites_aegyptiaca<-image_read('balanites_aegyptiaca.png')
#data(iris)
bp<-ggboxplot(Data_Species,x="Species",y="NDVI_mean",color="Species")
bp<-bp + theme_grey()
bp + theme(axis.title.x = element_blank(),axis.title.x.bottom = element_blank())
plot(Data_Species$NDVI_mean~Data_Species$Species,main="Iris - Longueurs de pétales")
rasterImage(Faidherbia,0.6,7.5,1.4,10.9)
rasterImage(acacia_nilotica,1.6,7.5,2.4,10.9)
rasterImage(adansonia_digitata,2.6,7.5,3.4,10.9)
#################
map<-st_as_sf(Species)
ggplot(map) + geom_sf(aes(fill=Species)) + theme_bw()
# var_not_interest<-c("Author","Comment","Comment_1")
# Species2<-Species %>% select(-var_not_interest)
# names()
##########################################

