library(FactoMineR)
library(factoextra)


filename<-paste0("C:\\Users\\Hp\\OneDrive\\cirad\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Spec<-st_read(filename,quiet = T)             
Spec<-st_drop_geometry(Spec)           
Spec<-Spec %>%
  dplyr::select(Species,Type_Sol)
names(Specdata)
Specdata<-as.data.frame(table(Spec))
Specdata<-reshape(Specdata,timevar="Type_Sol",idvar ="Species" ,direction = "wide" ,v.names  = "Freq") %>%
  set_names(c("Species","Ferrugineux tropicaux","Hydromorphe","Hydromorphe sale")) %>%
 dplyr::filter(Species %in% c("Acacia nilotica","Balanites aegyptiaca","Faidherbia albida","Anogeissus leiocarpus","Adansonia digitata"))
  
write.csv2(Specdata,"C:/Users/Hp/OneDrive/cirad/R/AFC.csv")
AFC <- read.csv2("C:/Users/Hp/OneDrive/cirad/R/AFC.csv",row.names = 2)
View(AFC)
names(AFC)
AFC<-AFC %>% 
  dplyr::select(Ferrugineux.tropicaux,Hydromorphe,Hydromorphe.sale)

res.ca <- CA(AFC,graph = FALSE)

# Résultats des lignes
rows.ca<-get_ca_row(res.ca)
rows.ca
# Cos² des lignes
head(rows.ca$cos2)
# Résultats des colonnes
col.ca<-get_ca_col(res.ca)
col.ca
# Contributions des colonnes
col.ca$contrib


# Graphe des lignes et des colonnes (biplot) à coordonnées symétriques
fviz_ca_biplot(res.ca, 
               repel = TRUE,
               labelsize=4,
               title="")

# Biplot asymétrique et colonnes colorées selon leur contribution

fviz_ca_biplot(res.ca,
               geom=c("point", "text"),
               shape.row=18,
               shape.col=22,
               map="colgap",
               col.col="contrib",
               gradient.cols=c("red","green"),
               col.row="blue",
               repel=TRUE,
               select.row=list(contrib=7),
               title="")

# Graphe asymétrique des lignes seules

fviz_ca_row(res.ca,
            geom.row="text",
            map="rowprincipal",
            labelsize=6,
            repel=TRUE,
            col.row="coord",
            gradient.cols=c("red","green"),
            title="")

# Graphe des colonnes seules
fviz_ca_col(res.ca,title="")

# Contribution des lignes au facteur 1
fviz_contrib(res.ca, 
             choice ="row", 
             axes = 1,
             title="Contributions des lignes à F1",
             ggtheme=theme_gray()) +
  theme(plot.title=element_text(hjust=0.5))
# Remarque : la ligne rouge pointillée correspond		
# à la valeur attendue si les contributions 		
# étaient uniformes.		

res.hcpc <- HCPC(res.ca,nb.clust = 3, graph = FALSE)

fviz_dend(res.hcpc, 
          cex = 0.7,                     # Taille du text
          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = "jco",           # Couleur du rectangle
          labels_track_height = 0.4,      # Augment l'espace pour le texte
          title = ""
)

fviz_cluster(res.hcpc,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = ""
)
