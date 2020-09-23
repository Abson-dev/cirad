library(FactoMineR)
library(factoextra)
library(sf)

###########stat desc
filename<-paste0("C:\\Users\\DELLDRAMOMO\\OneDrive\\cirad\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Spec<-st_read(filename,quiet = T)             
Species_df<-st_drop_geometry(Spec) 
library(funModeling)
library(tidyverse)
df_stat<-df_status(Species_df)
group_by_Species<-Species_df %>% group_by(Species) %>% 
  summarise(Effectif=n()) %>% 
  mutate(Pourcentage=round(Effectif*100/9258,2)) %>%
  arrange(desc(Pourcentage))
###############"
df <- data.frame(
  Espèce = c("Faidherbia albida", "Balanites aegyptiaca","Anogeissus leiocarpus" ,"Adansonia digitata","Acacia nilotica",
             "Azadirachta indica","Diospiros mespiliformis","Borassus aethiopium","Piliostigma reticulatum","Bauhinia rufescens","Ziziphus mauritiana","Autres(52 dont le %< 1%)"),
  Pourcentage = c(41.82, 11, 8.75, 6.19,3.86,3,3,2.68,2.43,2.32,2.32,12.63)
)
df<-df %>% arrange(Espèce,desc(Pourcentage)) %>%
  mutate(lab_ypos = cumsum(Pourcentage) - 0.5 * Pourcentage)
df$Parc<-"Parc à Faidherbia albida"
library(ggpubr)
parcf<-ggbarplot(df, 
          x = "Parc", y = "Pourcentage",
          fill = "Espèce",
          color="Espèce",
          
          label = TRUE, 
          lab.col = "white", 
          lab.pos = "in",
          lab.vjust=2,
          legend="right") + theme_bw() + xlab("")
parcf   
p <- ggplot(data = df, aes(x = Parc, y = Pourcentage)) +
  geom_col(aes(fill = Espèce), width = 0.7)+
  geom_text(aes(y = lab_ypos, label = Pourcentage), color = "white")
p
write.csv(df,file="faidherbiaparc.csv",row.names = FALSE)
# write_excel_csv(df,"faidherbiaparc")
# 
# bp<- ggplot(df, aes(x="", y=Pourcentage, fill=Espèce))+
#   geom_bar(width = 1, stat = "identity")
# bp
# pie <- bp + coord_polar("y", start=0)
# pie
# blank_theme <- theme_minimal()+
#   theme(
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     panel.border = element_blank(),
#     panel.grid=element_blank(),
#     axis.ticks = element_blank(),
#     plot.title=element_text(size=2, face="bold")
#   )
# library(scales)
# pie + scale_fill_brewer("Espèce")  + blank_theme +
#   theme(axis.text.x=element_blank())+
#   geom_text(aes(y = Pourcentage/12 + c(0, cumsum(Pourcentage)[-length(Pourcentage)]), 
#                 label = percent(Pourcentage/100)), size=2)
##########"
group_by_Species_df<-as.data.frame(group_by_Species)

Spec1<-group_by_Species_df %>% 
  filter(Pourcentage>1) 

Spec1<-tibble::column_to_rownames(Spec1,var = "Species")

xtable::xtable(Spec1)
# group_by_Species_top_n<-group_by_Species %>% 
#   top_n(n=5,wt=Effectif) %>%
#   arrange(desc(Pourcentage)) 

Spec2<-group_by_Species_df %>% 
  filter(Pourcentage<=1 & Pourcentage>=0.1) 

Spec2<-tibble::column_to_rownames(Spec2,var = "Species")

xtable::xtable(Spec2)

Spec3<-group_by_Species_df %>% 
  filter(Pourcentage<=0.09) 

Spec3<-tibble::column_to_rownames(Spec3,var = "Species")

rownames(Spec1)
xtable::xtable(Spec3)
####################???
filename<-paste0("C:\\Users\\Hp\\OneDrive\\cirad\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Spec<-st_read(filename,quiet = T) 
extent(Spec)
Spec<-st_drop_geometry(Spec)           
Spec<-Spec %>%
  dplyr::select(Species,Type_Sol)
names(Spec)
Specdata<-as.data.frame(table(Spec))
Specdata<-reshape(Specdata,timevar="Type_Sol",idvar ="Species" ,direction = "wide" ,v.names  = "Freq") %>%
  purrr::set_names(c("Species","Ferrugineux tropicaux","Hydromorphe","Hydromorphe sale")) %>%
 dplyr::filter(Species %in% rownames(Spec1))

# Spec<-st_read(filename,quiet = T)             
# Spec<-st_drop_geometry(Spec) 
# Spec<-Spec %>%
#   dplyr::select(Species,Zone)
# 
# Specdata2<-as.data.frame(table(Spec))
# Specdata2<-reshape(Specdata2,timevar="Zone",idvar ="Species" ,direction = "wide" ,v.names  = "Freq") %>%
#   purrr::set_names(c("Species","Zone1","Zone2","Zone3","Zone4")) %>%
#   dplyr::filter(Species %in% c("Acacia nilotica","Balanites aegyptiaca","Faidherbia albida","Anogeissus leiocarpus","Adansonia digitata"))
# Specdata2<-Specdata2 %>% 
#   dplyr::select(Zone1,Zone2,Zone3,Zone4)
# 
# Specdata<-cbind(Specdata,Specdata2)

Specdata$Species<-plyr::revalue(Specdata$Species,c("Faidherbia albida"="F. albida",
                                                   "Balanites aegyptiaca"="B. aegyptiaca",
                                                   "Anogeissus leiocarpus"="A. leiocarpus",
                                                   "Adansonia digitata"="A. digitata",
                                                   "Acacia nilotica"="A. nilotica",
                                                   "Acacia seyal"="A. seyal",
                                                   "Azadirachta indica"="A. indica",
                                                   "Bauhinia rufescens"="B. rufescens",
                                                   "Borassus aethiopium"="B. aethiopium",
                                                   "Celtis integrifolia"="C. integrifolia",
                                                   "Diospiros mespiliformis"="D. mespiliformis",
                                                   "Piliostigma reticulatum"="P. reticulatum",
                                                   "Prosopis juliflora"="P. juliflora",
                                                   "Sclerocarya birrea"="S. birrea",
                                                   "Tamarindus indica"="T. indica",
                                                   "Ziziphus mauritiana"="Z. mauritiana"))

AFC<-tibble::column_to_rownames(Specdata,var = "Species")
library(questionr)
##Profil ligne
Profil_ligne<-as.data.frame(lprop(as.matrix(AFC)))
Profil_ligne<-reshape(Profil_ligne,timevar="Var2",idvar ="Var1" ,direction = "wide" ,v.names  = "Freq") %>%
  purrr::set_names(c("Species","Ferrugineux tropicaux","Hydromorphe","Hydromorphe sale","Total")) %>% tibble::as.tibble()
Profil_ligne<-tibble::column_to_rownames(Profil_ligne,var = "Species")
xtable::xtable(Profil_ligne)
# #tibble::rownames_to_column(Specdata,var="Species")  
# write.csv2(Specdata,"C:/Users/Hp/OneDrive/cirad/R/AFC.csv")
#AFC <- read.csv2("C:/Users/Hp/OneDrive/cirad/R/AFC.csv",row.names = 2)
View(AFC)
names(AFC)
# AFC<-AFC %>% 
#   dplyr::select(Ferrugineux.tropicaux,Hydromorphe,Hydromorphe.sale)


library("gplots")
dt<-as.table(as.matrix(AFC))
xtable::xtable(dt)
balloonplot(t (dt), main="",show.margins = FALSE,xlab = "",ylab = "")

chisq<-chisq.test(AFC)
chisq
# Pearson's Chi-squared test
# 
# data:  AFC
# X-squared = 413.52, df = 30, p-value < 2.2e-16

# Statistiques de khi2
chi2 <- 413.52
# Degré de liberté
df <- (nrow (AFC) - 1) * (ncol (AFC) - 1)
# p value
pval <- pchisq (chi2, df = df, lower.tail = FALSE)
pval
#5.148535e-69
res.ca <- CA(AFC,graph = FALSE)
eig.val <- as.data.frame(get_eigenvalue (res.ca)) %>% 
  purrr::set_names(c("Valeur propre","Variance","Variance cumulée"))
eig.val

xtable::xtable(eig.val)

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

fviz_screeplot (res.ca,title="", barfill="green",
                barcolor = "green",addlabels = TRUE) + ylab("Pourcentage")


library("corrplot")
row <- get_ca_row(res.ca)
corrplot(row$cos2, is.corr = FALSE)

corrplot(row$contrib, is.corr=FALSE)

corrplot(col.ca$cos2, is.corr=FALSE)
corrplot(col.ca$contrib, is.corr=FALSE)
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
               #select.row=list(contrib=7),
               title="",arrow = c(TRUE, TRUE))

fviz_ca_biplot(res.ca,
               geom=c("point", "text"),
               shape.row=18,
               shape.col=22,
               map="colgap",
               col.col="contrib",
               gradient.cols=c("red","green"),
               col.row="blue",
               repel=TRUE,
               #select.row=list(contrib=7),
               title="")

# Graphe asymétrique des lignes seules

fviz_ca_row(res.ca,
            geom.row="text",
            map="rowprincipal",
            labelsize=6,
            repel=TRUE,
            col.row="coord",
            gradient.cols=c("red","green"),
            title="",arrow = c(TRUE, TRUE))

# Graphe des colonnes seules
fviz_ca_col(res.ca,title="")

# Contribution des lignes au facteur 1
fviz_contrib(res.ca, 
             choice ="col", 
             axes = 1,
             title="(1)",
             ggtheme=theme_gray()) +
  theme(plot.title=element_text(hjust=0.5))
ggpubr::ggarrange(fviz_contrib(res.ca, 
                               choice ="row", 
                               axes = 1,
                               fill="green",
                               color = "green",
                               title="(1)",
                               ggtheme=theme_gray()) +
                    theme(plot.title=element_text(hjust=0.5)),
                  fviz_contrib(res.ca, 
                               choice ="row", 
                               axes = 2,
                               fill="green",
                               color = "green",
                               title="(2)",
                               ggtheme=theme_gray()) +
                    theme(plot.title=element_text(hjust=0.5)))

ggpubr::ggarrange(fviz_contrib(res.ca, 
                               choice ="col", 
                               axes = 1,
                               title="(1)",
                               fill="green",
                               color = "green",
                               ggtheme=theme_gray()) +
                    theme(plot.title=element_text(hjust=0.5)),
                  fviz_contrib(res.ca, 
                               choice ="col", 
                               axes = 2,
                               title="(2)",
                               fill="green",
                               color = "green",
                               ggtheme=theme_gray()) +
                    theme(plot.title=element_text(hjust=0.5)))
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

c<-fviz_cluster(res.hcpc,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = ""
)


fviz_ca_biplot (res.ca,title  = "",
                map = "rowprincipal", arrow = c(TRUE, TRUE),
                repel = TRUE)

fviz_ca_biplot (res.ca,title  = "", map = "colgreen", arrow = c (TRUE, FALSE),
                repel = TRUE)

########################Nioro

filename<-paste0("C:\\Users\\DELLDRAMOMO\\OneDrive\\cirad\\ParcFaidherbia\\Data\\BD_Arbres","\\bd_arbres_nioro.shp")
Spec<-st_read(filename,quiet = T) 
library(raster)
extent(Spec)
Species_df<-st_drop_geometry(Spec) 
library(funModeling)
library(tidyverse)
df_stat<-df_status(Species_df)
group_by_Species2<-Species_df %>% 
  filter(! is.na(Espece))%>%
   group_by(Espece) %>% 
  summarise(Effectif=n()) %>% 
  mutate(Pourcentage=round(Effectif*100/6611,2)) %>%
  arrange(desc(Pourcentage))
write.csv2(group_by_Species2,"Base_Nioro.csv")
write.csv2(group_by_Species,"Base_Niakhar.csv")
############"
df<-group_by_Species %>% 
  select(Espece,Pourcentage) %>% 
  filter(Pourcentage>2) 
df2 <- data.frame(
  Espece = "Autres(51 dont le %< 2%)",
  Pourcentage = 14.2
)
df<-rbind(df,df2)
df<-df %>% 
  arrange(Pourcentage)
write.csv(df,file="cordylaparc.csv",row.names = FALSE)
######
group_by_Species_df<-as.data.frame(group_by_Species) 
Spec1<-group_by_Species_df %>% 
  filter(Pourcentage>1) 

Spec1<-tibble::column_to_rownames(Spec1,var = "Espece")

xtable::xtable(Spec1)
group_by_Species_df<-  tibble::column_to_rownames(group_by_Species_df,var = "Espece")
xtable::xtable(group_by_Species_df)

Spec<-Species_df %>%
  filter(!is.na(Espece) & !is.na(Type_Sol_2)) %>% 
  dplyr::select(Espece,Type_Sol_2)
table(Spec$Type_Sol_2)
names(Spec)
Specdata<-as.data.frame(table(Spec))
Specdata<-reshape(Specdata,timevar="Type_Sol_2",idvar ="Espece" ,direction = "wide" ,v.names  = "Freq") %>%
   filter(Espece %in% rownames(Spec1)) %>% 
  purrr::set_names(c("Espece","lithosols","ferrugineux  tropicaux lessives","hydromorphes","ferrugineux  tropicaux rubefies")) %>% tibble::as.tibble()


Specdata$Espece<-plyr::revalue(Specdata$Espece,c("Faidherbia albida"="F. albida",
                                                   "Anacardium occidentale"="A. occidentale",
                                                   "Anogeissus leiocarpus"="A. leiocarpus",
                                                   "Adansonia digitata"="A. digitata",
                                                   "Acacia nilotica"="A. nilotica",
                                                   "Acacia seyal"="A. seyal",
                                                   "Azadirachta indica"="A. indica",
                                                   "Borassus aethiopium"="B. aethiopium",
                                                   "Cordyla pinnata"="C. pinnata",
                                                 
                                                   "Diospyros mespiliformis"="D. mespiliformis",
                                                   "Piliostigma reticulatum"="P. reticulatum",
                                                   
                                                   "Sclerocarya birrea"="S. birrea",
                                                   "Tamarindus indica"="T. indica",
                                                   "Ziziphus mauritiana"="Z. mauritiana",
                                                 "Eucalyptus camaldulensis
"="E.camaldulensis",
                                                 "Ficus capensis"="F. capensis",
                                                 "Prosopis africana"="P. africana",
                                                 "Terminalia macroptera"="T. macroptera"))


Specdata$Espece<-plyr::revalue(Specdata$Espece,c("Eucalyptus camaldulensis"="E. camaldulensis"))

AFC<-tibble::column_to_rownames(Specdata,var = "Espece")
chisq<-chisq.test(AFC)
chisq

# Pearson's Chi-squared test
# 
# data:  AFC
# X-squared = 317.99, df = 39, p-value < 2.2e-16

# Statistiques de khi2
chi2 <- 317.99
# Degré de liberté
df <- (nrow (AFC) - 1) * (ncol (AFC) - 1)
# p value
pval <- pchisq (chi2, df = df, lower.tail = FALSE)
pval
#1.92932e-45

library(questionr)
Profil_ligne<-as.data.frame(lprop(as.matrix(AFC)))
Profil_ligne<-reshape(Profil_ligne,timevar="Var2",idvar ="Var1" ,direction = "wide" ,v.names  = "Freq") %>%
  purrr::set_names(c("Espece","lithosols","ferrugineux  tropicaux lessives","hydromorphes","ferrugineux  tropicaux rubefies","Total")) %>% tibble::as.tibble()
Profil_ligne<-tibble::column_to_rownames(Profil_ligne,var = "Espece")
xtable::xtable(Profil_ligne)
res.ca <- CA(AFC,graph = FALSE)
eig.val <- as.data.frame(get_eigenvalue (res.ca)) %>% 
  purrr::set_names(c("Valeur propre","Variance","Variance cumulée"))
eig.val

xtable::xtable(eig.val)

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

fviz_screeplot (res.ca,title="", barfill="green",
                barcolor = "green",addlabels = TRUE) + ylab("Pourcentage")


library("corrplot")
row <- get_ca_row(res.ca)
corrplot(row$cos2, is.corr = FALSE)

corrplot(row$contrib, is.corr=FALSE)

corrplot(col.ca$cos2, is.corr=FALSE)
corrplot(col.ca$contrib, is.corr=FALSE)
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
               #select.row=list(contrib=7),
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
             choice ="col", 
             axes = 1,
             title="(1)",
             ggtheme=theme_gray()) +
  theme(plot.title=element_text(hjust=0.5))
ggpubr::ggarrange(fviz_contrib(res.ca, 
                               choice ="row", 
                               axes = 1,
                               fill="green",
                               color = "green",
                               title="(1)",
                               ggtheme=theme_gray()) +
                    theme(plot.title=element_text(hjust=0.5)),
                  fviz_contrib(res.ca, 
                               choice ="row", 
                               axes = 2,
                               fill="green",
                               color = "green",
                               title="(2)",
                               ggtheme=theme_gray()) +
                    theme(plot.title=element_text(hjust=0.5)))

ggpubr::ggarrange(fviz_contrib(res.ca, 
                               choice ="col", 
                               axes = 1,
                               title="(1)",
                               fill="green",
                               color = "green",
                               ggtheme=theme_gray()) +
                    theme(plot.title=element_text(hjust=0.5)),
                  fviz_contrib(res.ca, 
                               choice ="col", 
                               axes = 2,
                               title="(2)",
                               fill="green",
                               color = "green",
                               ggtheme=theme_gray()) +
                    theme(plot.title=element_text(hjust=0.5)))
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


fviz_ca_biplot (res.ca,title  = "",
                map = "rowprincipal", arrow = c(TRUE, TRUE),
                repel = TRUE)

fviz_ca_biplot (res.ca,title  = "", map = "colgreen", arrow = c (TRUE, FALSE),
                repel = TRUE)