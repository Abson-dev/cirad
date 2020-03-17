
rm(list = ls()) #Effacement de toutes les données en mémoire
graphics.off() #Effacement de tous les graphiques en mémoire
source("C:\\Users\\Hp\\OneDrive\\Memoire_ITS4\\Code R\\data_preparation.R")
install.packages("devtools")
library(devtools)
remotes::install_github("rstudio/gt")
library(gt)
# Use `sza` to create a gt table; color
# the `sza` column using the `data_color()`
# function, then, add a footnote to the
# `sza` column label explaining what the
# color scale signifies

################"
etat<-df_stat %>%
  arrange(desc(p_na))
etat<-etat %>%
  gt() %>%
  data_color(
    columns = vars(p_na),
    colors = scales::col_numeric(
      palette = c("green", "red"),
      domain = c(0, 100))
  )%>%
  tab_footnote(
    footnote = md("**en %, du vert(pas de valeur manquante) au rouge(trop de valeurs manquantes)**"),
    locations = cells_column_labels(
      columns = vars(p_na))
  )%>%
  tab_source_note(
    source_note = md("**Source** : Projets SERENA et LYSA(2017-2019), Calcul de l'auteur")
  )
etat
etat %>% gtsave("etat.png", path = "C:\\Users\\Hp\\OneDrive\\redactions")
#####################



tab_espece<-espece %>%
  gt() %>%
  data_color(
    columns = vars(Pourcentage),
    colors = scales::col_numeric(
      palette = c("red", "green", "navyblue"),
      domain = c(0, 100))
  ) %>%
  tab_footnote(
    footnote = md("**en %, du rouge(très faible) au vert(élévé)**"),
    locations = cells_column_labels(
      columns = vars(Pourcentage))
  ) %>%
  tab_source_note(
    source_note = md("**Source** : Projets SERENA et LYSA(2017-2019), Calcul de l'auteur")
  )
tab_espece  
tab_espece <- tab_espece %>%
  tab_options(
    table.background.color = "lightcyan"
  )
tab_espece

webshot::install_phantomjs()
tab_espece %>% gtsave("tab_espece.png", path = "C:\\Users\\Hp\\OneDrive\\redactions")
#################
grub_tab_max<-grubs_max %>%
  gt() %>% 
  tab_footnote(
    footnote = md("**valeur seuil à 5%**"),
    locations = cells_column_labels(
      columns = vars(p_value))
  )%>%
  tab_source_note(
    source_note = md("**Source** : Projets SERENA et LYSA(2017-2019), Calcul de l'auteur")
  ) %>%
  tab_header(
    title = md("Grubbs test for one outlier **(Maximum de NDVI_mean)**"),
    subtitle = md("`alternative hypothesis:` highest value 77717.595833 is an outlier")
  )
grub_tab_max <- grub_tab_max %>%
  tab_options(
    table.background.color = "lightcyan"
  ) 
grub_tab_max
grub_tab_max %>% gtsave("grub_tab_max.png", path = "C:\\Users\\Hp\\OneDrive\\redactions")
###################"
grub_tab_min<-grubs_min %>%
  gt() %>% 
  tab_footnote(
    footnote = md("**valeur seuil à 5%**"),
    locations = cells_column_labels(
      columns = vars(p_value))
  )%>%
  tab_source_note(
    source_note = md("**Source** : Projets SERENA et LYSA(2017-2019), Calcul de l'auteur")
  ) %>%
  tab_header(
    title = md("Grubbs test for one outlier **(Minimum de NDVI_mean)**"),
    subtitle = md("`alternative hypothesis:` lowest value 60969.767857 is an outlier")
  )
grub_tab_min <- grub_tab_min %>%
  tab_options(
    table.background.color = "lightcyan"
  ) 
grub_tab_min
grub_tab_min %>% gtsave("grub_tab_min.png", path = "C:\\Users\\Hp\\OneDrive\\redactions")
####################
grub_zone_tab <-grub_zone %>%
  gt() %>% 
  tab_footnote(
    footnote = md("**le quantile de student au seuil 5% et à n(Effectif)-2 dégrés de liberté**"),
    locations = cells_column_labels(
      columns = vars(t))
  )%>%
  tab_source_note(
    source_note = md("**Source** : Projets SERENA et LYSA(2017-2019), Calcul de l'auteur")
  ) %>%
  tab_header(
    title = md("Grubbs test for one outlier **(per Zone, NDVI_mean)**"))
grub_zone_tab <- grub_zone_tab %>%
  tab_options(
    table.background.color = "lightcyan"
  ) 
grub_zone_tab<-grub_zone_tab %>% 
  tab_footnote(
    footnote = md("**la statistique du test de Grubbs**"),
    locations = cells_column_labels(
      columns = vars(stat))
  )
grub_zone_tab
grub_zone_tab %>% gtsave("grub_zone_tab.png", path = "C:\\Users\\Hp\\OneDrive\\redactions")
