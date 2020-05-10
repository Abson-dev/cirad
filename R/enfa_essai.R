predictors_enfa <- as(worldclim.crop,'SpatialPixelsDataFrame')
str(predictors_enfa)
#predictors_enfa <- predictors_enfa[!is.na(predictors_enfa$SST), ]
#predictors_enfa <- predictors_enfa[!is.na(predictors_enfa$CHL), ]
pr <- slot(count.points(Species, predictors_enfa), "data")[,1]
pc <- dudi.pca(slot(predictors_enfa, "data"), scannf=FALSE)


enfa <- enfa(pc,pr, scannf = FALSE)
pred <- predict.enfa(enfa, predictors_enfa)


####
  
# enfa_data <- data2enfa(predictors_enfa, Species$geometry)
# pc <- dudi.pca(enfa_data$tab, scannf = FALSE)
# enfa <- enfa(pc, enfa_data$pr, scannf = FALSE)
# ##
# mod1 <- enfa(x = worldclim.crop, s.dat = DataENFA_F, field = "Faidherbia.albida")

###############"
Base_Faidherbia_Z<-Base_Espece_df[,c("xcoord","ycoord","Faidherbia_albida")] 
names(Base_Faidherbia_Z)<-c("lon","lat","Faidherbia.albida")
FaidherbiaPr<- Base_Faidherbia_Z %>%
  filter(Faidherbia.albida ==1)
#Transform data as SpatialPointDataFrame
sp::coordinates(FaidherbiaPr) <-~lon+lat
sp::proj4string(FaidherbiaPr) <-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
predictors_enfa <- as(SoilGrid.crop,'SpatialPixelsDataFrame')
str(predictors_enfa)
predictors_enfa <- predictors_enfa[!is.na(predictors_enfa$AETI), ]
predictors_enfa <- predictors_enfa[!is.na(predictors_enfa$SINT), ]
predictors_enfa <- predictors_enfa[!is.na(predictors_enfa$SOS), ]
predictors_enfa <- predictors_enfa[!is.na(predictors_enfa$NBWP), ]
predictors_enfa <- predictors_enfa[!is.na(predictors_enfa$CLYPPT), ]
predictors_enfa <- predictors_enfa[!is.na(predictors_enfa$ORCDRC), ]
predictors_enfa <- predictors_enfa[!is.na(predictors_enfa$PHIHOX), ]
predictors_enfa <- predictors_enfa[!is.na(predictors_enfa$SLTPPT), ]
predictors_enfa <- predictors_enfa[!is.na(predictors_enfa$NTO), ]
predictors_enfa <- predictors_enfa[!is.na(predictors_enfa$SNDPPT), ]
predictors_enfa <- predictors_enfa[!is.na(predictors_enfa$P), ]

#proj4string(predictors_enfa)<-proj4string(Base_Faidherbia_Z)
pr <- slot(count.points(FaidherbiaPr, predictors_enfa), "data")[,1]
tab<-slot(predictors_enfa, "data")

pc <- dudi.pca(tab, scannf=F)
scatterniche(tab, pr,pts=TRUE)

enfa <- adehabitatHS::enfa(pc,pr, scannf = TRUE)
(enfa1 <- adehabitatHS::enfa(pc, pr,
               scannf = FALSE))
library(adehabitatHS)
adehabitatHS::scatter(enfa1)
(renfa <- randtest(enfa1))
plot(renfa)
extent(SoilGrid.crop)
# xmin       : -16.53797 
# xmax       : -16.35464 
# ymin       : 14.45374 
# ymax       : 14.63499 
extent(FaidherbiaPr)
# xmin       : -16.53817 
# xmax       : -16.35454 
# ymin       : 14.45461 
# ymax       : 14.63513 
extent(Species)
FaidherbiaPr@bbox <-as.matrix(extent(SoilGrid.crop))
glc <- GLcenfa(x = SoilGrid.crop)
FaidherbiaPr@data$Faidherbia.albida<-as.numeric(FaidherbiaPr@data$Faidherbia.albida)
mod.enfa <- enfa(x = SoilGrid.crop, s.dat = FaidherbiaPr, field = "Faidherbia.albida")
scatter(x = mod.enfa, y = glc)
mod.cnfa <- cnfa(x = SoilGrid.crop, s.dat = FaidherbiaPr, field = "Faidherbia.albida")
predictorsbio_enfa<-worldclim.crop
proj4string(predictorsbio_enfa)<-proj4string(Base_Faidherbia_Z)

enfa(x = predictors_enfa, s.dat = Base_Faidherbia_Z, field = "Faidherbia albida")
##########"
#(niche(pc,pr,scannf = FALSE))
image(predictors_enfa)
histniche()
#FANTER
gn<-gnesfa(pc,Focus = pr)
scatterniche(gn$li,pr)
s.arrow(gn$co)
s.arrow(gn$cor)
gn2<-gnesfa(pc,Reference  = pr)
scatterniche(gn2$li,pr,side = "bottom")
s.arrow(gn2$co)
s.arrow(gn2$cor)
(mad<-madifa(pc,pr,scannf = F))

enfa(pc,pr,scannf=F)
#########
data(chamois)
cpi <- slot(count.points(chamois$locs, chamois$map),"data")[,1]
chamois$map

tab <- slot(chamois$map, "data")

## we focus on the distance to ecotone and on the slope,
## after centring and scaling (with the help of a PCA)
scatterniche(dudi.pca(tab[,2:3], scannf=FALSE)$tab, cpi)
scatterniche(dudi.pca(tab[,2:3], scannf=FALSE)$tab, cpi, pts=TRUE)
