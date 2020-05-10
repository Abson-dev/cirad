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
##########################
rm(list = ls()) #Effacement de toutes les données en mémoire
graphics.off() #Effacement de tous les graphiques en mémoire

# import presence-absence species data
filename<-paste0("C:\\Users\\Hp\\OneDrive\\cirad\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Species<-st_read(filename,quiet = T)
# import raster data
lsoil<-list.files("D:\\Stage_SDM\\SDM\\Data\\SoilGrids_250m\\",patt="\\.tif")
lsoil<-sprintf("D:\\Stage_SDM\\SDM\\Data\\SoilGrids_250m\\%s",lsoil)
lsoil
AETI<-raster(lsoil[1])
AETI.crop<-crop(AETI,extent(Species))
names(AETI.crop)<-"AETI"
CLYPPT<-raster(lsoil[2])
CLYPPT.crop<-crop(CLYPPT,extent(Species))
names(CLYPPT.crop)<-"CLYPPT"
ORCDRC<-raster(lsoil[3])
ORCDRC.crop<-crop(ORCDRC,extent(Species))
names(ORCDRC.crop)<-"ORCDRC"
PHIHOX<-raster(lsoil[4])
PHIHOX.crop<-crop(PHIHOX,extent(Species))
names(PHIHOX.crop)<-"PHIHOX"
SLTPPT<-raster(lsoil[5])
SLTPPT.crop<-crop(SLTPPT,extent(Species))
names(SLTPPT.crop)<-"SLTPPT"
SNDPPT<-raster(lsoil[6])
SNDPPT.crop<-crop(SNDPPT,extent(Species))
names(SNDPPT.crop)<-"SNDPPT"
NTO<-raster(lsoil[7])
NTO.crop<-crop(NTO,extent(Species))
names(NTO.crop)<-"NTO"
P<-raster(lsoil[8])
P.crop<-crop(P,extent(Species))
names(P.crop)<-"P"
NBWP<-raster(lsoil[9])
NBWP.crop<-crop(NBWP,extent(Species))
names(NBWP.crop)<-"NBWP"
SINT<-raster(lsoil[10])
SINT.crop<-crop(SINT,extent(Species))
names(SINT.crop)<-"SINT"
SOS<-raster(lsoil[11])
SOS.crop<-crop(SOS,extent(Species))
names(SOS.crop)<-"SOS"
#CLYPPT.crop,ORCDRC.crop,PHIHOX.crop,SLTPPT.crop,NTO.crop,P.crop,SNDPPT.crop
AETI<-projectRaster(AETI.crop,P.crop)
CLYPPT<-projectRaster(CLYPPT.crop,P.crop)
#Êextent(CLYPPT)<-extent(P.crop)
ORCDRC<-projectRaster(ORCDRC.crop,P.crop)
#extent(ORCDRC)<-extent(P.crop)
PHIHOX<-projectRaster(PHIHOX.crop,P.crop)
#extent(PHIHOX)<-extent(P.crop)
SLTPPT<-projectRaster(SLTPPT.crop,P.crop)
#extent(SLTPPT)<-extent(P.crop)
NTO<-projectRaster(NTO.crop,P.crop)
#extent(NTO)<-extent(P.crop)
SNDPPT<-projectRaster(SNDPPT.crop,P.crop)
#extent(SNDPPT)<-extent(P.crop)
NBWP<-projectRaster(NBWP.crop,P.crop)
SINT<-projectRaster(SINT.crop,P.crop)
SOS<-projectRaster(SOS.crop,P.crop)
P<-P.crop
#,PHIHOX,SLTPPT,NTO,P,SNDPPT
SoilGrid.crop<-stack(AETI,SINT,SOS,NBWP,CLYPPT,ORCDRC,PHIHOX,SLTPPT,NTO,SNDPPT,P)
SoilGrid.crop
############corrélation
glc <- GLcenfa(x = SoilGrid.crop)
mat.cov<-round(glc@cov,4)
corela<-ggcorrplot(mat.cov,ggtheme = ggplot2::theme_gray,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           colors = c("#6D9EC1", "white", "#E46726")) +
  ggtitle("(a)") + theme(plot.title = element_text(hjust = 0.5, size = 10))
Cor<-DataModelF[,-1]
corr <- round(cor(Cor),1)
# Compute a matrix of correlation p-values
p.mat <- round(cor_pmat(Cor),3)
# Visualize the correlation matrix
# --------------------------------
# method = "square" (default)

# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient
sign<-ggcorrplot(mat.cov,ggtheme = ggplot2::theme_gray,
                    hc.order = TRUE,
                    p.mat = p.mat,
                    colors = c("#6D9EC1", "white", "#E46726")) +
  ggtitle("(b)") + theme(plot.title = element_text(hjust = 0.5, size = 10))
soilgridcorr<-ggarrange(corela,sign,
          common.legend = TRUE)
ggexport(soilgridcorr,filename ="soilgridcorr.png",width = 948, height = 480)
#ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\correlationsoilgrid_no_sign.png",no_sign)
#####################crosplot
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

Base_Espece_df<-st_drop_geometry(Base_Espece)
Base_Espece_df$Faidherbia_albida<-as.factor(Base_Espece_df$Faidherbia_albida)
Base_Espece_df$Balanites_aegyptiaca<-as.factor(Base_Espece_df$Balanites_aegyptiaca)
Base_Espece_df$Anogeissus_leiocarpus<-as.factor(Base_Espece_df$Anogeissus_leiocarpus)
Base_Espece_df$Adansonia_digitata<-as.factor(Base_Espece_df$Adansonia_digitata)
Base_Espece_df$Acacia_nilotica<-as.factor(Base_Espece_df$Acacia_nilotica)
Base_EspeceCROSS<-Base_Espece_df[,18:ncol(Base_Espece_df)]
Base_Faidherbia_Z<-Base_EspeceCROSS[,c("xcoord","ycoord","Faidherbia_albida")] 
names(Base_Faidherbia_Z)<-c("lon","lat","Faidherbia albida")
#Transform data as SpatialPointDataFrame
sp::coordinates(Base_Faidherbia_Z) <-~lon+lat
sp::proj4string(Base_Faidherbia_Z) <-"+proj=longlat +datum=WGS84"
dataF<-CovarExtract(x=Base_Faidherbia_Z,cov.paths = lsoil)
DataModelF<-dataF@data
DataModelF<-DataModelF %>% 
  rename(CLYPPT=af_CLYPPT_T__M_agg30cm_250m_4326,ORCDRC=af_ORCDRC_T__M_agg30cm_250m_4326,PHIHOX=af_PHIHOX_T__M_agg30cm_250m_4326,SLTPPT=af_SLTPPT_T__M_agg30cm_250m_4326,SNDPPT=af_SNDPPT_T__M_agg30cm_250m,NTO=af250m_nutrient_n_m_agg30cm,P=af250m_nutrient_p_t_m_agg30cm)
DataModelF$PHIHOX<-DataModelF$PHIHOX/10

DataModelF<-DataModelF %>% 
  rename(SINT=SINT_mean_2010_2019,AETI=AETI_2009_2019_mean,NBWP=L1_NBWP_2009_2019_mean,SOS=SOS_mean_2010_2019)


DataModelF$CLYPPT3<-equal_freq(var=DataModelF$CLYPPT, n_bins = 3)
DataModelF$ORCDRC3<-equal_freq(var = DataModelF$ORCDRC,n_bins = 3)
DataModelF$PHIHOX3<-equal_freq(var = DataModelF$PHIHOX,n_bins = 3)
DataModelF$SLTPPT3<-equal_freq(var = DataModelF$SLTPPT,n_bins = 3)
DataModelF$SNDPPT3<-equal_freq(var = DataModelF$SNDPPT,n_bins = 3)
DataModelF$NTO3<-equal_freq(var = DataModelF$NTO,n_bins = 3)
DataModelF$P3<-equal_freq(var = DataModelF$P,n_bins = 3)
DataFC<- DataModelF[,c("Faidherbia.albida","CLYPPT3","PHIHOX3","SNDPPT3","P3","ORCDRC3","SLTPPT3","NTO3")]
DataFC <- DataFC %>%
  rename(CLYPPT=CLYPPT3,PHIHOX=PHIHOX3,SNDPPT=SNDPPT3,P=P3,ORCDRC=ORCDRC3,SLTPPT=SLTPPT3,NTO=NTO3)
DataFC$Faidherbia.albida<-if_else(DataFC$Faidherbia.albida =="1","présence","absence")
cross_plot(DataFC, input="CLYPPT",target="Faidherbia.albida", auto_binning = F,plot_type = "both")
a<-as.data.frame(table(DataFC$CLYPPT))
a$Freq<-c(0.473,0.386,0.391)
class(a)
b<-cross_plot(DataFC, input="PHIHOX",target="Faidherbia.albida", auto_binning = F,plot_type = "quantity")
ggarrange(a,b)
#ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\pH.png",g)

#plot(SoilGrid.crop)
Base_Espece<-Species
Base_Espece$Faidherbia_albida<-if_else(Base_Espece$Species =="Faidherbia albida","1","0")
Base_Espece$Faidherbia_albida<-as.factor(Base_Espece$Faidherbia_albida)
data_df<-st_drop_geometry(Base_Espece)
Base_Faidherbia_Z<-data_df[,c("xcoord","ycoord","Faidherbia_albida")] 
names(Base_Faidherbia_Z)<-c("lon","lat","Faidherbia")
Base<-Base_Faidherbia_Z
#Transform data as SpatialPointDataFrame
sp::coordinates(Base_Faidherbia_Z) <-~lon+lat
sp::proj4string(Base_Faidherbia_Z) <-"+proj=longlat +datum=WGS84"
dataF<-CovarExtract(x=Base_Faidherbia_Z,cov.paths = lsoil)
DataModelF<-dataF@data

################crossplot
DataModelF$Faidherbia<-as.factor(DataModelF$Faidherbia)
DataModelF$Azote_G=equal_freq(var=DataModelF$Azote, n_bins = 3)
DataModelF$Organic_CarbonG=equal_freq(var=DataModelF$Organic_Carbon, n_bins = 3)
DataModelF$Phosphore_G=equal_freq(var=DataModelF$Phosphore, n_bins = 3)
DataModelF$Proportion__sableG=equal_freq(var=DataModelF$Proportion__sable, n_bins = 3)
DataModelF$Proportion_argileG=equal_freq(var=DataModelF$Proportion_argile, n_bins = 3)
DataModelF$Proportion_LimonG=equal_freq(var=DataModelF$Proportion_Limon, n_bins = 3)
DataModelF$Soil_PhG=equal_freq(var=DataModelF$Soil_Ph, n_bins = 3)
cross_plot(DataModelF, input=c("Azote_G","Organic_CarbonG","Phosphore_G","Proportion__sableG","Proportion_argileG","Proportion_LimonG"), target="Faidherbia", auto_binning = T,plot_type='percentual')

############???"importance des variables
library(ggRandomForests)
DataRF<-as.data.frame(DataModelF[,1:8])
fpa <- as.factor(DataRF[, 'Faidherbia.albida'])
crf <- randomForest(DataRF[, 2:ncol(DataRF)], fpa)
crf$importance
class(crf)
plot(crf)
varImpPlot(crf)
### Ecological Niche Factor Analysis(ENFA)
library(adehabitatHS)
library(CENFA)
help(CENFA)
??CENFA
DataENFA_F<-DataModelF
hist(DataENFA_F[,2:ncol(DataENFA_F)], type = "l")
## We prepare the data for the ENFA
tab <- DataRF[, 2:ncol(DataRF)]
pr <- as.numeric(DataRF$Faidherbia.albida)
## We then perform the PCA before the ENFA
pc <- dudi.pca(tab, scannf = FALSE)
(enfa1 <- enfa(pc, pr,
               scannf = FALSE))
hist(enfa1)
hist(enfa1, scores = FALSE, type = "l")

## scatterplot
scatter(enfa1)










awt<-SoilGrid.crop
####BlockCV
# investigate spatial autocorrelation in raster covariates
# this helps to choose a suitable size for spatial blocks
range<-spatialAutoRange(rasterLayer = awt, # raster file
                        doParallel = F,
                        showPlots = TRUE)

PA<-Base_Faidherbia_Z
# make a sf object from data.frame
pa_data <- sf::st_as_sf(PA, coords = c("lon", "lat"), crs = raster::crs(awt))
set.seed(1994)
sb <- spatialBlock(speciesData = pa_data,
                   species = "Faidherbia",
                   rasterLayer = awt,
                   theRange = 6128, # size of the blocks
                   k = 5,
                   selection = "random",
                   iteration = 100, # find evenly dispersed folds
                   biomod2Format = TRUE,
                   xOffset = 0, # shift the blocks horizontally
                   yOffset = 0)

sb$plots + geom_sf(data = pa_data, alpha = 0.5)
# species occurrences
Data<-Base_Espece_df[,c("xcoord","ycoord","Faidherbia_albida")] 
dim(Data)
names(Data)<-c("lon","lat","Faidherbia")
# DataSpecies <- Data %>%
#               slice(1:9253)
DataSpecies <- Data
dim(DataSpecies)
#  # the name of studied species
myRespName <- "Faidherbia"
# the presence/absences data for our species
myResp <- as.numeric(DataSpecies[,myRespName])
# the XY coordinates of species data
myRespXY <- DataSpecies[,c("lon","lat")]

#  awt <- stack(awt)
#
#  # 1. Formatting Data
library(biomod2)
myBiomodData2 <- BIOMOD_FormatingData(resp.var = myResp,
                                      expl.var = awt, # explanatory raster data
                                      resp.xy = myRespXY,
                                      resp.name = myRespName,
                                      na.rm = TRUE)
plot(myBiomodData2)
#
#  # 2. Defining the folds for DataSplitTable
#  # note that biomodTable should be used here not folds
DataSplitTable <- sb$biomodTable # use generated folds from spatialBlock in previous section
#  # 3. Defining Models Options using default options.
myBiomodOption <- BIOMOD_ModelingOptions()
#
#  # 4. Model fitting
myBiomodModelOut2 <- BIOMOD_Modeling( myBiomodData2,
                                      models = c('GLM','RF'),
                                      models.options = myBiomodOption,
                                      DataSplitTable = DataSplitTable, # blocking folds
                                      VarImport = 4,
                                      models.eval.meth = c('ROC','TSS'),
                                      
                                      modeling.id="test")



