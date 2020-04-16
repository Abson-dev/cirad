filename<-paste0("C:\\Users\\Hp\\OneDrive\\cirad\\Data\\BD_Arbre","\\arbres_diohine_mai2018_par_Zone_OK_BON.shp")
Species<-st_read(filename,quiet = T)
Base_Espece<-Species
Base_Espece$Faidherbia_albida<-if_else(Base_Espece$Species =="Faidherbia albida","1","0")
Base_Espece$Faidherbia_albida<-as.factor(Base_Espece$Faidherbia_albida)
data_df<-st_drop_geometry(Base_Espece)
Base_Faidherbia_Z<-data_df[,c("xcoord","ycoord","Faidherbia_albida")] 
names(Base_Faidherbia_Z)<-c("lon","lat","Faidherbia")
#Transform data as SpatialPointDataFrame
sp::coordinates(Base_Faidherbia_Z) <-~lon+lat
sp::proj4string(Base_Faidherbia_Z) <-"+proj=longlat +datum=WGS84"
l1<-list.files("F:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\",patt="\\.tif")
l1<-sprintf("F:\\Stage_SDM\\SDM\\Data\\WorldClim\\wc2.0_30s_bio\\%s",l1)
dataFZ<-CovarExtract(x=Base_Faidherbia_Z,cov.paths = l1) # en utilsisant SDMSelect
DataModelFZ<-dataFZ@data
Cor<-DataModelFZ[,c(-1,-7,-12)]
# Loading
library(ggcorrplot)
# Compute a correlation matrix
#data(mtcars)
corr <- round(cor(Cor),1)
head(corr[, 1:6])

# Compute a matrix of correlation p-values
p.mat <- cor_pmat(Cor)
head(p.mat[, 1:4])

# Visualize the correlation matrix
# --------------------------------
# method = "square" (default)
ggcorrplot(corr)

# method = "circle"
ggcorrplot(corr, method = "circle")


# Reordering the correlation matrix
# --------------------------------
# using hierarchical clustering
ggcorrplot(corr, hc.order = TRUE, outline.color = "white")


# Types of correlogram layout
# --------------------------------
# Get the lower triangle
ggcorrplot(corr,
           hc.order = TRUE,
           type = "lower",
           outline.color = "white")


# Change colors and theme
# --------------------------------
# Argument colors
ggcorrplot(
  corr,
  hc.order = TRUE,
  type = "lower",
  outline.color = "white",
  ggtheme = ggplot2::theme_gray,
  colors = c("#6D9EC1", "white", "#E46726")
)


# Add correlation coefficients
# --------------------------------
# argument lab = TRUE
correlation1<-ggcorrplot(corr,
           hc.order = TRUE,
           type = "lower",
           lab = TRUE)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\correlationbio_coef.png",correlation1)

# Add correlation significance level
# --------------------------------
# Argument p.mat
# Barring the no significant coefficient
no_sign<-ggcorrplot(corr,
           hc.order = TRUE,
           type = "lower",
           p.mat = p.mat)
ggsave("C:\\Users\\Hp\\OneDrive\\redactions\\correlationbio_no_sign.png",no_sign)


# Leave blank on no significant coefficient
ggcorrplot(
  corr,
  p.mat = p.mat,
  hc.order = TRUE,
  type = "lower",
  insig = "blank"
)
library(xtable)
summary(DataModelFZ$bio1)
