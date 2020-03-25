library(FactoMineR)
library(ade4)
# require(Factoshiny)
# res <- MCAshiny(tea)
##########Base de Faidherbia albida dans les zones avec les variables bioclimatiques
#zone 1
BaseAFC_FZ1<-map1
BaseAFC_FZ1<-st_drop_geometry(BaseAFC_FZ1)
BaseAFC_FZ1<-as.data.frame(BaseAFC_FZ1)
dim(BaseAFC_FZ1)
res.mcaFZ1 <- MCA(BaseAFC_FZ1,quanti.sup=2,quali.sup=1,graph = FALSE)
data(tea)
res.mca <- MCA(tea,quanti.sup=19,quali.sup=20:36)
#############"
Bcross<-BaseAFC_FZ1
Bcross$bio6_G=equal_freq(var=Bcross$bio6, n_bins = 2)
cross_plot(Bcross,input="bio6_G",target = "Faidherbia")
