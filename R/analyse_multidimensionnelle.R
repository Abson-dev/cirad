library(FactoMineR)
library(ade4)
# require(Factoshiny)
# res <- MCAshiny(tea)
##########Base de Faidherbia albida dans les zones avec les variables bioclimatiques
#zone 1
SDMdata_FZ1<-dataFZ1@data
SDMdata_FZ1
Faidherbia_pres<-Base_Faidherbia_Z %>%
  filter(Faidherbia==1)
Faidherbia_pres<-Faidherbia_pres[,1:2]
presvals<-extract(predictors,Faidherbia_pres)
Faidherbia_abs<-Base_Faidherbia_Z %>%
  filter(Faidherbia==0)
Faidherbia_abs<-Faidherbia_abs[,1:2]

set.seed(0)
group<-kfold(Faidherbia_pres,5)
Faidherbia_pres_train<-Faidherbia_pres[group!=1,]
dim(Faidherbia_pres_train)
Faidherbia_pres_test<-Faidherbia_pres[group==1,]
dim(Faidherbia_pres_test)

ext<-extent(-16.53864,-16.35454,14.45461,14.63543)
# class      : Extent 
# xmin       : -16.53864 
# xmax       : -16.35454 
# ymin       : 14.45461 
# ymax       : 14.63543 
set.seed(0)
pred_nf<-predictors

backg<-Faidherbia_abs
colnames(backg)=c("lon","lat")
group<-kfold(backg,5)
backg_train<-backg[group!=1,]
backg_test<-backg[group==1,]
#######bioclim
bcSDMdata_FZ1<-bioclim(pred_nf,Faidherbia_pres_train)
plot(bcSDMdata_FZ1,a=2,b=5,p=0.85)
evalFZ1<-evaluate(Faidherbia_pres_test,backg_test,bcSDMdata_FZ1,pred_nf)
##trouver un seuil
tr<-threshold(evalFZ1,'spec_sens')
pb<-predict(pred_nf,bcSDMdata_FZ1,ext=ext,progress='')
pb
par(mfrow=c(1,2))
plot(pb,main='Bioclim,raw values')
plot(zone_etude1,add=TRUE,border='dark grey')
plot(pb>tr,main='presence/absence')
plot(zone_etude1,add=TRUE, border='dark grey')
points(Faidherbia_pres_train, pch='+')

