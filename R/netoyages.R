#Netoyage des données
install.packages("funModeling")
library(funModeling)
Base_N<-Species_df
etat_sante<-df_status(Base_N)
#convertire en factore les zones
Base_N$Zone<-as.factor(Base_N$Zone)
Species_df<-Base_N
names(Species_df)

