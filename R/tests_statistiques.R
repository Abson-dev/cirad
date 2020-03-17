#tests statistiques
#tests de détection d'outliers
###### Detection des valeurs atypiques par Calcul des quantiles pour les variables continues 

# Calcul des quantiles NDVI_mean
Qinf_NDVI_mean<-quantile(Species_df$NDVI_mean,0.025,na.rm = T)
Qsup_NDVI_mean<-quantile(Species_df$NDVI_mean,0.975,na.rm = T)
IQR<-Qsup_NDVI_mean-Qinf_NDVI_mean
library(data.table)
Species_df_outliers_detect<-setDT(Species_df)
Base_Q<-Species_df_outliers_detect[NDVI_mean>Qsup_NDVI_mean-1.5*IQR|NDVI_mean<Qsup_NDVI_mean-1.5*IQR]
#determiner les valeurs atypiques au sein de chaque zone
library(dplyr)
Base<-Species_df_outliers_detect%>%
  group_by(Zone)%>%
  mutate(Qinf=quantile(NDVI_mean,0.025,na.rm = T),
         Qsup=quantile(NDVI_mean,0.975,na.rm = T))%>% # puisque nous voulons les valeurs atypiques au sein d'un groupe, il nous faut calculer le Qinf et le Qsup dans chaque groupe
  filter(NDVI_mean>Qsup|NDVI_mean<Qinf)%>%
  ungroup
table(Base$Zone)
aberant_zone_NDVI_mean_S<-Base %>% 
  select(-ID_GPS) %>%
  group_by(Species) %>% 
  summarise(Effectif=n()) 
aberant_zone_NDVI_mean<-as.data.frame(table(Base$Zone))
#Detection des valeurs aberrantes par la methode de hampel
# Calcul de la mediane et de l'ecart moyen median
med<-median(Species_df$NDVI_mean,na.rm=T)
em<-mad(Species_df$NDVI_mean,na.rm=T)

# Calcul de la borne inf et de la borne sup
Qinf<-med-3*em
Qsup<-med+3*em

# Detction des valeurs atypiques
Base_Q<-Species_df[NDVI_mean>Qsup|NDVI_mean<Qinf]
# detection des valeurs atypiques dans les differentes categorie de puissance
Base<-Species_df%>%
  group_by(Zone)%>%
  mutate(med=median(Species_df$NDVI_mean,na.rm=T),
         em=mad(Species_df$NDVI_mean,na.rm=T))%>%
  mutate(Qinf=med-3*em,
         Qsup=med+3*em)%>%
  filter(NDVI_mean>Qsup|NDVI_mean<Qinf)%>%
  ungroup
#Methode du box plot 2 iem cas
names(boxplot(Species_df$NDVI_mean))
# visualisation des valeurs atypiques

out<-boxplot(Species_df$NDVI_mean)$out
b<-Species_df
b %>% filter(NDVI_mean %in% out)
boxplot(b$NDVI_mean~b$Zone)
out2<-boxplot(b$NDVI_mean~b$Zone)$out
out2
length(out2)
names(boxplot(b$NDVI_mean~b$Zone))
#identifier pour chaque valeur atypique l'espèce et la zone
out3 <- boxplot(b$NDVI_mean~b$Zone)$group
out3 

out2<-boxplot(b$NDVI_mean~b$Zone)$stats
out2

Qsup <- out2[5,]
Qinf <- out2[1,]

Zone<-boxplot(b$NDVI_mean~b$Zone)$names

out3 <- data.frame(Qsup,Qinf,Zone)
out3$Zone <- as.numeric(out3$Zone)
glimpse(out3)
b1<-b
b1<-b1 %>% left_join(out3)

ind_atyp_group <- b1 %>% filter(NDVI_mean<Qinf | NDVI_mean>Qsup)
#### Methode par test
# chargement du package
install.packages("outliers")
library(outliers)

# Test de Grubbs
help("grubbs.test")

#-- Tester la plus grande valeur
grubbs.test(b$NDVI_mean)
MOY<-mean(b$NDVI_mean)
#68590.5
MIN<-min(b$NDVI_mean,na.rm = T)
#60969.77
MAX<-max(b$NDVI_mean,na.rm = T)
#77717.6
var(b$NDVI_mean,na.rm = T)
s<-sqrt(var(b$NDVI_mean,na.rm = T))
G_min<-(MOY-MIN)/s
G_max<-(MAX-MOY)/s
# alpha = 5%
t<-qt(0.05,df=9258-2)
statistique<-((9258-1)/sqrt(9258))*sqrt(t*t/(t*t+9258-2))
grubs_max<-data.frame(Moyenne=MOY,Minimum=MIN,Maximum=MAX,ecart_type=s,G_max=G_max,p_value=1,statistique=statistique)
#-- Tester la plus petite valeur
grubbs.test(b$NDVI_mean,opposite=TRUE)
grubs_min<-data.frame(Moyenne=MOY,Minimum=MIN,Maximum=MAX,ecart_type=s,G_min=G_min,p_value=1,statistique=statistique)



#-- Tester pour les zones
#max
grubbs.test(b$NDVI_mean[b$Zone=="1"])
grubbs.test(b$NDVI_mean[b$Zone=="2"])
grubbs.test(b$NDVI_mean[b$Zone=="3"])
grubbs.test(b$NDVI_mean[b$Zone=="4"])
#min
grubbs.test(b$NDVI_mean[b$Zone=="1"],opposite=TRUE)
grubbs.test(b$NDVI_mean[b$Zone=="2"],opposite=TRUE)
grubbs.test(b$NDVI_mean[b$Zone=="3"],opposite=TRUE)
grubbs.test(b$NDVI_mean[b$Zone=="4"],opposite=TRUE)
g<-b %>%
  select(NDVI_mean,Zone) %>%
  group_by(Zone) %>%
  mutate(Moyenne=mean(NDVI_mean,na.rm = T),Minimum=min(NDVI_mean,na.rm = T),Maximum=max(NDVI_mean,na.rm = T),s=sqrt(var(NDVI_mean,na.rm = T)))
ok<-filter(g,Zone=="4")
grub_zone<-data.frame(Zone=c("1","2","3","4"),Moyenne=c(66455.72,66671.79,67815.50,70919.52),Minimum=c(63312.09,61066.11,60969.77,64077.12),Maximum=c(74434.55,72636.1,74189.03,77717.6),ecart_type=c(3588.147,2341.234,2847.443,2281.454
),G_max=c(2.22370,2.54750,2.4042,2.99910),G_min=c(0.87611,2.39430,2.23830,2.97970))
grub_zone$Effectif<-c(193,2646,3048,3371)
grub_zone$t<-c(-1.653,-1.645,-1.645,-1.645)
grub_zone$stat<-c(1.63,1.64,1.644,1.6447)
# ts<---1.645
# e<-3371
# statistique1<-((e-1)/sqrt(e))*sqrt(ts*ts/(ts*ts+e-2))
##-- Tester pour tout les individus

# Recuperer le vecteur compact
mpg_compact <- mpg$hwy[mpg$class=="compact"]

# Tester pour tous les individus 
while (grubbs.test(mpg_compact)$p.value<0.05) {
  mpg_compact <-mpg_compact[mpg_compact!=max(mpg_compact,na.rm=T)] 
}

# affichage des non-aberrants
mpg_compact

# identification des valeurs aberrants de la class compact dans la base mpg
compact_ab <- setdiff(mpg$hwy[mpg$class=="compact"],mpg_compact)

mpg <- mpg %>% mutate(comp_ab=(hwy %in% compact_ab & class=="compact"))

#########

##########"tests graphiques pour les outliers
install.packages("ggpubr")
library(ggpubr)
bp<-ggboxplot(Species_df,x="Zone",y="NDVI_mean",color="Zone")
bp<-bp + theme_grey()
ggsave("bp.png")
jarqueberaTest(x)# tester la normalité de NDVI_mean
install.packages("fBasics")
library(fBasics)
# ksnormTest	Kolmogorov-Smirnov normality test,
# shapiroTest	Shapiro-Wilk's test for normality,
# jarqueberaTest	Jarque--Bera test for normality,
# dagoTest	D'Agostino normality test.


#Jarque-Bera
jarqueberaTest(Species_df$NDVI_mean)

#-- by shapiro
shapiro.test(mpg$hwy)

#-- by kolmogorov smirnov
y=rnorm(length(mpg$hwy))
ks.test((mpg$hwy-mean(mpg$hwy))/sd(mpg$hwy),y)
###############"
#test graphique de normalité
ggplot(Species_df,aes(x=NDVI_mean,y=..density..)) +
  geom_histogram(binwidth = 200,fill="cornsilk",color="grey60",size=.2) + 
  geom_density() + 
  facet_grid(Zone ~ .)
  

