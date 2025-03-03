library(tidyverse)
library(dplyr)
library(TSA)
library(ggplot2)
############ *******************TP1***************************** #################

######### lire les donn�es
spot<-read.csv(file="VCYT.csv")
data.frame(spot)

######### calcul des rendements
for(i in 2:252){spot[i-1,8]=log(spot[i,5])-log(spot[i-1,5])}
spot[252,8]=-1.0771950e-3
data.frame(spot[,8])

######## calcul du carr� des rendements
spot_1=spot%>%
  select(Date,V8)%>%
  rename(Rendements="V8")%>%
  mutate(rend_carr�s=Rendements^2)

###### ACF des rendements
spot.ts = ts(spot_1, frequency = 1, start=c(2021, 6, 1))
head(spot.ts)

acf(spot.ts[,2], main="acf des rendements")
acf(spot.ts[,3], main="acf des rendements au carr�")

######## tester la correlation
Box.test(spot.ts[,2], lag=1, type = "Ljung-Box") #les rendements sont non-corr�l�s
Box.test(spot.ts[,3], type = "Ljung-Box") #les rendements sont corr�l�s

######## histogramme des rendements
hist(x=spot.ts[,2])

########## test de la normalit�
shapiro.test(spot.ts[,2]) #### >50 le test devient sensible

library("ggpubr")
# Diagramme de densit�
ggdensity(spot.ts[,2], fill = "lightpink") ## la densit� 
# QQ plot
ggqqplot(spot.ts[,2])
qqnorm(spot.ts[,2])
qqline(spot.ts[,2])