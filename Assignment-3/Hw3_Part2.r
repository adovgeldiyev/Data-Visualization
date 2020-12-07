#Hw 3
# Azat Dovgeldiyev
#Part 2

library(tidyverse)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(ggforce)

messier_data <- read.csv(file = 'MessierData.csv', header = TRUE, sep = ",")
head(messier_data)
messier_data<-drop_na(messier)

#a
distancePlot<-ggplot(messier_data, aes(x=Messier..,y=Distance..LY.))

distancePlot+geom_point()+scale_y_log10()+ggtitle("log10 distance in Messier")+xlim(0,100)

#b
ggplot(messier_data, aes(x=Messier..,y=Distance..LY., color=Kind))+
  geom_point()+scale_y_log10()+ggtitle("log10 distance in Messier")+xlim(0,100)+ylab("Distance L.Y in Log(10)")
#c
ggplot(messier_data, aes(x=Distance..LY., y=Apparent.Magnitude, color=Apparent.Magnitude))+
  geom_point()+scale_x_log10()+ggtitle("Scatterplot of Apparent Magnitude to log(10) distance")+
  scale_y_continuous(limits = c(0, 12))+xlab("Distance L.Y in Log(10)")
#d
ggplot(messier_data, aes(x=Distance..LY., y=Apparent.Magnitude, color=Kind))+
  geom_point(aes(size = messier_data$Size.....))+scale_x_log10()+ggtitle("Scatterplot of Apparent Magnitude to log(10) distance")+
  scale_y_continuous(limits = c(0, 12))+xlab("Distance L.Y in Log(10)")
