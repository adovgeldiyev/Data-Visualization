#Hw 3
# Azat Dovgeldiyev
#Part 2

library(tidyverse)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(ggforce)
library(magrittr)
library(scales)

air_data = read.csv(file = 'AirQuality.csv', header = TRUE, sep = ",")
head(air_data)

aq <- air_data %>%
  drop_na()

aq.qq <- aq %$%
  data.frame(wind=sort(Wind),
             ozone=sort(Ozone),
             solar=sort(Solar.R),
             temp=sort(Temp))
#a
aq.qq %>% ggplot(aes(wind,solar)) + geom_point()+ggtitle("Wind vs Solar Radiation")+
  geom_smooth(method=lm,se=FALSE)

#b
aq.qq <- aq %$%
  data.frame(wind=sort(Wind),
             ozone=sort(Ozone),
             solar=sort(Solar.R),
             temp=rescale(Temp, to=c(0,1)) %>% sort)
aq.qq %>% 
  mutate(wind=(wind-min(wind))/(max(wind)-min(wind))) %>%
  mutate(solar=rescale(solar, to=c(0,1))) %>%
  ggplot(aes(wind,solar)) + 
  geom_point() + 
  geom_abline(slope=1, intercept=0)

#c
aq.qq %>% 
  ggplot(aes(rescale(wind, to=c(0,1)), temp)) + # temp done above
  geom_point() +
  geom_abline(slope=1, intercept=0) +ggtitle("Wind vs temp")

aq.qq %>% 
  ggplot(aes(rescale(ozone, to=c(0,1)), temp)) + # temp done above
  geom_point() +
  geom_abline(slope=1, intercept=0) +ggtitle("Ozone vs temp")

#d
ggplot(aq, aes(sample=Wind))+geom_qq()+geom_qq_line()+ggtitle("QQ plot of Wind")
ggplot(aq, aes(sample=Solar.R))+geom_qq()+geom_qq_line() + ggtitle("QQ plot of Solar")
