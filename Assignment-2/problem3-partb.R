library(ggplot2)
library(tidyverse)
library(scales)
library(lubridate)


pwl<-read.csv(file="PortlandWaterLevel2003.csv", sep = ",",header = T)
head(pwl)
pwl$newDate<- as.Date(pwl$Date,"%m/%d/%Y")
h<-hour(hm(pwl$Time))

p <- ggplot(pwl, aes(x=newDate, y=h))
p+geom_tile(aes(fill=WL))+
  scale_x_date(date_breaks = "1 month",date_labels = "%b")+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  labs(title = "Moving average of water levels")+xlab("Date")+ylab("Hours")

