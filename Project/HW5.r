library(tidyverse)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(ggforce)
library(lubridate)
library(corrplot)
library(car)
library(MASS)

df<- read.csv(file = 'updatedCovidlatlong.csv', header = TRUE, sep = ",")
df$continent <- factor(df$ï..continent, exclude = NULL)
df$location <- factor(df$Location, exclude = NULL)
df$date <- as.Date(df$Date,"%m/%d/%Y")
df <- subset(df, select = -c(Location,Date,ï..continent) )
summary(df)
#to make more readable
addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

plt2 <- ggplot(df, aes(x=date, y=Tot.Deaths.P.M))
plt2 +  scale_y_continuous(labels =addUnits, limits = c(0, 25))+
  scale_x_date(date_breaks = "1 month", 
               limits = as.Date(c('2020-02-01', '2020-10-30'), format="%d/%m"),
               date_labels="%b" )+
  stat_density2d(aes(fill=..density..),
                 geom="raster",
                 contour=FALSE)+ylab("Total deaths p/m")+ggtitle("density of deaths in each month")

#will focus specifically in India
india_df<- filter(covid,
                   location=="India")
summary(india_df)
library(gganimate)
library(gifski)
library(gapminder)
library(transformr)

p <- ggplot(
  india_df, 
  aes(x = date, y=New.Cases,colour = 'black')
)+scale_x_date(date_breaks = "1 month", 
               limits = as.Date(c('2020-02-01', '2020-10-30'), format="%d/%m"),
               date_labels="%b" )+
  geom_line(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +scale_y_continuous(labels =addUnits, limits = c(0, 100000))+
  #scale_y_log10() +
  labs(x = "Months in 2020", y = "New Cases")
p
animate_plot<-p + transition_reveal(date)
animate(animate_plot, fps = 10, width = 750, height = 450)

