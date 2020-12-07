library(tidyverse)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(ggforce)
library(lubridate)
library(corrplot)
library(car)
library(MASS)
library(mapproj)

#Preprocessing
covid <- read.csv(file = 'updatedCovidlatlong.csv', header = TRUE, sep = ",")
head(covid)
summary(covid)

covid$continent <- factor(covid$ï..continent, exclude = NULL)
covid$location <- factor(covid$Location, exclude = NULL)
covid$date <- as.Date(covid$Date,"%m/%d/%Y")
covid <- subset(covid, select = -c(Location,Date,ï..continent) )

num_data = subset(covid,select = -c(Iso.Code,continent,location,date))

spearCor <- cor(num_data, method = 'spearman')
corrplot(spearCor, type = "upper", order="hclust",tl.cex = 0.75,tl.col="black")

library(tidyquant)
covid[, "month"] <- format(covid[,"date"], "%m")
covid$month<-as.factor(covid$month)

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

#label size
levels(covid$continent) <- str_wrap( levels(covid$continent), width=10 )

ggplot(covid,aes(x=continent, y=New.Cases, fill=continent)) + 
  geom_bar(stat="identity")+
  
  scale_y_continuous(labels =addUnits, limits = c(0, 10000000))


summary(covid)
library(scales)
hwplot <- ggplot(covid, aes(x = Life.Expectancy, y = Gdp.Per.Capita ,color =Total.Deaths)) +
  geom_point(size = 2)+ggtitle("Total Deaths of LE to GDP per Capita")
  #scale_y_continuous(labels =addUnits, limits = c(0, 1000))+
  #scale_x_date(date_breaks = "1 month", 
                                   #limits = as.Date(c('2020-02-01', '2020-10-30'), format="%d/%m"),
                                   #date_labels="%b-%Y" )
hwplot +  scale_colour_gradient2(
  low = "white",
  mid = "yellow", # middle color value
  high = "red",
  midpoint = 100.5, # middle data value
  space= "Lab",
  na.value = "grey50"
)+theme_dark()

uspopage_plot <- ggplot(india_df, aes(x = date, y = Total.Deaths, fill = factor(Median.Age))) +
  scale_y_continuous(labels =addUnits, limits = c(0, 75000))+
  scale_x_date(date_breaks = "1 month", 
               limits = as.Date(c('2020-02-01', '2020-10-30'), format="%d/%m"),
               date_labels="%b-%Y" ) +
  theme(axis.text.x = element_text(angle = 90))+
  geom_area()

uspopage_plot+scale_fill_viridis_d()

india_df <- filter(covid,
                   location=="India")
summary(india_df)

hwplot <- ggplot(india_df, aes(x = date, y = New.Cases.P.M ,color =Tot.Deaths.P.M)) +
  geom_point(size = 2)+ 
  scale_y_continuous(labels =addUnits, limits = c(0, 50))+
  scale_x_date(date_breaks = "1 month", 
               limits = as.Date(c('2020-02-01', '2020-10-30'), format="%d/%m"),
               date_labels="%b-%Y" )+ggtitle("New cases in India")

hwplot +  scale_colour_gradient2(
  low = "yellow",
  mid = "red", # middle color value
  high = "black",
  midpoint = 100.5, # middle data value
  space= "Lab",
  na.value = "grey50"
)+theme_dark()
summary(covid)
library(gridExtra)
plt1 <- ggplot(covid, aes(x=date, y=Tot.Deaths.P.M))
plt1 +  scale_y_continuous(labels =addUnits, limits = c(0, 50))+
  scale_x_date(date_breaks = "1 month", 
                    limits = as.Date(c('2020-02-01', '2020-10-30'), format="%d/%m"),
                    date_labels="%b" )+
  stat_bin2d(aes(fill=..density..))+ylab("Total deaths p/m")+ggtitle("density of deaths in each month")

plt2 <- ggplot(covid, aes(x=date, y=Tot.Deaths.P.M))
plt2 +  scale_y_continuous(labels =addUnits, limits = c(0.5, 10.5))+
  scale_x_date(date_breaks = "1 month", 
               limits = as.Date(c('2020-02-01', '2020-10-30'), format="%d/%m"),
               date_labels="%b" )+
  stat_density2d(aes(fill=..density..),
                 geom="raster",
                 contour=FALSE)+ylab("Total deaths p/m")+ggtitle("density of deaths in each month")

plt3<- ggplot(covid, aes(x=Life.Expectancy , y=Gdp.Per.Capita,colour=Tot.Deaths.P.M))
plt3 +
  stat_density2d() +
  geom_point()

library(gganimate)
library(gifski)
library(gapminder)
library(transformr)

p <- ggplot(
  positive, 
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
anim_save("india_cases.gif")


