cellPlans = data.frame(
  c("ATT", "Sprint", "Verizon", "ATT", "Sprint",
    "Verizon", "ATT", "Sprint", "Verizon", "ATT",
    "Verizon", "Sprint", "Verizon", "ATT",
    "Verizon", "Sprint", "ATT", "ATT", "Sprint"),
  c(1, 1, 2, 3, 3, 4, 6, 6, 8, 10, 12, 12, 16, 16,
    24, 24, 25, 30, 40),
  c(30, 20, 35, 40, 30, 50, 60, 45, 70, 80, 80, 60,
    90, 90, 110, 80, 110, 135, 100))
names(cellPlans) = c("Company", "DataGB", "Price")
library(dplyr)
library(mosaic)
library(lubridate)
library(ggplot2)
library(tidyquant)
ggplot(cellPlans, aes(x=DataGB, y=Price, color=Company,shape = factor(Company))) + 
  geom_point(aes(size=DataGB))+ggtitle("Cellphone companies with plans")+
  scale_x_continuous(name="Data GB", breaks = seq(0,45,5)) +
  scale_y_continuous(name="Price",breaks = seq(0,140,20))
