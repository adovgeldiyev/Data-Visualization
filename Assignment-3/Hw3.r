#Hw 3
# Azat Dovgeldiyev
#Part 1

library(tidyverse)
library(ggplot2)
library(dplyr)
library(Hmisc)
library(ggforce)

df <- big_five <- read.csv(file = 'PerceptionExperiment.csv', header = TRUE, sep = ",")
head(df)
summary(df)

df$Test <-as.factor(df$Test)
#creating new error  and abs(error) variables
df$Error <-df$Response - df$TrueValue
df$absError <-abs(df$Error)
#fixing labels with text wrap
levels(df$Test) <- str_wrap( levels(df$Test), width=10 )
levels(df$Test)

#a)
ggplot(data=df, mapping = aes(x=Test, y=Error)) +
  stat_summary(fun.data = mean_sdl, geom='bar') +ylab("Mean Error")+
  ggtitle("Mean Error for Test")

#b)
ggplot(data=df, mapping=aes(x=Test, y=absError)) +geom_boxplot()

#c) filtering Subjects
filtered_sub <- df[df$Subject>55 & df$Subject<74,]
summary(filtered_sub)

filtered_sub$Display <- as.factor(filtered_sub$Display)
ggplot(data=filtered_sub, mapping = aes(x=Display, y=Response, fill=Display))+
  geom_boxplot() + ggtitle("Boxplot of Responses for Subjects")

#d)
filter(df, Test %in% c(553,1104))
vertical_non<-slice(df,553:1104)
summary(vertical_non)
df$Trial<-as.factor(df$Trial)


ggplot(data=vertical_non, mapping = aes(x=Subject, y=Response, color=Trial))+
  geom_jitter()+ggtitle("Responses for Vertical Distance Non-Aligned")

