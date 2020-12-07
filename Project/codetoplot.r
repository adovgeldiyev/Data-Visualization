num_data = subset(covid,select = -c(iso_code,continent,location,date))

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

ggplot(covid,aes(x=continent, y=total_deaths, fill=continent)) + 
  geom_bar(stat="identity")+
  expand_limits(y = c(0, NA)) +
  scale_y_continuous(labels =addUnits, limits = c(0, 50000000))