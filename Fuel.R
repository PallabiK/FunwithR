library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stats)
library (nnet)
library(car)
library(aod)
library(ggfortify)
library(ggpubr)

setwd("~/OneDrive - University of Nebraska-Lincoln/FunwithR/")
fuel<-read_csv(file = "fuel.csv")

#gasprice per gallon over time
ggplot(data=fuel, aes(x=date, y=costpgal)) + geom_point()+theme_bw()+
  scale_y_continuous(breaks = seq(1, 4.5, by=0.5), limits=c(1,4.5)) +geom_smooth()

#gas price by year
fuel$year <- format(as.Date(fuel$date, format="%Y-%m-%d"),"%Y")
g1<-fuel %>% group_by(year) %>% select (year, costpgal)
sumg1<-g1 %>% get_summary_stats(costpgal, type = "mean_sd")
ggplot(sumg1)+
  geom_bar( aes(x=year, y=mean), stat="identity", fill="purple", alpha=0.7) +
  geom_errorbar( aes(x=year, ymin=mean-sd, ymax=mean+sd),
                 width=0.4, colour="black", alpha=0.9, size=0.5)

