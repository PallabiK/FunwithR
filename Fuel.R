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
library(rstatix)

setwd("~/OneDrive - University of Nebraska-Lincoln/FunwithR/")
fuel<-read_csv(file = "fuel.csv")
fuel<-fuel %>% filter(!is.na(date))

#gas price per gallon over time
ggplot(data=fuel, aes(x=date, y=costpgal)) + geom_point()+theme_bw()+
  scale_y_continuous(breaks = seq(1, 4.5, by=0.5), limits=c(1,4.5)) +
  geom_smooth()+ylab("Cost of gas($)/gallon") +
  xlab("Years (2018-2023)")+
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black'),
        axis.text.y=element_text(face="bold", color='black'))

#gas price per gallon over time by year facet wrap
fuel$year <- format(as.Date(fuel$date, format="%Y-%m-%d"),"%Y")
fuel$daymon<-format(as.Date(fuel$date, format="%Y-%m-%d"), "%m-%d")
ggplot(data=fuel, aes(x=daymon, y=costpgal)) + geom_point()+theme_bw()+
  scale_y_continuous(breaks = seq(1, 4.5, by=0.5), limits=c(1,4.5)) +
  geom_smooth()+ylab("Cost of gas($)/gallon") +
  xlab("Years (2018-2023)")+
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black'),
        axis.text.y=element_text(face="bold", color='black'))+
  facet_wrap(~year)

#gas price by year
g1<-fuel %>% group_by(year) %>% select (year, costpgal)
sumg1<-g1 %>% get_summary_stats(costpgal, type = "mean_sd")
ggplot(sumg1)+
  geom_point( aes(x=year, y=mean), stat="identity", fill="#7C227F", alpha=1) +
  geom_errorbar( aes(x=year, ymin=mean-sd, ymax=mean+sd),
                 width=0.4, colour="black", alpha=0.9, size=0.5)+theme_bw()+
  ylab("Average cost of gas($)/gallon") +
  xlab("Years")+
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black'),
        axis.text.y=element_text(face="bold", color='black'))

costttest<-fuel %>% t_test(costpgal ~ year)

#number of times gas filled up per year
numperyear<-fuel %>% group_by(year) %>% tally()
ggplot(numperyear)+
  geom_bar( aes(x=year, y=n), stat="identity", fill="#7CC999", alpha=1)+theme_bw()+
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black'),
        axis.text.y=element_text(face="bold", color='black'))+
  xlab("Years")+
  ylab("#of times visited gas stations")

#which pumps and how many times
pump<-fuel %>% group_by(gs) %>% tally()
ggplot(pump)+
  geom_bar( aes(x=gs, y=n), stat="identity", fill="#7C227F", alpha=1)+theme_bw()+
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black'),
        axis.text.y=element_text(face="bold", color='black'))+
  xlab("Gas Stations")+
  ylab("#of times visited")

#cost at each gs
costbygs<- fuel %>% group_by(gs) %>% 
  get_summary_stats(costpgal, cost, type = "mean_sd")

costbygscpg<-costbygs %>% filter(variable=="costpgal")
ggplot(costbygscpg)+
  geom_point( aes(x=gs, y=mean), stat="identity", alpha=1) +
  geom_errorbar( aes(x=gs, ymin=mean-sd, ymax=mean+sd),
                 width=0.4, colour="black", alpha=0.9, size=0.5)+theme_bw()+
  ylab("Average cost of gas($)/gallon") +
  xlab("Gas Stations")+
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black'),
        axis.text.y=element_text(face="bold", color='black'))


#where gas filled per year

#average miles per gallon variation by year

#distance traveled per year

#money spent per year on gas

#costpmile per year


