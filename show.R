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
show<-read_csv(file = "show.csv")
show1<-show %>% filter(!is.na(enddate)) #filter out shows that I haven't finished watching

plat<-show1 %>% group_by(platform) %>% tally() #group shows by where I watched it

ggplot(data=plat, aes(x=platform, y=n)) + 
  geom_bar(stat="identity", position = "dodge",
fill=c("#15177A", "#C343E3", "#26D345", "#8E1E32", "#BF3C11", "#22D4DC",
                "#EEE71B", "#0A4F11", "#893490"), color='black') +
  scale_y_continuous(breaks = seq(0, 40, by=5), limits=c(0,40))+
  theme_bw() + ylab("# shows watched by Pallichi") + xlab("Platform\n(Sept'22-Mar'23)")+
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black'),
        axis.text.y=element_text(face="bold", color='black'))


#summary by platform of how many episodes/part of episodes or movies watched in a day
#combining shows I watched by myself or with Michi
plat1<-show1 %>% group_by(platform) %>% select(platform, unitpday) %>% 
  get_summary_stats(unitpday, type = "mean_sd")

ggplot(plat1)+
  geom_bar( aes(x=platform, y=mean), stat="identity",
            fill=c("#15177A", "#C343E3", "#26D345",
                            "#8E1E32", "#BF3C11", "#22D4DC",
                            "#EEE71B", "#0A4F11", "#893490"), color='black', alpha=0.75) +
  geom_errorbar( aes(x=platform, ymin=mean-sd, ymax=mean+sd),
                 width=0.4, colour="black", alpha=0.9, size=0.5)+
  scale_y_continuous(breaks = seq(-1, 11, by=2), limits=c(-1,11))+
  theme_bw() + ylab("Average no. of units watched per day by Pallichi") + 
  xlab("Platform\n(Sept'22-Mar'23)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black'),
        axis.text.y=element_text(face="bold", color='black'))

#summary by platform of how many days it took to watch 1 movie/series
#combining shows I watched by myself or with Michi
plat2<-show1 %>% group_by(platform) %>% select(platform, num_days) %>% 
  get_summary_stats(num_days, type = "mean_sd")
ggplot(plat2)+
  geom_bar( aes(x=platform, y=mean), stat="identity",
            fill=c("#15177A", "#C343E3", "#26D345",
                            "#8E1E32", "#BF3C11", "#22D4DC",
                            "#EEE71B", "#0A4F11", "#893490"), color='black', alpha=0.75) +
                              geom_errorbar( aes(x=platform, ymin=mean-sd, ymax=mean+sd),
                              width=0.4, colour="black", alpha=0.9, size=0.5)+
  scale_y_continuous(breaks = seq(-45, 65, by=5), limits=c(-45,65))+
  theme_bw() + ylab("Average no. of days to watch one show") + 
  xlab("Platform\n(Sept'22-Jan'23)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black'),
        axis.text.y=element_text(face="bold", color='black'))

plat3<-show1 %>% group_by(platform, with) %>% 
                            select(platform, with, num_days) %>% 
  get_summary_stats(num_days, type = "mean_sd")
#shows I watched by myself
onlyp<-plat3 %>% filter(with!="Michi")

ggplot(onlyp)+
  geom_bar( aes(x=platform, y=mean), stat="identity",
            fill=c("#C343E3", "#26D345",
                  "#8E1E32", "#BF3C11", "#22D4DC",
                  "#EEE71B", "#0A4F11", "#893490"), color='black', alpha=0.75) +
                geom_errorbar( aes(x=platform, ymin=mean-sd, ymax=mean+sd),
                width=0.4, colour="black", alpha=0.9, size=0.5)+
  scale_y_continuous(breaks = seq(-25, 60, by=5), limits=c(-25,60))+
  theme_bw() + ylab("Average no. of days to watch one show by Pallabi") + 
  xlab("Platform\n(Sept'22-Jan'23)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black'),
        axis.text.y=element_text(face="bold", color='black'))

#shows I watched with Michi
onlyPM<-plat3 %>% filter(with=="Michi")

ggplot(onlyPM)+
  geom_bar( aes(x=platform, y=mean), stat="identity",
            fill=c("#15177A", "#C343E3", "#26D345",
                            "#8E1E32", "#22D4DC",
                            "#EEE71B"), color='black', alpha=0.75) +
                              geom_errorbar( aes(x=platform, ymin=mean-sd, ymax=mean+sd),
                                             width=0.4, colour="black", alpha=0.9, size=0.5)+
  scale_y_continuous(breaks = seq(-5, 140, by=5), limits=c(-5,140))+
  theme_bw() + ylab("Average no. of days to watch one show by Pallichi") + 
  xlab("Platform\n(Sept'22-Jan'23)") +
  theme(axis.title = element_text(face="bold"),
        axis.text.x=element_text(face="bold", color='black'),
        axis.text.y=element_text(face="bold", color='black'))

#shows per year by movie/show
#shows overall


