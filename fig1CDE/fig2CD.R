library(treeio)
library(ggtree)
library(ggplot2)
library(lubridate)
rm(list = ls())
Sys.setlocale("LC_TIME", "English") #print in english

###########A#########
data_seq<-read.csv("\\final_seqs_Eurasia_beast_mafft_del_ml.csv")
c1=rgb(137,205,240, max = 255)
c2=rgb(75,134,197, max = 255)
c3=rgb(241,100,74, max = 255)
c4=rgb(162,98,172, max = 255)
c5=rgb(132,192,119, max = 255)
c6=rgb(244,132,84, max = 255)
c7=rgb(255,193,132, max = 255)
c8=rgb(159,131,166, max = 255)

cols<-c('Wild'=c1,'EuropeDomestic'=c2,
        'ChinaDomestic'=c3,'VietnamDomestic'=c4,'JapanDomestic'=c5,
        'KoreaDomestic'=c6,'BangladeshDomestic'=c7,'IndonesiaDomestic'=c8)

tree<-read.beast("\\final_seqs_Eurasia_beast_dta_mcc.txt")
groupfile1<- data_seq[,c(1,2,5)]
groupInfo1 <- split(row.names(groupfile1), groupfile1$Country)
tree1 <- groupOTU(tree, groupInfo1)


dateRanges <- data.frame(
  start = seq(1988.5, 2022.5, 2),
  end = seq(1989.5, 2023.5, 2)
)

p1 <-ggtree(tree1,ladderize=TRUE,aes(color=Host),
            mrsd = "2023-1-25",size=0.5,alpha=1) + 
  geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("lightgray"))+
  theme_tree2() +
  labs(x="Time")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12))+
  theme(legend.position = c(0.15,0.8),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size = 12),
        #axis.line.x = element_line(size=1.1),
        #axis.ticks.x = element_line(size=1.1)
        plot.margin = unit(c(1,0,1,0), "lines")) +
  scale_color_manual(values = cols) + 
  #scale_fill_manual(values = cols) +
  scale_x_continuous(limits = c(1988,2024), breaks=seq(1990,2024,3),expand = c(0,0))+
  scale_y_continuous(expand = c(0.01,0.01)) +
  geom_rootedge(rootedge = 0.1)+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,0,1), "lines"))

p1 <- p1+geom_tippoint(aes(fill=Host),size=1,shape=19,alpha=0.5)+
  scale_fill_manual(values = cols)+ 
  theme(aspect.ratio= 3/5)
p1

p <- ggplot(data=data_seq, aes(x = date, y =distance)) +
  geom_smooth(method = "lm", se=FALSE, 
              color="black", formula = y ~ x, linetype = 2)+
  geom_point(aes(x = date, y =distance,fill = Country),size = 2, alpha = 0.6, shape = 21)+
  theme_minimal()+
  theme_bw()+
  scale_fill_manual(values = cols)+
  labs(x="Time", y="Genetic divergence")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        axis.line.y = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12),
        legend.title=element_blank(),
        #legend.position=c(0.15,0.7),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black",size = 0))+
  scale_x_continuous(limits = c(1995,2025), breaks=seq(1995,2025,10))+
  #scale_y_continuous(limits = c(0,0.065), breaks=seq(0,0.065,0.02))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"),
        legend.key.width = unit(1.1,"cm"),
        legend.key.height = unit(0.8,"cm"))
p


################################################################################
library(readxl)
library(ggplot2)
library(reshape2)
library(scales)
library(metR)
library(writexl)
library(Matrix)
library(ggpubr)
library(RColorBrewer)
library(tidyverse)
library(plyr)
library(dplyr)
library(lubridate)
library(zoo)
library(Rmisc)
library(cowplot)
library(dplyr)

c1=rgb(162,98,172, max = 255)
c2=rgb(132,192,119, max = 255)
c3=rgb(244,132,84, max = 255)

Sys.setlocale("LC_TIME", "English") #print in english
df<-read_xlsx("\\final_seqs_Eurasia_beast_dta_combined.jumpHistory_sum.xlsx",sheet = "Sheet1")
df$date <- 2023.0657534246575 - df$time
data <- df[,c(1:3,5)]
colnames(data)[4] <- "time"
data <- subset(data, data$time >= 1996)

cal_period_when <- function(time_num){
  case_when(
    (time_num >=1996 & time_num < 1997) ~ -2,
    (time_num >=1997 & time_num < 1998) ~ -1,
    (time_num >=1998 & time_num < 1999) ~ 0,
    (time_num >=1999 & time_num < 2000) ~ 1,
    (time_num >=2000 & time_num < 2001) ~ 2,
    (time_num >=2001 & time_num < 2002) ~ 3,
    (time_num >=2002 & time_num < 2003) ~ 4,
    (time_num >=2003 & time_num < 2004) ~ 5,
    (time_num >=2004 & time_num < 2005) ~ 6,
    (time_num >=2005 & time_num < 2006) ~ 7,
    (time_num >=2006 & time_num < 2007) ~ 8,
    (time_num >=2007 & time_num < 2008) ~ 9,
    (time_num >=2008 & time_num < 2009) ~ 10,
    (time_num >=2009 & time_num < 2010) ~ 11,
    (time_num >=2010 & time_num < 2011) ~ 12,
    (time_num >=2011 & time_num < 2012) ~ 13,
    (time_num >=2012 & time_num < 2013) ~ 14,
    (time_num >=2013 & time_num < 2014) ~ 15,
    (time_num >=2014 & time_num < 2015) ~ 16,
    (time_num >=2015 & time_num < 2016) ~ 17,
    (time_num >=2016 & time_num < 2017) ~ 18,
    (time_num >=2017 & time_num < 2018) ~ 19,
    (time_num >=2018 & time_num < 2019) ~ 20,
    (time_num >=2019 & time_num < 2020) ~ 21,
    (time_num >=2020 & time_num < 2021) ~ 22,
    (time_num >=2021 & time_num < 2022) ~ 23,
    (time_num >=2022 & time_num < 2023) ~ 24,
    (time_num >=2023 & time_num < 2024) ~ 25
  )
}

data$period = cal_period_when(data$time)
data$jump = 1
colnames(data)[1] <- "treeId"
colnames(data)[2] <- "startLocation"
colnames(data)[3] <- "endLocation"


ChinaToWild <- subset(data,data$startLocation =="ChinaDomestic" & data$endLocation =="Wild")
ChinaToEurope <- subset(data,data$startLocation =="ChinaDomestic"& data$endLocation=="EuropeDomestic")
ChinaToIndonesia <- subset(data,data$startLocation =="ChinaDomestic" & data$endLocation =="IndonesiaDomestic")
ChinaToVietnam <- subset(data,data$startLocation =="ChinaDomestic"& data$endLocation=="VietnamDomestic")
ChinaToBangladesh <- subset(data,data$startLocation =="ChinaDomestic" & data$endLocation =="BangladeshDomestic")
ChinaToKorea <- subset(data,data$startLocation =="ChinaDomestic"& data$endLocation=="KoreaDomestic")
ChinaToJapan <- subset(data,data$startLocation =="ChinaDomestic" & data$endLocation =="JapanDomestic")

EuropeToWild <- subset(data,data$startLocation =="EuropeDomestic"& data$endLocation=="Wild")
EuropeToChina <- subset(data,data$startLocation =="EuropeDomestic" & data$endLocation =="ChinaDomestic")
EuropeToIndonesia <- subset(data,data$startLocation =="EuropeDomestic"& data$endLocation=="IndonesiaDomestic")
EuropeToVietnam <- subset(data,data$startLocation =="EuropeDomestic" & data$endLocation =="VietnamDomestic")
EuropeToBangladesh <- subset(data,data$startLocation =="EuropeDomestic"& data$endLocation=="BangladeshDomestic")
EuropeToKorea <- subset(data,data$startLocation =="EuropeDomestic" & data$endLocation =="KoreaDomestic")
EuropeToJapan <- subset(data,data$startLocation =="EuropeDomestic"& data$endLocation=="JapanDomestic")

IndonesiaToWild <- subset(data,data$startLocation =="IndonesiaDomestic"& data$endLocation=="Wild")
IndonesiaToChina <- subset(data,data$startLocation =="IndonesiaDomestic" & data$endLocation =="ChinaDomestic")
IndonesiaToEurope <- subset(data,data$startLocation =="IndonesiaDomestic"& data$endLocation=="EuropeDomestic")
IndonesiaToVietnam <- subset(data,data$startLocation =="IndonesiaDomestic" & data$endLocation =="VietnamDomestic")
IndonesiaToBangladesh <- subset(data,data$startLocation =="IndonesiaDomestic"& data$endLocation=="BangladeshDomestic")
IndonesiaToKorea <- subset(data,data$startLocation =="IndonesiaDomestic" & data$endLocation =="KoreaDomestic")
IndonesiaToJapan <- subset(data,data$startLocation =="IndonesiaDomestic"& data$endLocation=="JapanDomestic")

VietnamToWild <- subset(data,data$startLocation =="VietnamDomestic"& data$endLocation=="Wild")
VietnamToChina <- subset(data,data$startLocation =="VietnamDomestic" & data$endLocation =="ChinaDomestic")
VietnamToEurope <- subset(data,data$startLocation =="VietnamDomestic"& data$endLocation=="EuropeDomestic")
VietnamToIndonesia <- subset(data,data$startLocation =="VietnamDomestic" & data$endLocation =="IndonesiaDomestic")
VietnamToBangladesh <- subset(data,data$startLocation =="VietnamDomestic"& data$endLocation=="BangladeshDomestic")
VietnamToKorea <- subset(data,data$startLocation =="VietnamDomestic" & data$endLocation =="KoreaDomestic")
VietnamToJapan <- subset(data,data$startLocation =="VietnamDomestic"& data$endLocation=="JapanDomestic")

BangladeshToWild <- subset(data,data$startLocation =="BangladeshDomestic"& data$endLocation=="Wild")
BangladeshToChina <- subset(data,data$startLocation =="BangladeshDomestic" & data$endLocation =="ChinaDomestic")
BangladeshToEurope <- subset(data,data$startLocation =="BangladeshDomestic"& data$endLocation=="EuropeDomestic")
BangladeshToIndonesia <- subset(data,data$startLocation =="BangladeshDomestic" & data$endLocation =="IndonesiaDomestic")
BangladeshToVietnam <- subset(data,data$startLocation =="BangladeshDomestic"& data$endLocation=="VietnamDomestic")
BangladeshToKorea <- subset(data,data$startLocation =="BangladeshDomestic" & data$endLocation =="KoreaDomestic")
BangladeshToJapan <- subset(data,data$startLocation =="BangladeshDomestic"& data$endLocation=="JapanDomestic")

KoreaToWild <- subset(data,data$startLocation =="KoreaDomestic"& data$endLocation=="Wild")
KoreaToChina <- subset(data,data$startLocation =="KoreaDomestic" & data$endLocation =="ChinaDomestic")
KoreaToEurope <- subset(data,data$startLocation =="KoreaDomestic"& data$endLocation=="EuropeDomestic")
KoreaToIndonesia <- subset(data,data$startLocation =="KoreaDomestic" & data$endLocation =="IndonesiaDomestic")
KoreaToVietnam <- subset(data,data$startLocation =="KoreaDomestic"& data$endLocation=="VietnamDomestic")
KoreaToBangladesh <- subset(data,data$startLocation =="KoreaDomestic" & data$endLocation =="BangladeshDomestic")
KoreaToJapan <- subset(data,data$startLocation =="KoreaDomestic"& data$endLocation=="JapanDomestic")

JapanToWild <- subset(data,data$startLocation =="JapanDomestic"& data$endLocation=="Wild")
JapanToChina <- subset(data,data$startLocation =="JapanDomestic" & data$endLocation =="ChinaDomestic")
JapanToEurope <- subset(data,data$startLocation =="JapanDomestic"& data$endLocation=="EuropeDomestic")
JapanToIndonesia <- subset(data,data$startLocation =="JapanDomestic" & data$endLocation =="IndonesiaDomestic")
JapanToVietnam <- subset(data,data$startLocation =="JapanDomestic"& data$endLocation=="VietnamDomestic")
JapanToBangladesh <- subset(data,data$startLocation =="JapanDomestic" & data$endLocation =="BangladeshDomestic")
JapanToKorea <- subset(data,data$startLocation =="JapanDomestic"& data$endLocation=="KoreaDomestic")

WildToChina <- subset(data,data$startLocation =="Wild" & data$endLocation =="ChinaDomestic")
WildToEurope <- subset(data,data$startLocation =="Wild"& data$endLocation=="EuropeDomestic")
WildToIndonesia <- subset(data,data$startLocation =="Wild" & data$endLocation =="IndonesiaDomestic")
WildToVietnam <- subset(data,data$startLocation =="Wild"& data$endLocation=="VietnamDomestic")
WildToBangladesh <- subset(data,data$startLocation =="Wild" & data$endLocation =="BangladeshDomestic")
WildToKorea <- subset(data,data$startLocation =="Wild"& data$endLocation=="KoreaDomestic")
WildToJapan <- subset(data,data$startLocation =="Wild"& data$endLocation=="JapanDomestic")


WildToUnva <- subset(data,data$startLocation =="Wild"& data$endLocation %in% c("JapanDomestic","KoreaDomestic","EuropeDomestic"))
WildToVa <- subset(data,data$startLocation =="Wild"& data$endLocation %in% c("ChinaDomestic","IndonesiaDomestic","VietnamDomestic","BangladeshDomestic"))
WildTo <- subset(data,data$startLocation =="Wild")

###按照treeid和time排列，并且给每一个MJ编号

index<-function(x){return(c(1:length(x)))}

ChinaToWild <- ChinaToWild %>% arrange(ChinaToWild$treeId,ChinaToWild$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(ChinaToWild$time,ChinaToWild$treeId,index))) 
ChinaToEurope <- ChinaToEurope %>% arrange(ChinaToEurope$treeId,ChinaToEurope$time)%>% 
  transform(MarkovJumpsNum=unlist(tapply(ChinaToEurope$time,ChinaToEurope$treeId,index)))
ChinaToIndonesia <- ChinaToIndonesia %>% arrange(ChinaToIndonesia$treeId,ChinaToIndonesia$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(ChinaToIndonesia$time,ChinaToIndonesia$treeId,index))) 
ChinaToVietnam <- ChinaToVietnam %>% arrange(ChinaToVietnam$treeId,ChinaToVietnam$time)%>% 
  transform(MarkovJumpsNum=unlist(tapply(ChinaToVietnam$time,ChinaToVietnam$treeId,index)))
ChinaToBangladesh <- ChinaToBangladesh %>% arrange(ChinaToBangladesh$treeId,ChinaToBangladesh$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(ChinaToBangladesh$time,ChinaToBangladesh$treeId,index))) 
ChinaToKorea <- ChinaToKorea %>% arrange(ChinaToKorea$treeId,ChinaToKorea$time)%>% 
  transform(MarkovJumpsNum=unlist(tapply(ChinaToKorea$time,ChinaToKorea$treeId,index)))
ChinaToJapan <- ChinaToJapan %>% arrange(ChinaToJapan$treeId,ChinaToJapan$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(ChinaToJapan$time,ChinaToJapan$treeId,index))) 

EuropeToChina <- EuropeToChina %>% arrange(EuropeToChina$treeId,EuropeToChina$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(EuropeToChina$time,EuropeToChina$treeId,index)))
EuropeToWild <- EuropeToWild %>% arrange(EuropeToWild$treeId,EuropeToWild$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(EuropeToWild$time,EuropeToWild$treeId,index)))
EuropeToIndonesia <- EuropeToIndonesia %>% arrange(EuropeToIndonesia$treeId,EuropeToIndonesia$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(EuropeToIndonesia$time,EuropeToIndonesia$treeId,index)))
EuropeToVietnam <- EuropeToVietnam %>% arrange(EuropeToVietnam$treeId,EuropeToVietnam$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(EuropeToVietnam$time,EuropeToVietnam$treeId,index)))
EuropeToBangladesh <- EuropeToBangladesh %>% arrange(EuropeToBangladesh$treeId,EuropeToBangladesh$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(EuropeToBangladesh$time,EuropeToBangladesh$treeId,index)))
EuropeToKorea <- EuropeToKorea %>% arrange(EuropeToKorea$treeId,EuropeToKorea$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(EuropeToKorea$time,EuropeToKorea$treeId,index)))
EuropeToJapan <- EuropeToJapan %>% arrange(EuropeToJapan$treeId,EuropeToJapan$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(EuropeToJapan$time,EuropeToJapan$treeId,index)))

IndonesiaToWild <- IndonesiaToWild %>% arrange(IndonesiaToWild$treeId,IndonesiaToWild$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(IndonesiaToWild$time,IndonesiaToWild$treeId,index)))
IndonesiaToChina <- IndonesiaToChina %>% arrange(IndonesiaToChina$treeId,IndonesiaToChina$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(IndonesiaToChina$time,IndonesiaToChina$treeId,index)))
IndonesiaToEurope <- IndonesiaToEurope %>% arrange(IndonesiaToEurope$treeId,IndonesiaToEurope$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(IndonesiaToEurope$time,IndonesiaToEurope$treeId,index)))
IndonesiaToVietnam <- IndonesiaToVietnam %>% arrange(IndonesiaToVietnam$treeId,IndonesiaToVietnam$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(IndonesiaToVietnam$time,IndonesiaToVietnam$treeId,index)))
IndonesiaToBangladesh <- IndonesiaToBangladesh %>% arrange(IndonesiaToBangladesh$treeId,IndonesiaToBangladesh$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(IndonesiaToBangladesh$time,IndonesiaToBangladesh$treeId,index)))
IndonesiaToKorea <- IndonesiaToKorea %>% arrange(IndonesiaToKorea$treeId,IndonesiaToKorea$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(IndonesiaToKorea$time,IndonesiaToKorea$treeId,index)))
IndonesiaToJapan <- IndonesiaToJapan %>% arrange(IndonesiaToJapan$treeId,IndonesiaToJapan$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(IndonesiaToJapan$time,IndonesiaToJapan$treeId,index)))

VietnamToWild <- VietnamToWild %>% arrange(VietnamToWild$treeId,VietnamToWild$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(VietnamToWild$time,VietnamToWild$treeId,index)))
VietnamToChina <- VietnamToChina %>% arrange(VietnamToChina$treeId,VietnamToChina$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(VietnamToChina$time,VietnamToChina$treeId,index)))
VietnamToEurope <- VietnamToEurope %>% arrange(VietnamToEurope$treeId,VietnamToEurope$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(VietnamToEurope$time,VietnamToEurope$treeId,index)))
VietnamToIndonesia <- VietnamToIndonesia %>% arrange(VietnamToIndonesia$treeId,VietnamToIndonesia$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(VietnamToIndonesia$time,VietnamToIndonesia$treeId,index)))
VietnamToBangladesh <- VietnamToBangladesh %>% arrange(VietnamToBangladesh$treeId,VietnamToBangladesh$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(VietnamToBangladesh$time,VietnamToBangladesh$treeId,index)))
VietnamToKorea <- VietnamToKorea %>% arrange(VietnamToKorea$treeId,VietnamToKorea$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(VietnamToKorea$time,VietnamToKorea$treeId,index)))
VietnamToJapan <- VietnamToJapan %>% arrange(VietnamToJapan$treeId,VietnamToJapan$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(VietnamToJapan$time,VietnamToJapan$treeId,index)))

BangladeshToWild <- BangladeshToWild %>% arrange(BangladeshToWild$treeId,BangladeshToWild$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(BangladeshToWild$time,BangladeshToWild$treeId,index)))
BangladeshToChina <- BangladeshToChina %>% arrange(BangladeshToChina$treeId,BangladeshToChina$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(BangladeshToChina$time,BangladeshToChina$treeId,index)))
BangladeshToEurope <- BangladeshToEurope %>% arrange(BangladeshToEurope$treeId,BangladeshToEurope$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(BangladeshToEurope$time,BangladeshToEurope$treeId,index)))
BangladeshToIndonesia <- BangladeshToIndonesia %>% arrange(BangladeshToIndonesia$treeId,BangladeshToIndonesia$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(BangladeshToIndonesia$time,BangladeshToIndonesia$treeId,index)))
BangladeshToVietnam <- BangladeshToVietnam %>% arrange(BangladeshToVietnam$treeId,BangladeshToVietnam$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(BangladeshToVietnam$time,BangladeshToVietnam$treeId,index)))
BangladeshToKorea <- BangladeshToKorea %>% arrange(BangladeshToKorea$treeId,BangladeshToKorea$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(BangladeshToKorea$time,BangladeshToKorea$treeId,index)))
BangladeshToJapan <- BangladeshToJapan %>% arrange(BangladeshToJapan$treeId,BangladeshToJapan$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(BangladeshToJapan$time,BangladeshToJapan$treeId,index)))

KoreaToWild <- KoreaToWild %>% arrange(KoreaToWild$treeId,KoreaToWild$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(KoreaToWild$time,KoreaToWild$treeId,index)))
KoreaToChina <- KoreaToChina %>% arrange(KoreaToChina$treeId,KoreaToChina$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(KoreaToChina$time,KoreaToChina$treeId,index)))
KoreaToEurope <- KoreaToEurope %>% arrange(KoreaToEurope$treeId,KoreaToEurope$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(KoreaToEurope$time,KoreaToEurope$treeId,index)))
KoreaToIndonesia <- KoreaToIndonesia %>% arrange(KoreaToIndonesia$treeId,KoreaToIndonesia$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(KoreaToIndonesia$time,KoreaToIndonesia$treeId,index)))
KoreaToVietnam <- KoreaToVietnam %>% arrange(KoreaToVietnam$treeId,KoreaToVietnam$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(KoreaToVietnam$time,KoreaToVietnam$treeId,index)))
KoreaToBangladesh <- KoreaToBangladesh %>% arrange(KoreaToBangladesh$treeId,KoreaToBangladesh$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(KoreaToBangladesh$time,KoreaToBangladesh$treeId,index)))
KoreaToJapan <- KoreaToJapan %>% arrange(KoreaToJapan$treeId,KoreaToJapan$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(KoreaToJapan$time,KoreaToJapan$treeId,index)))

JapanToWild <- JapanToWild %>% arrange(JapanToWild$treeId,JapanToWild$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(JapanToWild$time,JapanToWild$treeId,index)))
JapanToChina <- JapanToChina %>% arrange(JapanToChina$treeId,JapanToChina$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(JapanToChina$time,JapanToChina$treeId,index)))
JapanToEurope <- JapanToEurope %>% arrange(JapanToEurope$treeId,JapanToEurope$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(JapanToEurope$time,JapanToEurope$treeId,index)))
JapanToIndonesia <- JapanToIndonesia %>% arrange(JapanToIndonesia$treeId,JapanToIndonesia$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(JapanToIndonesia$time,JapanToIndonesia$treeId,index)))
JapanToVietnam <- JapanToVietnam %>% arrange(JapanToVietnam$treeId,JapanToVietnam$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(JapanToVietnam$time,JapanToVietnam$treeId,index)))
JapanToBangladesh <- JapanToBangladesh %>% arrange(JapanToBangladesh$treeId,JapanToBangladesh$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(JapanToBangladesh$time,JapanToBangladesh$treeId,index)))
JapanToKorea <- JapanToKorea %>% arrange(JapanToKorea$treeId,JapanToKorea$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(JapanToKorea$time,JapanToKorea$treeId,index)))

WildToChina <- WildToChina %>% arrange(WildToChina$treeId,WildToChina$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(WildToChina$time,WildToChina$treeId,index)))
WildToEurope <- WildToEurope %>% arrange(WildToEurope$treeId,WildToEurope$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(WildToEurope$time,WildToEurope$treeId,index)))
WildToIndonesia <- WildToIndonesia %>% arrange(WildToIndonesia$treeId,WildToIndonesia$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(WildToIndonesia$time,WildToIndonesia$treeId,index)))
WildToVietnam <- WildToVietnam %>% arrange(WildToVietnam$treeId,WildToVietnam$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(WildToVietnam$time,WildToVietnam$treeId,index)))
WildToBangladesh <- WildToBangladesh %>% arrange(WildToBangladesh$treeId,WildToBangladesh$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(WildToBangladesh$time,WildToBangladesh$treeId,index)))
WildToKorea <- WildToKorea %>% arrange(WildToKorea$treeId,WildToKorea$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(WildToKorea$time,WildToKorea$treeId,index)))
WildToJapan <- WildToJapan %>% arrange(WildToJapan$treeId,WildToJapan$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(WildToJapan$time,WildToJapan$treeId,index)))

WildToUnva <- WildToUnva %>% arrange(WildToUnva$treeId,WildToUnva$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(WildToUnva$time,WildToUnva$treeId,index)))
WildToVa <- WildToVa %>% arrange(WildToVa$treeId,WildToVa$time) %>% 
  transform(MarkovJumpsNum=unlist(tapply(WildToVa$time,WildToVa$treeId,index)))

ChinaToWild$group ="ChinaToWild"
ChinaToEurope$group ="ChinaToEurope"
ChinaToIndonesia$group ="ChinaToIndonesia"
ChinaToVietnam$group ="ChinaToVietnam"
ChinaToBangladesh$group ="ChinaToBangladesh"
ChinaToKorea$group ="ChinaToKorea"
ChinaToJapan$group ="ChinaToJapan"

EuropeToWild$group ="EuropeToWild"
EuropeToChina$group ="EuropeToChina"
EuropeToIndonesia$group ="EuropeToIndonesia"
EuropeToVietnam$group ="EuropeToVietnam"
EuropeToBangladesh$group ="EuropeToBangladesh"
EuropeToKorea$group ="EuropeToKorea"
EuropeToJapan$group ="EuropeToJapan"

IndonesiaToWild$group ="IndonesiaToWild"
IndonesiaToChina$group ="IndonesiaToChina"
IndonesiaToEurope$group ="IndonesiaToEurope"
IndonesiaToVietnam$group ="IndonesiaToVietnam"
IndonesiaToBangladesh$group ="IndonesiaToBangladesh"
IndonesiaToKorea$group ="IndonesiaToKorea"
IndonesiaToJapan$group ="IndonesiaToJapan"

VietnamToWild$group ="VietnamToWild"
VietnamToChina$group ="VietnamToChina"
VietnamToEurope$group ="VietnamToEurope"
VietnamToIndonesia$group ="VietnamToIndonesia"
VietnamToBangladesh$group ="VietnamToBangladesh"
VietnamToKorea$group ="VietnamToKorea"
VietnamToJapan$group ="VietnamToJapan"

BangladeshToWild$group ="BangladeshToWild"
BangladeshToChina$group ="BangladeshToChina"
BangladeshToEurope$group ="BangladeshToEurope"
BangladeshToIndonesia$group ="BangladeshToIndonesia"
BangladeshToVietnam$group ="BangladeshToVietnam"
BangladeshToKorea$group ="BangladeshToKorea"
BangladeshToJapan$group ="BangladeshToJapan"

KoreaToWild$group ="KoreaToWild"
KoreaToChina$group ="KoreaToChina"
KoreaToEurope$group ="KoreaToEurope"
KoreaToIndonesia$group ="KoreaToIndonesia"
KoreaToVietnam$group ="KoreaToVietnam"
KoreaToBangladesh$group ="KoreaToBangladesh"
KoreaToJapan$group ="KoreaToJapan"

JapanToWild$group ="JapanToWild"
JapanToChina$group ="JapanToChina"
JapanToEurope$group ="JapanToEurope"
JapanToIndonesia$group ="JapanToIndonesia"
JapanToVietnam$group ="JapanToVietnam"
JapanToBangladesh$group ="JapanToBangladesh"
JapanToKorea$group ="JapanToKorea"

WildToChina$group ="WildToChina"
WildToEurope$group ="WildToEurope"
WildToIndonesia$group ="WildToIndonesia"
WildToVietnam$group ="WildToVietnam"
WildToBangladesh$group ="WildToBangladesh"
WildToKorea$group ="WildToKorea"
WildToJapan$group ="WildToJapan"

WildToUnva $group ="WildToUnva "
WildToVa $group ="WildToVa "
WildToUnva_max = WildToUnva %>% group_by(treeId) %>% slice_max(MarkovJumpsNum)
WildToVa_max = WildToVa %>% group_by(treeId) %>% slice_max(MarkovJumpsNum)

CI(WildToUnva_max$MarkovJumpsNum, ci=0.95)
CI(WildToVa_max$MarkovJumpsNum)

write.csv(WildToUnva_max,"\\WildToUnva_max.csv")
write.csv(WildToVa_max,"\\WildToVa_max.csv")

#############################################

final_data <- rbind(ChinaToWild,ChinaToEurope,ChinaToIndonesia,ChinaToVietnam,ChinaToBangladesh,ChinaToKorea,ChinaToJapan,
                    EuropeToWild,EuropeToChina,EuropeToIndonesia,EuropeToVietnam,EuropeToBangladesh,EuropeToKorea,EuropeToJapan,
                    IndonesiaToWild,IndonesiaToChina,IndonesiaToEurope,IndonesiaToVietnam,IndonesiaToBangladesh,IndonesiaToKorea,IndonesiaToJapan,
                    VietnamToWild,VietnamToChina,VietnamToEurope,VietnamToIndonesia,VietnamToBangladesh,VietnamToKorea,VietnamToJapan,
                    BangladeshToWild,BangladeshToChina,BangladeshToEurope,BangladeshToIndonesia,BangladeshToVietnam,BangladeshToKorea,BangladeshToJapan,
                    KoreaToWild,KoreaToChina,KoreaToEurope,KoreaToIndonesia,KoreaToVietnam,KoreaToBangladesh,KoreaToJapan,
                    JapanToWild,JapanToChina,JapanToEurope,JapanToIndonesia,JapanToVietnam,JapanToBangladesh,JapanToKorea,
                    WildToChina,WildToEurope,WildToIndonesia,WildToVietnam,WildToBangladesh,WildToKorea,WildToJapan)

final_data <- final_data[,c(1:6,8)]
final_data$period <- final_data$period+2000-2

to_Wild <- subset(final_data,final_data$endLocation == "Wild")
to_Europe <- subset(final_data,final_data$endLocation == "EuropeDomestic")

write.csv(to_Wild, "\\final_data_to_Wild_MJs.csv")
write.csv(to_Europe, "\\final_data_to_Europe_MJs.csv")
###
to_Wild <- read.csv("\\final_data_to_Wild_MJs.csv")
to_Europe <- read.csv("\\final_data_to_Europe_MJs.csv")

df_to_wild <- aggregate(to_Wild$jump,list(to_Wild$treeId,to_Wild$startLocation,to_Wild$period), sum)
df_to_Europe <- aggregate(to_Europe$jump,list(to_Europe$treeId,to_Europe$startLocation,to_Europe$period), sum)

colnames(df_to_wild)[1:4] <- c("treeId","To","period","jumps")
colnames(df_to_Europe)[1:4] <- c("treeId","To","period","jumps")

dff_to_Wild <- aggregate(df_to_wild$jumps,list(df_to_wild$treeId,df_to_wild$period), sum)
dff_to_Europe <- aggregate(df_to_Europe$jumps,list(df_to_Europe$treeId,df_to_Europe$period), sum)

mean_to_wild <- aggregate(dff_to_Wild$x ,list(dff_to_Wild$Group.2 ), mean)
mean_to_Europe <- aggregate(dff_to_Europe$x,list(dff_to_Europe$Group.2), mean)

colnames(mean_to_wild)[1:2] <- c("period","mean")
colnames(mean_to_Europe)[1:2] <- c("period","mean")

p <- ggplot() +
  geom_line(data=mean_to_wild, 
            aes(x=period, y=mean),alpha=1,color = "red")+
  geom_line(data=mean_to_Europe, 
            aes(x=period, y=mean),alpha=1,color = "black")+
  scale_x_continuous(limits = c(1994.5,2023.5), breaks=seq(1996,2023,5),expand = c(0.01,0))+
  scale_y_continuous(limits = c(0,70), breaks=seq(0,70,20),expand = c(0.01,0))+
  labs(x="Time",y="Number of Markov jumps between host")+
  theme_bw()+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        axis.line.y = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12),
        legend.title=element_blank(),
        legend.position = c(0.2,0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,0,1), "lines"),
        legend.key.width = unit(1.1,"cm"),
        legend.key.height = unit(0.8,"cm"))





#############################################
###同一个treeid的一个period里，只取最大的MJs
#a <- P_JapKorToWild %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
#b <- WildToP_JapKor %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
ChinaToWild_filter <- ChinaToWild %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
ChinaToEurope_filter <- ChinaToEurope %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
ChinaToIndonesia_filter <- ChinaToIndonesia %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
ChinaToVietnam_filter <- ChinaToVietnam %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
ChinaToBangladesh_filter <- ChinaToBangladesh %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
ChinaToKorea_filter <- ChinaToKorea %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
ChinaToJapan_filter <- ChinaToJapan %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))

ChinaToWild_peryear <- ChinaToWild %>% group_by(treeId,period) %>% tally()
ChinaToEurope_peryear <- ChinaToEurope %>% group_by(treeId,period) %>% tally()
ChinaToIndonesia_peryear <- ChinaToIndonesia %>% group_by(treeId,period) %>% tally()
ChinaToVietnam_peryear <- ChinaToVietnam %>% group_by(treeId,period) %>% tally()
ChinaToBangladesh_peryear <- ChinaToBangladesh %>% group_by(treeId,period) %>% tally()
ChinaToKorea_peryear <- ChinaToKorea %>% group_by(treeId,period) %>% tally()
ChinaToJapan_peryear <- ChinaToJapan %>% group_by(treeId,period) %>% tally()

ChinaToWild_peryear$period <- ChinaToWild_peryear$period+2000-2
ChinaToEurope_peryear$period <- ChinaToEurope_peryear$period+2000-2
ChinaToIndonesia_peryear$period <- ChinaToIndonesia_peryear$period+2000-2
ChinaToVietnam_peryear$period <- ChinaToVietnam_peryear$period+2000-2
ChinaToBangladesh_peryear$period <- ChinaToBangladesh_peryear$period+2000-2
ChinaToKorea_peryear$period <- ChinaToKorea_peryear$period+2000-2
ChinaToJapan_peryear$period <- ChinaToJapan_peryear$period+2000-2

ChinaToWild_peryear$group <- "ChinaToWild"
ChinaToEurope_peryear$group <- "ChinaToEurope"
ChinaToIndonesia_peryear$group <- "ChinaToIndonesia"
ChinaToVietnam_peryear$group <- "ChinaToVietnam"
ChinaToBangladesh_peryear$group <- "ChinaToBangladesh"
ChinaToKorea_peryear$group <- "ChinaToKorea"
ChinaToJapan_peryear$group <- "ChinaToJapan"
###
EuropeToWild_filter <- EuropeToWild %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
EuropeToChina_filter <- EuropeToChina %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
EuropeToIndonesia_filter <- EuropeToIndonesia %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
EuropeToVietnam_filter <- EuropeToVietnam %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
EuropeToBangladesh_filter <- EuropeToBangladesh %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
EuropeToKorea_filter <- EuropeToKorea %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
EuropeToJapan_filter <- EuropeToJapan %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
#
EuropeToWild_peryear <- EuropeToWild %>% group_by(treeId,period) %>% tally()
EuropeToChina_peryear <- EuropeToChina %>% group_by(treeId,period) %>% tally()
EuropeToIndonesia_peryear <- EuropeToIndonesia %>% group_by(treeId,period) %>% tally()
EuropeToVietnam_peryear <- EuropeToVietnam %>% group_by(treeId,period) %>% tally()
EuropeToBangladesh_peryear <- EuropeToBangladesh %>% group_by(treeId,period) %>% tally()
EuropeToKorea_peryear <- EuropeToKorea %>% group_by(treeId,period) %>% tally()
EuropeToJapan_peryear <- EuropeToJapan %>% group_by(treeId,period) %>% tally()
#
EuropeToWild_peryear$period <- EuropeToWild_peryear$period+2000-2
EuropeToChina_peryear$period <- EuropeToChina_peryear$period+2000-2
EuropeToIndonesia_peryear$period <- EuropeToIndonesia_peryear$period+2000-2
EuropeToVietnam_peryear$period <- EuropeToVietnam_peryear$period+2000-2
EuropeToBangladesh_peryear$period <- EuropeToBangladesh_peryear$period+2000-2
EuropeToKorea_peryear$period <- EuropeToKorea_peryear$period+2000-2
EuropeToJapan_peryear$period <- EuropeToJapan_peryear$period+2000-2
#
EuropeToWild_peryear$group <- "EuropeToWild"
EuropeToChina_peryear$group <- "EuropeToChina"
EuropeToIndonesia_peryear$group <- "EuropeToIndonesia"
EuropeToVietnam_peryear$group <- "EuropeToVietnam"
EuropeToBangladesh_peryear$group <- "EuropeToBangladesh"
EuropeToKorea_peryear$group <- "EuropeToKorea"
EuropeToJapan_peryear$group <- "EuropeToJapan"
###
IndonesiaToWild_filter <- IndonesiaToWild %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
IndonesiaToChina_filter <- IndonesiaToChina %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
IndonesiaToEurope_filter <- IndonesiaToEurope %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
IndonesiaToVietnam_filter <- IndonesiaToVietnam %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
IndonesiaToBangladesh_filter <- IndonesiaToBangladesh %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
IndonesiaToKorea_filter <- IndonesiaToKorea %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
IndonesiaToJapan_filter <- IndonesiaToJapan %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
#
IndonesiaToWild_peryear <- IndonesiaToWild %>% group_by(treeId,period) %>% tally()
IndonesiaToChina_peryear <- IndonesiaToChina %>% group_by(treeId,period) %>% tally()
IndonesiaToEurope_peryear <- IndonesiaToEurope %>% group_by(treeId,period) %>% tally()
IndonesiaToVietnam_peryear <- IndonesiaToVietnam %>% group_by(treeId,period) %>% tally()
IndonesiaToBangladesh_peryear <- IndonesiaToBangladesh %>% group_by(treeId,period) %>% tally()
IndonesiaToKorea_peryear <- IndonesiaToKorea %>% group_by(treeId,period) %>% tally()
IndonesiaToJapan_peryear <- IndonesiaToJapan %>% group_by(treeId,period) %>% tally()
#
IndonesiaToWild_peryear$period <- IndonesiaToWild_peryear$period+2000-2
IndonesiaToChina_peryear$period <- IndonesiaToChina_peryear$period+2000-2
IndonesiaToEurope_peryear$period <- IndonesiaToEurope_peryear$period+2000-2
IndonesiaToVietnam_peryear$period <- IndonesiaToVietnam_peryear$period+2000-2
IndonesiaToBangladesh_peryear$period <- IndonesiaToBangladesh_peryear$period+2000-2
IndonesiaToKorea_peryear$period <- IndonesiaToKorea_peryear$period+2000-2
IndonesiaToJapan_peryear$period <- IndonesiaToJapan_peryear$period+2000-2
#
IndonesiaToWild_peryear$group <- "IndonesiaToWild"
IndonesiaToChina_peryear$group <- "IndonesiaToChina"
IndonesiaToEurope_peryear$group <- "IndonesiaToEurope"
IndonesiaToVietnam_peryear$group <- "IndonesiaToVietnam"
IndonesiaToBangladesh_peryear$group <- "IndonesiaToBangladesh"
IndonesiaToKorea_peryear$group <- "IndonesiaToKorea"
IndonesiaToJapan_peryear$group <- "IndonesiaToJapan"
###
VietnamToWild_filter <- VietnamToWild %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
VietnamToChina_filter <- VietnamToChina %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
VietnamToEurope_filter <- VietnamToEurope %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
VietnamToIndonesia_filter <- VietnamToIndonesia %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
VietnamToBangladesh_filter <- VietnamToBangladesh %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
VietnamToKorea_filter <- VietnamToKorea %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
VietnamToJapan_filter <- VietnamToJapan %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
#
VietnamToWild_peryear <- VietnamToWild %>% group_by(treeId,period) %>% tally()
VietnamToChina_peryear <- VietnamToChina %>% group_by(treeId,period) %>% tally()
VietnamToEurope_peryear <- VietnamToEurope %>% group_by(treeId,period) %>% tally()
VietnamToIndonesia_peryear <- VietnamToIndonesia %>% group_by(treeId,period) %>% tally()
VietnamToBangladesh_peryear <- VietnamToBangladesh %>% group_by(treeId,period) %>% tally()
VietnamToKorea_peryear <- VietnamToKorea %>% group_by(treeId,period) %>% tally()
VietnamToJapan_peryear <- VietnamToJapan %>% group_by(treeId,period) %>% tally()
#
VietnamToWild_peryear$period <- VietnamToWild_peryear$period+2000-2
VietnamToChina_peryear$period <- VietnamToChina_peryear$period+2000-2
VietnamToEurope_peryear$period <- VietnamToEurope_peryear$period+2000-2
VietnamToIndonesia_peryear$period <- VietnamToIndonesia_peryear$period+2000-2
VietnamToBangladesh_peryear$period <- VietnamToBangladesh_peryear$period+2000-2
VietnamToKorea_peryear$period <- VietnamToKorea_peryear$period+2000-2
VietnamToJapan_peryear$period <- VietnamToJapan_peryear$period+2000-2
#
VietnamToWild_peryear$group <- "VietnamToWild"
VietnamToChina_peryear$group <- "VietnamToChina"
VietnamToEurope_peryear$group <- "VietnamToEurope"
VietnamToIndonesia_peryear$group <- "VietnamToIndonesia"
VietnamToBangladesh_peryear$group <- "VietnamToBangladesh"
VietnamToKorea_peryear$group <- "VietnamToKorea"
VietnamToJapan_peryear$group <- "VietnamToJapan"
###
BangladeshToWild_filter <- BangladeshToWild %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
BangladeshToChina_filter <- BangladeshToChina %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
BangladeshToEurope_filter <- BangladeshToEurope %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
BangladeshToIndonesia_filter <- BangladeshToIndonesia %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
BangladeshToVietnam_filter <- BangladeshToVietnam %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
BangladeshToKorea_filter <- BangladeshToKorea %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
BangladeshToJapan_filter <- BangladeshToJapan %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
#
BangladeshToWild_peryear <- BangladeshToWild %>% group_by(treeId,period) %>% tally()
BangladeshToChina_peryear <- BangladeshToChina %>% group_by(treeId,period) %>% tally()
BangladeshToEurope_peryear <- BangladeshToEurope %>% group_by(treeId,period) %>% tally()
BangladeshToIndonesia_peryear <- BangladeshToIndonesia %>% group_by(treeId,period) %>% tally()
BangladeshToVietnam_peryear <- BangladeshToVietnam %>% group_by(treeId,period) %>% tally()
BangladeshToKorea_peryear <- BangladeshToKorea %>% group_by(treeId,period) %>% tally()
BangladeshToJapan_peryear <- BangladeshToJapan %>% group_by(treeId,period) %>% tally()
#
BangladeshToWild_peryear$period <- BangladeshToWild_peryear$period+2000-2
BangladeshToChina_peryear$period <- BangladeshToChina_peryear$period+2000-2
BangladeshToEurope_peryear$period <- BangladeshToEurope_peryear$period+2000-2
BangladeshToIndonesia_peryear$period <- BangladeshToIndonesia_peryear$period+2000-2
BangladeshToVietnam_peryear$period <- BangladeshToVietnam_peryear$period+2000-2
BangladeshToKorea_peryear$period <- BangladeshToKorea_peryear$period+2000-2
BangladeshToJapan_peryear$period <- BangladeshToJapan_peryear$period+2000-2
#
BangladeshToWild_peryear$group <- "BangladeshToWild"
BangladeshToChina_peryear$group <- "BangladeshToChina"
BangladeshToEurope_peryear$group <- "BangladeshToEurope"
BangladeshToIndonesia_peryear$group <- "BangladeshToIndonesia"
BangladeshToVietnam_peryear$group <- "BangladeshToVietnam"
BangladeshToKorea_peryear$group <- "BangladeshToKorea"
BangladeshToJapan_peryear$group <- "BangladeshToJapan"
###
KoreaToWild_filter <- KoreaToWild %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
KoreaToChina_filter <- KoreaToChina %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
KoreaToEurope_filter <- KoreaToEurope %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
KoreaToIndonesia_filter <- KoreaToIndonesia %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
KoreaToVietnam_filter <- KoreaToVietnam %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
KoreaToBangladesh_filter <- KoreaToBangladesh %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
KoreaToJapan_filter <- KoreaToJapan %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
#
KoreaToWild_peryear <- KoreaToWild %>% group_by(treeId,period) %>% tally()
KoreaToChina_peryear <- KoreaToChina %>% group_by(treeId,period) %>% tally()
KoreaToEurope_peryear <- KoreaToEurope %>% group_by(treeId,period) %>% tally()
KoreaToIndonesia_peryear <- KoreaToIndonesia %>% group_by(treeId,period) %>% tally()
KoreaToVietnam_peryear <- KoreaToVietnam %>% group_by(treeId,period) %>% tally()
KoreaToBangladesh_peryear <- KoreaToBangladesh %>% group_by(treeId,period) %>% tally()
KoreaToJapan_peryear <- KoreaToJapan %>% group_by(treeId,period) %>% tally()
#
KoreaToWild_peryear$period <- KoreaToWild_peryear$period+2000-2
KoreaToChina_peryear$period <- KoreaToChina_peryear$period+2000-2
KoreaToEurope_peryear$period <- KoreaToEurope_peryear$period+2000-2
KoreaToIndonesia_peryear$period <- KoreaToIndonesia_peryear$period+2000-2
KoreaToVietnam_peryear$period <- KoreaToVietnam_peryear$period+2000-2
KoreaToBangladesh_peryear$period <- KoreaToBangladesh_peryear$period+2000-2
KoreaToJapan_peryear$period <- KoreaToJapan_peryear$period+2000-2
#
KoreaToWild_peryear$group <- "KoreaToWild"
KoreaToChina_peryear$group <- "KoreaToChina"
KoreaToEurope_peryear$group <- "KoreaToEurope"
KoreaToIndonesia_peryear$group <- "KoreaToIndonesia"
KoreaToVietnam_peryear$group <- "KoreaToVietnam"
KoreaToBangladesh_peryear$group <- "KoreaToBangladesh"
KoreaToJapan_peryear$group <- "KoreaToJapan"
###
JapanToWild_filter <- JapanToWild %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
JapanToChina_filter <- JapanToChina %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
JapanToEurope_filter <- JapanToEurope %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
JapanToIndonesia_filter <- JapanToIndonesia %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
JapanToVietnam_filter <- JapanToVietnam %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
JapanToBangladesh_filter <- JapanToBangladesh %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
JapanToKorea_filter <- JapanToKorea %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
#
JapanToWild_peryear <- JapanToWild %>% group_by(treeId,period) %>% tally()
JapanToChina_peryear <- JapanToChina %>% group_by(treeId,period) %>% tally()
JapanToEurope_peryear <- JapanToEurope %>% group_by(treeId,period) %>% tally()
JapanToIndonesia_peryear <- JapanToIndonesia %>% group_by(treeId,period) %>% tally()
JapanToVietnam_peryear <- JapanToVietnam %>% group_by(treeId,period) %>% tally()
JapanToBangladesh_peryear <- JapanToBangladesh %>% group_by(treeId,period) %>% tally()
JapanToKorea_peryear <- JapanToKorea %>% group_by(treeId,period) %>% tally()
#
JapanToWild_peryear$period <- JapanToWild_peryear$period+2000-2
JapanToChina_peryear$period <- JapanToChina_peryear$period+2000-2
JapanToEurope_peryear$period <- JapanToEurope_peryear$period+2000-2
JapanToIndonesia_peryear$period <- JapanToIndonesia_peryear$period+2000-2
JapanToVietnam_peryear$period <- JapanToVietnam_peryear$period+2000-2
JapanToBangladesh_peryear$period <- JapanToBangladesh_peryear$period+2000-2
JapanToKorea_peryear$period <- JapanToKorea_peryear$period+2000-2
#
JapanToWild_peryear$group <- "JapanToWild"
JapanToChina_peryear$group <- "JapanToChina"
JapanToEurope_peryear$group <- "JapanToEurope"
JapanToIndonesia_peryear$group <- "JapanToIndonesia"
JapanToVietnam_peryear$group <- "JapanToVietnam"
JapanToBangladesh_peryear$group <- "JapanToBangladesh"
JapanToKorea_peryear$group <- "JapanToKorea"
###
WildToJapan_filter <- WildToJapan %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
WildToChina_filter <- WildToChina %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
WildToEurope_filter <- WildToEurope %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
WildToIndonesia_filter <- WildToIndonesia %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
WildToVietnam_filter <- WildToVietnam %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
WildToBangladesh_filter <- WildToBangladesh %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
WildToKorea_filter <- WildToKorea %>% group_by(treeId,period) %>% slice(which.max(MarkovJumpsNum))
#
WildToJapan_peryear <- WildToJapan %>% group_by(treeId,period) %>% tally()
WildToChina_peryear <- WildToChina %>% group_by(treeId,period) %>% tally()
WildToEurope_peryear <- WildToEurope %>% group_by(treeId,period) %>% tally()
WildToIndonesia_peryear <- WildToIndonesia %>% group_by(treeId,period) %>% tally()
WildToVietnam_peryear <- WildToVietnam %>% group_by(treeId,period) %>% tally()
WildToBangladesh_peryear <- WildToBangladesh %>% group_by(treeId,period) %>% tally()
WildToKorea_peryear <- WildToKorea %>% group_by(treeId,period) %>% tally()
#
WildToJapan_peryear$period <- WildToJapan_peryear$period+2000-2
WildToChina_peryear$period <- WildToChina_peryear$period+2000-2
WildToEurope_peryear$period <- WildToEurope_peryear$period+2000-2
WildToIndonesia_peryear$period <- WildToIndonesia_peryear$period+2000-2
WildToVietnam_peryear$period <- WildToVietnam_peryear$period+2000-2
WildToBangladesh_peryear$period <- WildToBangladesh_peryear$period+2000-2
WildToKorea_peryear$period <- WildToKorea_peryear$period+2000-2
#
WildToJapan_peryear$group <- "WildToJapan"
WildToChina_peryear$group <- "WildToChina"
WildToEurope_peryear$group <- "WildToEurope"
WildToIndonesia_peryear$group <- "WildToIndonesia"
WildToVietnam_peryear$group <- "WildToVietnam"
WildToBangladesh_peryear$group <- "WildToBangladesh"
WildToKorea_peryear$group <- "WildToKorea"


final_data <- rbind(ChinaToWild_peryear,ChinaToEurope_peryear,ChinaToIndonesia_peryear,ChinaToVietnam_peryear,ChinaToBangladesh_peryear,ChinaToKorea_peryear,ChinaToJapan_peryear,
                    EuropeToWild_peryear,EuropeToChina_peryear,EuropeToIndonesia_peryear,EuropeToVietnam_peryear,EuropeToBangladesh_peryear,EuropeToKorea_peryear,EuropeToJapan_peryear,
                    IndonesiaToWild_peryear,IndonesiaToChina_peryear,IndonesiaToEurope_peryear,IndonesiaToVietnam_peryear,IndonesiaToBangladesh_peryear,IndonesiaToKorea_peryear,IndonesiaToJapan_peryear,
                    VietnamToWild_peryear,VietnamToChina_peryear,VietnamToEurope_peryear,VietnamToIndonesia_peryear,VietnamToBangladesh_peryear,VietnamToKorea_peryear,VietnamToJapan_peryear,
                    BangladeshToWild_peryear,BangladeshToChina_peryear,BangladeshToEurope_peryear,BangladeshToIndonesia_peryear,BangladeshToVietnam_peryear,BangladeshToKorea_peryear,BangladeshToJapan_peryear,
                    KoreaToWild_peryear,KoreaToChina_peryear,KoreaToEurope_peryear,KoreaToIndonesia_peryear,KoreaToVietnam_peryear,KoreaToBangladesh_peryear,KoreaToJapan_peryear,
                    JapanToWild_peryear,JapanToChina_peryear,JapanToEurope_peryear,JapanToIndonesia_peryear,JapanToVietnam_peryear,JapanToBangladesh_peryear,JapanToKorea_peryear,
                    WildToChina_peryear,WildToEurope_peryear,WildToIndonesia_peryear,WildToVietnam_peryear,WildToBangladesh_peryear,WildToKorea_peryear,WildToJapan_peryear)

#write.csv(final_data,"D:\\LBY\\fig1\\fig2b.csv")




### 获得每年新增MJ数量以及max和min
getTimePionts_year <- function(df){
  df <- na.omit (df)
  TimeSeries <- as.data.frame(unique(df$period))
  colnames(TimeSeries)[1] = "TimeSeries"
  TimeSeries$TimePoint = 0
  TimeSeries$lower = 0
  TimeSeries$higher = 0
  TimeSeries$group = unique(df$group)
  CI_TimeSeries <- TimeSeries
  for (i in 1:length(CI_TimeSeries$TimeSeries)){
    a <- df[df$period == CI_TimeSeries[i,1],]
    
    CI_TimeSeries[i,2] = mean(a$n)
    CI_TimeSeries[i,3] = min(a$n)
    CI_TimeSeries[i,4] = max(a$n)
  }
  
  return(CI_TimeSeries)
}

ChinaToWild_peryear_count <- getTimePionts_year(ChinaToWild_peryear)
ChinaToEurope_peryear_count <- getTimePionts_year(ChinaToEurope_peryear)
ChinaToIndonesia_peryear_count <- getTimePionts_year(ChinaToIndonesia_peryear)
ChinaToVietnam_peryear_count <- getTimePionts_year(ChinaToVietnam_peryear)
ChinaToBangladesh_peryear_count <- getTimePionts_year(ChinaToBangladesh_peryear)
ChinaToKorea_peryear_count <- getTimePionts_year(ChinaToKorea_peryear)
ChinaToJapan_peryear_count <- getTimePionts_year(ChinaToJapan_peryear)

EuropeToWild_peryear_count <- getTimePionts_year(EuropeToWild_peryear)
EuropeToChina_peryear_count <- getTimePionts_year(EuropeToChina_peryear)
EuropeToIndonesia_peryear_count <- getTimePionts_year(EuropeToIndonesia_peryear)
EuropeToVietnam_peryear_count <- getTimePionts_year(EuropeToVietnam_peryear)
EuropeToBangladesh_peryear_count <- getTimePionts_year(EuropeToBangladesh_peryear)
EuropeToKorea_peryear_count <- getTimePionts_year(EuropeToKorea_peryear)
EuropeToJapan_peryear_count <- getTimePionts_year(EuropeToJapan_peryear)

IndonesiaToWild_peryear_count <- getTimePionts_year(IndonesiaToWild_peryear)
IndonesiaToChina_peryear_count <- getTimePionts_year(IndonesiaToChina_peryear)
IndonesiaToEurope_peryear_count <- getTimePionts_year(IndonesiaToEurope_peryear)
IndonesiaToVietnam_peryear_count <- getTimePionts_year(IndonesiaToVietnam_peryear)
IndonesiaToBangladesh_peryear_count <- getTimePionts_year(IndonesiaToBangladesh_peryear)
IndonesiaToKorea_peryear_count <- getTimePionts_year(IndonesiaToKorea_peryear)
IndonesiaToJapan_peryear_count <- getTimePionts_year(IndonesiaToJapan_peryear)

VietnamToWild_peryear_count <- getTimePionts_year(VietnamToWild_peryear)
VietnamToChina_peryear_count <- getTimePionts_year(VietnamToChina_peryear)
VietnamToEurope_peryear_count <- getTimePionts_year(VietnamToEurope_peryear)
VietnamToIndonesia_peryear_count <- getTimePionts_year(VietnamToIndonesia_peryear)
VietnamToBangladesh_peryear_count <- getTimePionts_year(VietnamToBangladesh_peryear)
VietnamToKorea_peryear_count <- getTimePionts_year(VietnamToKorea_peryear)
VietnamToJapan_peryear_count <- getTimePionts_year(VietnamToJapan_peryear)

BangladeshToWild_peryear_count <- getTimePionts_year(BangladeshToWild_peryear)
BangladeshToChina_peryear_count <- getTimePionts_year(BangladeshToChina_peryear)
BangladeshToEurope_peryear_count <- getTimePionts_year(BangladeshToEurope_peryear)
BangladeshToIndonesia_peryear_count <- getTimePionts_year(BangladeshToIndonesia_peryear)
BangladeshToVietnam_peryear_count <- getTimePionts_year(BangladeshToVietnam_peryear)
BangladeshToKorea_peryear_count <- getTimePionts_year(BangladeshToKorea_peryear)
BangladeshToJapan_peryear_count <- getTimePionts_year(BangladeshToJapan_peryear)

KoreaToWild_peryear_count <- getTimePionts_year(KoreaToWild_peryear)
KoreaToChina_peryear_count <- getTimePionts_year(KoreaToChina_peryear)
KoreaToEurope_peryear_count <- getTimePionts_year(KoreaToEurope_peryear)
KoreaToIndonesia_peryear_count <- getTimePionts_year(KoreaToIndonesia_peryear)
KoreaToVietnam_peryear_count <- getTimePionts_year(KoreaToVietnam_peryear)
KoreaToBangladesh_peryear_count <- getTimePionts_year(KoreaToBangladesh_peryear)
KoreaToJapan_peryear_count <- getTimePionts_year(KoreaToJapan_peryear)

JapanToWild_peryear_count <- getTimePionts_year(JapanToWild_peryear)
JapanToChina_peryear_count <- getTimePionts_year(JapanToChina_peryear)
JapanToEurope_peryear_count <- getTimePionts_year(JapanToEurope_peryear)
JapanToIndonesia_peryear_count <- getTimePionts_year(JapanToIndonesia_peryear)
JapanToVietnam_peryear_count <- getTimePionts_year(JapanToVietnam_peryear)
JapanToBangladesh_peryear_count <- getTimePionts_year(JapanToBangladesh_peryear)
JapanToKorea_peryear_count <- getTimePionts_year(JapanToKorea_peryear)

WildToJapan_peryear_count <- getTimePionts_year(WildToJapan_peryear)
WildToChina_peryear_count <- getTimePionts_year(WildToChina_peryear)
WildToEurope_peryear_count <- getTimePionts_year(WildToEurope_peryear)
WildToIndonesia_peryear_count <- getTimePionts_year(WildToIndonesia_peryear)
WildToVietnam_peryear_count <- getTimePionts_year(WildToVietnam_peryear)
WildToBangladesh_peryear_count <- getTimePionts_year(WildToBangladesh_peryear)
WildToKorea_peryear_count <- getTimePionts_year(WildToKorea_peryear)

final_data_peryear <- rbind(ChinaToWild_peryear_count,ChinaToEurope_peryear_count,ChinaToIndonesia_peryear_count,ChinaToVietnam_peryear_count,ChinaToBangladesh_peryear_count,ChinaToKorea_peryear_count,ChinaToJapan_peryear_count,
                                  EuropeToWild_peryear_count,EuropeToChina_peryear_count,EuropeToIndonesia_peryear_count,EuropeToVietnam_peryear_count,EuropeToBangladesh_peryear_count,EuropeToKorea_peryear_count,EuropeToJapan_peryear_count,
                                  IndonesiaToWild_peryear_count,IndonesiaToChina_peryear_count,IndonesiaToEurope_peryear_count,IndonesiaToVietnam_peryear_count,IndonesiaToBangladesh_peryear_count,IndonesiaToKorea_peryear_count,IndonesiaToJapan_peryear_count,
                                  VietnamToWild_peryear_count,VietnamToChina_peryear_count,VietnamToEurope_peryear_count,VietnamToIndonesia_peryear_count,VietnamToBangladesh_peryear_count,VietnamToKorea_peryear_count,VietnamToJapan_peryear_count,
                                  BangladeshToWild_peryear_count,BangladeshToChina_peryear_count,BangladeshToEurope_peryear_count,BangladeshToIndonesia_peryear_count,BangladeshToVietnam_peryear_count,BangladeshToKorea_peryear_count,BangladeshToJapan_peryear_count,
                                  KoreaToWild_peryear_count,KoreaToChina_peryear_count,KoreaToEurope_peryear_count,KoreaToIndonesia_peryear_count,KoreaToVietnam_peryear_count,KoreaToBangladesh_peryear_count,KoreaToJapan_peryear_count,
                                  JapanToWild_peryear_count,JapanToChina_peryear_count,JapanToEurope_peryear_count,JapanToIndonesia_peryear_count,JapanToVietnam_peryear_count,JapanToBangladesh_peryear_count,JapanToKorea_peryear_count,
                                  WildToChina_peryear_count,WildToEurope_peryear_count,WildToIndonesia_peryear_count,WildToVietnam_peryear_count,WildToBangladesh_peryear_count,WildToKorea_peryear_count,WildToJapan_peryear_count)

#write.csv(final_data_peryear,"D:\\LBY\\fig1\\fig2a.csv")
###合并，根绝period获得准确年份
final_data <- rbind(ChinaToWild_filter,ChinaToEurope_filter,ChinaToIndonesia_filter,ChinaToVietnam_filter,ChinaToBangladesh_filter,ChinaToKorea_filter,ChinaToJapan_filter,
                           EuropeToWild_filter,EuropeToChina_filter,EuropeToIndonesia_filter,EuropeToVietnam_filter,EuropeToBangladesh_filter,EuropeToKorea_filter,EuropeToJapan_filter,
                           IndonesiaToWild_filter,IndonesiaToChina_filter,IndonesiaToEurope_filter,IndonesiaToVietnam_filter,IndonesiaToBangladesh_filter,IndonesiaToKorea_filter,IndonesiaToJapan_filter,
                           VietnamToWild_filter,VietnamToChina_filter,VietnamToEurope_filter,VietnamToIndonesia_filter,VietnamToBangladesh_filter,VietnamToKorea_filter,VietnamToJapan_filter,
                           BangladeshToWild_filter,BangladeshToChina_filter,BangladeshToEurope_filter,BangladeshToIndonesia_filter,BangladeshToVietnam_filter,BangladeshToKorea_filter,BangladeshToJapan_filter,
                           KoreaToWild_filter,KoreaToChina_filter,KoreaToEurope_filter,KoreaToIndonesia_filter,KoreaToVietnam_filter,KoreaToBangladesh_filter,KoreaToJapan_filter,
                           JapanToWild_filter,JapanToChina_filter,JapanToEurope_filter,JapanToIndonesia_filter,JapanToVietnam_filter,JapanToBangladesh_filter,JapanToKorea_filter,
                           WildToChina_filter,WildToEurope_filter,WildToIndonesia_filter,WildToVietnam_filter,WildToBangladesh_filter,WildToKorea_filter,WildToJapan_filter)

final_data$period <- final_data$period+2000-2
#write.csv(final_data,"\\final_data_MJs.csv")

#group_max <- aggregate(final_data$MarkovJumpsNum, list(final_data$group,final_data$treeId), mean)
#final_data_max <- final_data %>% group_by(treeId,group) %>% slice(which.max(MarkovJumpsNum))
#group_mean <- aggregate(final_data_max$MarkovJumpsNum, list(final_data_max$group), mean)

numdateTodate <- function(df){
  df$year = floor(df$time)
  df$numdays = df$time-df$year
  df$starday = as.Date(paste(df$year, "01","01",sep="/"),format="%Y/%m/%d")
  df$endday = as.Date(paste(df$year, "12","31",sep="/"),format="%Y/%m/%d")
  df$yeardays = as.numeric(substr(difftime(df$endday, df$starday, units = "days"),1,3))+1
  df$days = floor(df$yeardays*df$numdays)
  df$months = month(df$starday + ddays(df$days))
  df$YearMonth = df$year*100+df$months
  final_data <- df
  #  final_data <- df[,c(1:5,13)]
  return(final_data)
}

WildToEurope_ <- numdateTodate(WildToEurope)
ChinaToWild_ <- numdateTodate(ChinaToWild)

getTimePionts <- function(df){
  #TimeSeries <- as.data.frame(unique(df$YearMonth))
  TimeSeries <- as.data.frame(unique(df$year))
  colnames(TimeSeries)[1] = "TimeSeries"
  TimeSeries$freq = 0
  TimeSeries$TimePoint = 0
  TimeSeries$lower = 0
  TimeSeries$higher = 0
  TimeSeries$group = unique(df$group)
  for (i in 1:length(TimeSeries$TimeSeries)){
    #TimeSeries[i,2] = sum(df$YearMonth == TimeSeries[i,1])
    TimeSeries[i,2] = sum(df$year == TimeSeries[i,1])
  }
  
  CI_TimeSeries <- TimeSeries#[TimeSeries$freq > CI(TimeSeries$freq,ci=0.95)[3],]
  for (i in 1:length(CI_TimeSeries$TimeSeries)){
    #a <- df[df$YearMonth==CI_TimeSeries[i,1],]
    a <- df[df$year==CI_TimeSeries[i,1],]
    b<- a %>% group_by(treeId) %>% filter(MarkovJumpsNum == max(MarkovJumpsNum))
    
    CI_TimeSeries[i,3] = mean(b$MarkovJumpsNum)
    CI_TimeSeries[i,4] = min(b$MarkovJumpsNum)
    CI_TimeSeries[i,5] = max(b$MarkovJumpsNum)
    
    #    CI_TimeSeries[i,3] = CI(b$MarkovJumpsNum,ci = 0.95)[2]
    #    CI_TimeSeries[i,4] = CI(b$MarkovJumpsNum,ci = 0.95)[1]
    #    CI_TimeSeries[i,5] = CI(b$MarkovJumpsNum,ci = 0.95)[3]
  }
  
  return(CI_TimeSeries)
}

WildToEurope_mean <- getTimePionts(WildToEurope_)
ChinaToWild_mean <- getTimePionts(ChinaToWild_)

line_with_HPD <- rbind(WildToEurope_mean, ChinaToWild_mean)

ggplot(line_with_HPD, aes(group = group,x = TimeSeries)) +
  geom_ribbon(aes(ymin = lower, ymax = higher,fill = group), alpha = 0.5) +
  geom_line(aes(y = TimePoint,color = group),size = 1, alpha = 1)+
  theme_minimal()+
  theme_bw()+
  labs(x="Time",y="The MJs")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        axis.line.y = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12),
        legend.title=element_blank(),
        legend.position=c(0.3,0.65),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black",size = 0))+
  scale_x_continuous(limits = c(1996.5,2022.5), breaks=seq(1996,2025,5),expand = c(0,0))+
  scale_y_continuous(limits = c(0,250), breaks=seq(0,250,50),expand = c(0.01,0))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"),
        legend.key.width = unit(1.1,"cm"),
        legend.key.height = unit(0.8,"cm"))





######################################################
MJ <- read_xlsx("\\final_seqs_Eurasia_beast_dta_combined.jumpHistory_sum.xlsx",
                sheet = "from_to")

cols <- c(colorRampPalette(c("#e7f0fa","#c9e2f6","#95cbee","#0099dc","#4ab04a","#ffd73e"))(10),
          colorRampPalette(c("#eec73a","#e29421","#e29421","#f05336","#ce472e"),bias=2)(99))  

ggplot(MJ,aes(x=To,y=From,fill=mean))+
  geom_raster()+
  #  scale_fill_gradient(low="white", high="#990033")+
  scale_fill_gradient(low="white", high="#990033", limits=c(0, 220),
                       breaks=c(0,50,100,220), 
                       na.value=rgb(100, 100, 100, max=255),
                       labels=c("0", "50", "100", "220"),
                       guide=guide_colourbar(ticks=T, 
                                             nbin=50,
                                             barheight=10, 
                                             label=T,
                                             barwidth=1))+
  theme(axis.text.y=element_text(size=10,color = "black", angle = 0),
        axis.text.x=element_text(size=10,color = "black", angle = 45),
        panel.grid=element_blank())


library(circlize)
MJ <- subset(MJ,MJ$median > 2)
group<-c("Wild"="C","KoreaDomestic"="A","JapanDomestic"="A",
         "VietnamDomestic"="B","IndonesiaDomestic"="B","ChinaDomestic"="B",
         "EuropeDomestic"="A","BangladeshDomestic"="B")

grid.col<-c("Wild"="#4B86C5","KoreaDomestic"="#89CDF0","JapanDomestic"="#EFA2B5",
            "VietnamDomestic"="#3A8D9A","IndonesiaDomestic"="#9F83A6","ChinaDomestic"="#F1644A",
            "EuropeDomestic"="#84C077","BangladeshDomestic"="#FFC184")

chordDiagram(MJ[,c(4,5,1)],
             link.lwd = 1,    # Line width
             link.lty = 1,    # Line type
             link.border = 1,
             annotationTrackHeight = mm_h(c(8, 10)),
             group = group,
             grid.col = grid.col) 
highlight.sector(c("EuropeDomestic","KoreaDomestic","JapanDomestic"), track.index = 1, 
                 col = "#FF856580", 
                 text = "A", cex = 0.8, text.col = "black", border = "black",niceFacing = TRUE)
highlight.sector(c("ChinaDomestic","IndonesiaDomestic","BangladeshDomestic","VietnamDomestic"), 
                 track.index = 1, col = "#FF856580", 
                 text = "A", cex = 0.8, text.col = "black", border = "black",niceFacing = TRUE)
highlight.sector(c("Wild"), track.index = 1, 
                 col = "#FF856580", 
                 text = "A", cex = 0.8, text.col = "black", border = "black",niceFacing = TRUE)





chordDiagram(
  MJ[,c(4,5,1)], directional = 1, 
  direction.type = c("diffHeight", "arrows"),
  link.arr.type = "big.arrow"
)

######################################################
final_data <- read.csv("\\final_data_MJs.csv")

grid.col<-c("Wild"="#4B86C5","KoreaDomestic"="#89CDF0","JapanDomestic"="#EFA2B5",
            "VietnamDomestic"="#3A8D9A","IndonesiaDomestic"="#9F83A6","ChinaDomestic"="#F1644A",
            "EuropeDomestic"="#84C077","BangladeshDomestic"="#FFC184")

treeid <- seq(min(final_data$treeId),max(final_data$treeId),20000)
treeid_num <- 500
random_treeid <- sample(treeid, treeid_num, replace = FALSE,prob = NULL)
random_state <- filter(final_data,final_data$treeId %in% random_treeid)

final_data <- random_state


###
p_1 <- ggplot() +
  geom_step(data=subset(final_data,final_data$group == "WildToChina"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.1,color = "#F1644A")+
  geom_step(data=subset(final_data,final_data$group == "WildToIndonesia"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.1,color = "#9F83A6")+
  geom_step(data=subset(final_data,final_data$group == "WildToVietnam"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.1,color = "#3A8D9A")+
  geom_step(data=subset(final_data,final_data$group == "WildToBangladesh"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.1,color = "#FFC184")+
  scale_x_continuous(limits = c(1994.5,2023.5), breaks=seq(1996,2023,5),expand = c(0.01,0))+
  scale_y_continuous(limits = c(0,250), breaks=seq(0,250,50),expand = c(0.01,0))+
  labs(x="Time",y="Cumulative number of \n
       Markov jumps from wild birds")+
  theme_bw()+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        axis.line.y = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12),
        legend.title=element_blank(),
        legend.position = c(0.2,0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,0,1), "lines"),
        legend.key.width = unit(1.1,"cm"),
        legend.key.height = unit(0.8,"cm"))

p_2 <- ggplot() +
  geom_step(data=subset(final_data,final_data$group == "WildToEurope"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.1,color = "#84C077")+
  geom_step(data=subset(final_data,final_data$group == "WildToJapan"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.1,color = "#EFA2B5")+
  geom_step(data=subset(final_data,final_data$group == "WildToKorea"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.1,color = "#89CDF0")+
  scale_x_continuous(limits = c(1994.5,2023.5), breaks=seq(1996,2023,5),expand = c(0.01,0))+
  scale_y_continuous(limits = c(0,250), breaks=seq(0,250,50),expand = c(0.01,0))+
  labs(x="Time",y="Cumulative number of /n
       Markov jumps from wild birds")+
  theme_bw()+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        axis.line.y = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12),
        legend.title=element_blank(),
        legend.position = c(0.2,0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,0,1), "lines"),
        legend.key.width = unit(1.1,"cm"),
        legend.key.height = unit(0.8,"cm"))

ggarrange(p_2, p_1,
          ncol = 2)

p_3 <- ggplot() +
  geom_step(data=subset(final_data,final_data$group == "ChinaToWild"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.1,color = "#F1644A")+
  geom_step(data=subset(final_data,final_data$group == "IndonesiaToWild"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.1,color = "#9F83A6")+
  geom_step(data=subset(final_data,final_data$group == "VietnamToWild"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.1,color = "#3A8D9A")+
  geom_step(data=subset(final_data,final_data$group == "BangladeshToWild"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.1,color = "#FFC184")+
  scale_x_continuous(limits = c(1994.5,2023.5), breaks=seq(1996,2023,5),expand = c(0.01,0))+
  scale_y_continuous(limits = c(0,120), breaks=seq(0,120,30),expand = c(0.01,0))+
  labs(x="Time",y="Cumulative number of /n
       Markov jumps to wild birds")+
  theme_bw()+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        axis.line.y = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12),
        legend.title=element_blank(),
        legend.position = c(0.2,0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,0,1), "lines"),
        legend.key.width = unit(1.1,"cm"),
        legend.key.height = unit(0.8,"cm"))

p_4 <- ggplot() +
  geom_step(data=subset(final_data,final_data$group == "EuropeToWild"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.1,color = "#84C077")+
  geom_step(data=subset(final_data,final_data$group == "JapanToWild"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.1,color = "#EFA2B5")+
  geom_step(data=subset(final_data,final_data$group == "KoreaToWild"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.1,color = "#89CDF0")+
  scale_x_continuous(limits = c(1994.5,2023.5), breaks=seq(1996,2023,5),expand = c(0.01,0))+
  scale_y_continuous(limits = c(0,120), breaks=seq(0,120,30),expand = c(0.01,0))+
  labs(x="Time",y="Cumulative number of /n
       Markov jumps to wild birds")+
  theme_bw()+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        axis.line.y = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12),
        legend.title=element_blank(),
        legend.position = c(0.2,0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,0,1), "lines"),
        legend.key.width = unit(1.1,"cm"),
        legend.key.height = unit(0.8,"cm"))

ggarrange(p_4, p_3,
          ncol = 2)

ggarrange(p_2,p_1,p_4, p_3,
          ncol = 2,nrow = 2)

ggplot() +
  geom_step(data=subset(final_data,final_data$group == "WildToEurope"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.2,color = "#4B86C5")+
  geom_step(data=subset(final_data,final_data$group == "ChinaToWild"), 
            aes(x=time, y=MarkovJumpsNum,group = treeId),alpha=0.2,color = "#F1644A")+
  geom_line(data=WildToEurope_mean, aes(y = TimePoint, x = TimeSeries+1),size = 1, alpha = 1)+
  geom_line(data=ChinaToWild_mean, aes(y = TimePoint, x = TimeSeries+1),size = 1, alpha = 1)+
  scale_x_continuous(limits = c(1994.5,2023.5), breaks=seq(1996,2023,5),expand = c(0.01,0))+
  scale_y_continuous(limits = c(0,250), breaks=seq(0,250,50),expand = c(0.01,0))+
  labs(x="Time",y="Cumulative number of /n
       Markov jumps from wild birds")+
  theme_bw()+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        axis.line.y = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12),
        legend.title=element_blank(),
        legend.position = c(0.2,0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,0,1), "lines"),
        legend.key.width = unit(1.1,"cm"),
        legend.key.height = unit(0.8,"cm"))
