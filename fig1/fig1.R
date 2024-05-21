library(readxl)
library(pheatmap)
library(tidyr)
library(ggplot2)
library(cowplot)
rm(list = ls())

data<-read_xlsx("E:\\H5_H7\\20230220\\global\\0714\\gisaid_epiflu_isolates_global_20230714.xlsx",sheet = "year_continent_all")
data_p<-read_xlsx("E:\\H5_H7\\20230220\\global\\0714\\gisaid_epiflu_isolates_global_20230714.xlsx",sheet = "year_continent_p")[1:6,]
data_w<-read_xlsx("E:\\H5_H7\\20230220\\global\\0714\\gisaid_epiflu_isolates_global_20230714.xlsx",sheet = "year_continent_w")[1:7,]

data <- tidyr::gather(data, Year, samples, -Continent)
data$Year <- as.numeric(data$Year)
data$class <- "all"

data_p <- tidyr::gather(data_p, Year, samples, -Continent)
data_p$Year <- as.numeric(data_p$Year)
data_p$class <- "p"

data_w <- tidyr::gather(data_w, Year, samples, -Continent)
data_w$Year <- as.numeric(data_w$Year)
data_w$class <- "w"

df <- rbind(data_p,data_w)
df <- subset(df,df$Continent == "Africa" | df$Continent == "Asia" | df$Continent == "Europe" | df$Continent == "North America")
ggplot(df,aes(Year,samples,fill=class))+
  geom_bar(stat="identity",position="dodge")+ 
  facet_grid(Continent~.)




#############
data<-read_xlsx("E:\\H5_H7\\20230220\\global\\0714\\gisaid_epiflu_isolates_global_20230714.xlsx",sheet = "hist")

df <- tidyr::gather(data, Year, samples, -Continent,-class)
###########################
rm(list = ls())
data_p<-read_xlsx("E:\\H5_H7\\20230220\\global\\0714\\gisaid_epiflu_isolates_global_20230714.xlsx",sheet = "year_continent_p")[1:6,]
data_w<-read_xlsx("E:\\H5_H7\\20230220\\global\\0714\\gisaid_epiflu_isolates_global_20230714.xlsx",sheet = "year_continent_w")[1:7,]
data_p <- tidyr::gather(data_p, Year, samples, -Continent)
data_p$Year <- as.numeric(data_p$Year)
data_p$class <- "p"

data_w <- tidyr::gather(data_w, Year, samples, -Continent)
data_w$Year <- as.numeric(data_w$Year)
data_w$class <- "w"
df <- rbind(data_p,data_w)

df$group <- paste0(df$Continent,"_",df$class)
df$samples <- as.numeric(df$samples)

df <- subset(df, df$Year > 1995)
#c1=rgb(63,133,15, max = 255)
c2=rgb(231,68,75, max = 255)

df$samplesgroup<-cut(df$samples,
                breaks = c(0,50,200,1100))

cols <- c(colorRampPalette(c("white",c2))(5))  

ggplot(df,aes(Year,group,fill = samples))+
  geom_tile()+
  theme_bw()+
  #scale_fill_manual(breaks = levels(df$samplesgroup),values = c("#86ebc9",c1,c2))+
  #scale_fill_gradient2(low= "white" ,high = c2, limits = c(1, max(df$samples)))+
  scale_fill_gradientn(colours=cols, limits=c(1, 1015),
                       breaks=c(50,200,500,1000), 
                       na.value=rgb(100, 100, 100, max=255),
                       labels=c("50", "200","500", "1000"),
                       guide=guide_colourbar(ticks=T, 
                                             nbin=50,
                                             barheight=10, 
                                             label=T,
                                             barwidth=0.5))+
  labs(x="Time",y="Samples")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        legend.text=element_text(size = 12),
        legend.background = element_blank())+
  theme(panel.grid.major = element_blank(),
        #legend.position = "none",
        legend.position = "right",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12)) +
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(0,0,0,0), "lines"))+
  scale_x_continuous(limits = c(1995.3,2023.7), breaks=seq(1996,2023,5),expand = c(0,0))
##############
rm(list = ls())
data_p<-read_xlsx("E:\\H5_H7\\20230220\\global\\0714\\gisaid_epiflu_isolates_global_20230714.xlsx",sheet = "year_country_p")
data_w<-read_xlsx("E:\\H5_H7\\20230220\\global\\0714\\gisaid_epiflu_isolates_global_20230714.xlsx",sheet = "year_country_w")
data_all<-read_xlsx("E:\\H5_H7\\20230220\\global\\0714\\gisaid_epiflu_isolates_global_20230714.xlsx",sheet = "year_country")
data_p <- tidyr::gather(data_p, Year, samples, -Continent,-Loc)
data_p$Year <- as.numeric(data_p$Year)
data_p$class <- "p"

data_w <- tidyr::gather(data_w, Year, samples, -Continent,-Loc)
data_w$Year <- as.numeric(data_w$Year)
data_w$class <- "w"
df <- rbind(data_p,data_w)

df <- subset(df, df$Continent == "Asia")

df$group <- paste0(df$Loc,"_",df$class)
df$samples <- as.numeric(df$samples)

df <- subset(df, df$Year > 1995)
c1=rgb(63,133,15, max = 255)
c2=rgb(195,0,110, max = 255)

df$samplesgroup<-cut(df$samples,
                     breaks = c(0,50,200,1100))

cols <- c(colorRampPalette(c("white",c2))(5))  

year_country_pw_Asia_p <- ggplot(df,aes(Year,group,fill = samples))+
  geom_tile()+
  theme_bw()+
  #scale_fill_manual(breaks = levels(df$samplesgroup),values = c("#86ebc9",c1,c2))+
  #scale_fill_gradient2(low= "white" ,high = c2, limits = c(1, max(df$samples)))+
  scale_fill_gradientn(colours=cols, limits=c(1, 500),
                       breaks=c(50,100,200,500), 
                       na.value=rgb(100, 100, 100, max=255),
                       labels=c("50", "100","200", "500"),
                       guide=guide_colourbar(ticks=T, 
                                             nbin=50,
                                             barheight=10, 
                                             label=T,
                                             barwidth=0.5))+
  labs(x="Time",y="")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        legend.text=element_text(size = 12),
        legend.background = element_blank())+
  theme(panel.grid.major = element_blank(),
        #legend.position = "none",
        legend.position = "right",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12)) +
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(0,0,0,0), "lines"))+
  scale_x_continuous(limits = c(1995.3,2023.7), breaks=seq(1996,2023,5),expand = c(0,0))


data_all <- tidyr::gather(data_all, Year, samples, -Continent,-Loc)
data_all$Year <- as.numeric(data_all$Year)
data_all$class <- "all"
year_country_all_Asia <- subset(data_all, data_all$Continent == "Asia")
year_country_all_Asia$samples <- as.numeric(year_country_all_Asia$samples)

year_country_all_Asia_p <- ggplot(year_country_all_Asia,aes(Year,Loc,fill = samples))+
  geom_tile()+
  theme_bw()+
  scale_fill_gradientn(colours=cols, limits=c(1, 816),
                       breaks=c(50,200,500,800), 
                       na.value=rgb(100, 100, 100, max=255),
                       labels=c("50", "200","500", "800"),
                       guide=guide_colourbar(ticks=T, 
                                             nbin=50,
                                             barheight=10, 
                                             label=T,
                                             barwidth=0.5))+
  labs(x="Time",y="")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        legend.text=element_text(size = 12),
        legend.background = element_blank())+
  theme(panel.grid.major = element_blank(),
        #legend.position = "none",
        legend.position = "right",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12)) +
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(0,0,0,0), "lines"))+
  scale_x_continuous(limits = c(1995.3,2023.7), breaks=seq(1996,2023,5),expand = c(0,0))


plot_grid(year_country_all_Asia_p,year_country_pw_Asia_p,
          ncol = 2,align = 'hv', axis = 'l', 
          rel_heights = c(1,1))


################
df_Africa <- subset(df,df$Continent == "Africa")
df_Asia <- subset(df,df$Continent == "Asia")
df_Europe <- subset(df,df$Continent == "Europe")
df_NorthAmerica <- subset(df,df$Continent == "North America")
df_Oceania <- subset(df,df$Continent == "Oceania")
df_SouthAmerica <- subset(df,df$Continent == "South America")
df_Antarctica <- subset(df,df$Continent == "Antarctica")

p_Africa <- ggplot(df_Africa,aes(Year,samples,fill = class))+
  geom_bar(stat="identity",position="dodge",colour='black',width=0.6)+
  theme_bw()+
  labs(x="p_Africa",y="Samples")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        legend.text=element_text(size = 12),
        legend.background = element_blank())+
  theme(panel.grid.major = element_line(color = "lightgray",linewidth = 1),
        legend.position = "none",
        #legend.position = "right",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,800), breaks=seq(0,800,200),expand = c(0,0))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"))

p_Asia <- ggplot(df_Asia,aes(Year,samples,fill = class))+
  geom_bar(stat="identity",position="dodge",colour='black',width=0.6)+
  theme_bw()+
  labs(x="p_Asia",y="Samples")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        legend.text=element_text(size = 12),
        legend.background = element_blank())+
  theme(panel.grid.major = element_line(color = "lightgray",linewidth = 1),
        legend.position = "none",
        #legend.position = "right",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,3100), breaks=seq(0,3000,600),expand = c(0,0))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"))

p_Europe <- ggplot(df_Europe,aes(Year,samples,fill = class))+
  geom_bar(stat="identity",position="dodge",colour='black',width=0.6)+
  theme_bw()+
  labs(x="p_Europe",y="Samples")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        legend.text=element_text(size = 12),
        legend.background = element_blank())+
  theme(panel.grid.major = element_line(color = "lightgray",linewidth = 1),
        legend.position = "none",
        #legend.position = "right",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,1800), breaks=seq(0,1800,400),expand = c(0,0))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"))

p_NorthAmerica <- ggplot(df_NorthAmerica,aes(Year,samples,fill = class))+
  geom_bar(stat="identity",position="dodge",colour='black',width=0.6)+
  theme_bw()+
  labs(x="p_NorthAmerica",y="Samples")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        legend.text=element_text(size = 12),
        legend.background = element_blank())+
  theme(panel.grid.major = element_line(color = "lightgray",linewidth = 1),
        legend.position = c(0.2,0.7),
        #legend.position = "right",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,1000), breaks=seq(0,1000,250),expand = c(0,0))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"))

p_Oceania <- ggplot(df_Oceania,aes(Year,samples,fill = class))+
  geom_bar(stat="identity",position="dodge",colour='black',width=0.6)+
  theme_bw()+
  labs(x="p_Oceania",y="Samples")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        legend.text=element_text(size = 12),
        legend.background = element_blank())+
  theme(panel.grid.major = element_line(color = "lightgray",linewidth = 1),
        legend.position = "none",
        #legend.position = "right",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,10), breaks=seq(0,10,3),expand = c(0,0))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"))

p_SouthAmerica <- ggplot(df_SouthAmerica,aes(Year,samples,fill = class))+
  geom_bar(stat="identity",position="dodge",colour='black',width=0.6)+
  theme_bw()+
  labs(x="p_SouthAmerica",y="Samples")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        legend.text=element_text(size = 12),
        legend.background = element_blank())+
  theme(panel.grid.major = element_line(color = "lightgray",linewidth = 1),
        legend.position = "none",
        #legend.position = "right",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,30), breaks=seq(0,30,9),expand = c(0,0))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"))

p_Antarctica <- ggplot(df_Antarctica,aes(Year,samples,fill = class))+
  geom_bar(stat="identity",position="dodge",colour='black',width=0.6)+
  theme_bw()+
  labs(x="p_Antarctica",y="Samples")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        legend.text=element_text(size = 12),
        legend.background = element_blank())+
  theme(panel.grid.major = element_line(color = "lightgray",linewidth = 1),
        legend.position = "none",
        #legend.position = "right",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0,3), breaks=seq(0,3,1),expand = c(0,0))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"))

plot_grid(p_Africa,p_Asia,p_Europe,p_NorthAmerica,p_Oceania,p_SouthAmerica,p_Antarctica,
          ncol = 2,align = 'v', axis = 'l', 
          rel_heights = c(1,1))
