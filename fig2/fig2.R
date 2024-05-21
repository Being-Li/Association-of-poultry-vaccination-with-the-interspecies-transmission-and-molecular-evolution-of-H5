library(treeio)
library(ggtree)
library(ggplot2)
library(lubridate)
library(readxl)
library(cowplot)
rm(list = ls())
Sys.setlocale("LC_TIME", "English") #print in english

###########A#########
c1=rgb(162,98,172, max = 255)
c2=rgb(132,192,119, max = 255)
c3=rgb(244,132,84, max = 255)
c4=rgb(100,100,100, max = 255)

cols<-c('Wild'=c1,'EuropeDomestic'=c2,
        'ChinaDomestic'=c3)

tree1<-read.beast("\\preWildBranch_base_mcc.txt")
tree2<-read.beast("\\ChinaBranch_base_combine_mcc.txt")
tree3<-read.beast("\\WildBranch_base_combine_mcc.txt")
tree4<-read.beast("\\Bangladesh_poultry_mcc.txt")
tree5<-read.beast("\\Indonesia_poultry_L1_mcc.txt")
tree6<-read.beast("\\Indonesia_poultry_L2_mcc.txt")

root1<-read_xlsx("\\tempest.xlsx",sheet = "prewild_branch_root")
root2<-read_xlsx("\\tempest.xlsx",sheet = "china_branch_root")
root3<-read_xlsx("\\tempest.xlsx",sheet = "wild_branch_root")

root4<-read_xlsx("\\tempest.xlsx",sheet = "Bangladesh_root")
root5<-read_xlsx("\\tempest.xlsx",sheet = "Indonesia_L1_root")
root6<-read_xlsx("\\tempest.xlsx",sheet = "Indonesia_L2_root")

colnames(root1)[1] = "age"
colnames(root2)[1] = "age"
colnames(root3)[1] = "age"
colnames(root4)[1] = "age"
colnames(root5)[1] = "age"
colnames(root6)[1] = "age"

dateRanges <- data.frame(
  start = seq(1985.5, 2022.5, 2),
  end = seq(1986.5, 2023.5, 2)
)

p1 <-ggtree(tree1,ladderize=TRUE,
            mrsd = "2022-12-07",size=0.5,alpha=0.5,color = c1) + 
  geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("lightgray"))+
  theme_tree2() +
  labs(x="Time")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12))+
  theme(legend.position = c(0.2,0.6),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size = 12),
        #axis.line.x = element_line(size=1.1),
        #axis.ticks.x = element_line(size=1.1)
        plot.margin = unit(c(1,0,1,0), "lines")) +
  scale_x_continuous(limits = c(1981,2024), breaks=seq(1990,2024,5))+
  scale_y_continuous(expand = c(0.01,0.01)) +
  geom_rootedge(rootedge = 0.1)+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,0,1), "lines"))
mean11 = mean(root1$age)
  p11 <- ggplot(data=root1,aes(x=age)) + 
  theme_bw()+
  geom_vline(xintercept = mean11)+
  geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("lightgray"))+
  geom_density(alpha = 0.4,color = c1,fill = c1,size = 1)+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12))+
  theme(legend.position = c(0.2,0.6),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size = 12),
        #axis.line.x = element_line(size=1.1),
        #axis.ticks.x = element_line(size=1.1)
        plot.margin = unit(c(1,0,1,0), "lines")) +
  scale_x_continuous(limits = c(1981,2024), breaks=seq(1990,2024,5))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,0,1), "lines"))


p2 <-ggtree(tree2,ladderize=TRUE,
            mrsd = "2022-8-12",size=0.5,alpha=1,color = c3) + 
  geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("lightgray"))+
  theme_tree2() +
  labs(x="Time")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12))+
  theme(legend.position = c(0.2,0.6),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size = 12),
        #axis.line.x = element_line(size=1.1),
        #axis.ticks.x = element_line(size=1.1)
        plot.margin = unit(c(1,0,1,0), "lines")) +
  scale_x_continuous(limits = c(1981,2024), breaks=seq(1990,2024,5))+
  scale_y_continuous(expand = c(0.01,0.01)) +
  geom_rootedge(rootedge = 0.1)+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,0,1), "lines"))
mean22 = mean(root2$age)
  p22 <- ggplot(data=root2,aes(x=age)) + 
  theme_bw()+
  geom_vline(xintercept = mean22)+
  geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("lightgray"))+
  geom_density(alpha = 0.4,color = c3,fill = c3,size = 1)+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12))+
  theme(legend.position = c(0.2,0.6),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size = 12),
        #axis.line.x = element_line(size=1.1),
        #axis.ticks.x = element_line(size=1.1)
        plot.margin = unit(c(1,0,1,0), "lines")) +
  scale_x_continuous(limits = c(1981,2024), breaks=seq(1990,2024,5))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,0,1), "lines"))

p3 <-ggtree(tree3,ladderize=TRUE,
            mrsd = "2023-1-16",size=0.5,alpha=1,color = c1) + 
  geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("lightgray"))+
  theme_tree2() +
  labs(x="Time")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12))+
  theme(legend.position = c(0.2,0.6),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size = 12),
        #axis.line.x = element_line(size=1.1),
        #axis.ticks.x = element_line(size=1.1)
        plot.margin = unit(c(1,0,1,0), "lines")) +
  scale_x_continuous(limits = c(1981,2024), breaks=seq(1990,2024,5))+
  scale_y_continuous(expand = c(0.01,0.01)) +
  geom_rootedge(rootedge = 0.1)+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,0,1), "lines"))
mean33 = mean(root3$age)
  p33 <- ggplot(data=root3,aes(x=age)) + 
  theme_bw()+
  geom_vline(xintercept = mean33)+
  geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
            inherit.aes=FALSE, alpha = 0.4, fill = c("lightgray"))+
  geom_density(color = c1,fill = c1,size = 1,alpha = 0.8)+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12))+
  theme(legend.position = c(0.2,0.6),
        panel.grid.major = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size = 12),
        #axis.line.x = element_line(size=1.1),
        #axis.ticks.x = element_line(size=1.1)
        plot.margin = unit(c(1,0,1,0), "lines")) +
  scale_x_continuous(limits = c(1981,2024), breaks=seq(1990,2024,5))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,0,1), "lines"))

p4 <-ggtree(tree4,ladderize=TRUE,
              mrsd = "2022-8-07",size=0.5,alpha=1,color = c4) + 
    geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes=FALSE, alpha = 0.4, fill = c("lightgray"))+
    theme_tree2() +
    labs(x="Time")+
    theme(axis.title=element_text(size = 14,color = "black"),
          axis.text=element_text(size = 12,color = "black"),
          axis.line.x = element_line(color = "black",size = 0),
          legend.text=element_text(size = 12))+
    theme(legend.position = c(0.2,0.6),
          panel.grid.major = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.text = element_text(size = 12),
          #axis.line.x = element_line(size=1.1),
          #axis.ticks.x = element_line(size=1.1)
          plot.margin = unit(c(1,0,1,0), "lines")) +
    scale_x_continuous(limits = c(1981,2024), breaks=seq(1990,2024,5))+
    scale_y_continuous(expand = c(0.01,0.01)) +
    geom_rootedge(rootedge = 0.1)+
    theme(plot.caption = element_text(hjust=0),
          plot.margin = unit(c(1,1,0,1), "lines"))
  mean44 = mean(root4$age)
  p44 <- ggplot(data=root4,aes(x=age)) + 
    theme_bw()+
    geom_vline(xintercept = mean44)+
    geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes=FALSE, alpha = 0.4, fill = c("lightgray"))+
    geom_density(color = c4,fill = c4,size = 1,alpha = 0.8)+
    theme(axis.title=element_text(size = 14,color = "black"),
          axis.text=element_text(size = 12,color = "black"),
          axis.line.x = element_line(color = "black",size = 0),
          legend.text=element_text(size = 12))+
    theme(legend.position = c(0.2,0.6),
          panel.grid.major = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.text = element_text(size = 12),
          #axis.line.x = element_line(size=1.1),
          #axis.ticks.x = element_line(size=1.1)
          plot.margin = unit(c(1,0,1,0), "lines")) +
    scale_x_continuous(limits = c(1981,2024), breaks=seq(1990,2024,5))+
    theme(plot.caption = element_text(hjust=0),
          plot.margin = unit(c(1,1,0,1), "lines"))

  p5 <-ggtree(tree5,ladderize=TRUE,
              mrsd = "2022-3-05",size=0.5,alpha=1,color = c4) + 
    geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes=FALSE, alpha = 0.4, fill = c("lightgray"))+
    theme_tree2() +
    labs(x="Time")+
    theme(axis.title=element_text(size = 14,color = "black"),
          axis.text=element_text(size = 12,color = "black"),
          axis.line.x = element_line(color = "black",size = 0),
          legend.text=element_text(size = 12))+
    theme(legend.position = c(0.2,0.6),
          panel.grid.major = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.text = element_text(size = 12),
          #axis.line.x = element_line(size=1.1),
          #axis.ticks.x = element_line(size=1.1)
          plot.margin = unit(c(1,0,1,0), "lines")) +
    scale_x_continuous(limits = c(1981,2024), breaks=seq(1990,2024,5))+
    scale_y_continuous(expand = c(0.01,0.01)) +
    geom_rootedge(rootedge = 0.1)+
    theme(plot.caption = element_text(hjust=0),
          plot.margin = unit(c(1,1,0,1), "lines"))
  mean55 = mean(root5$age)
  p55 <- ggplot(data=root5,aes(x=age)) + 
    theme_bw()+
    geom_vline(xintercept = mean55)+
    geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes=FALSE, alpha = 0.4, fill = c("lightgray"))+
    geom_density(color = c4,fill = c4,size = 1,alpha = 0.8)+
    theme(axis.title=element_text(size = 14,color = "black"),
          axis.text=element_text(size = 12,color = "black"),
          axis.line.x = element_line(color = "black",size = 0),
          legend.text=element_text(size = 12))+
    theme(legend.position = c(0.2,0.6),
          panel.grid.major = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.text = element_text(size = 12),
          #axis.line.x = element_line(size=1.1),
          #axis.ticks.x = element_line(size=1.1)
          plot.margin = unit(c(1,0,1,0), "lines")) +
    scale_x_continuous(limits = c(1981,2024), breaks=seq(1990,2024,5))+
    theme(plot.caption = element_text(hjust=0),
          plot.margin = unit(c(1,1,0,1), "lines"))
  
  p6 <-ggtree(tree6,ladderize=TRUE,
              mrsd = "2014-10-29",size=0.5,alpha=1,color = c4) + 
    geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes=FALSE, alpha = 0.4, fill = c("lightgray"))+
    theme_tree2() +
    labs(x="Time")+
    theme(axis.title=element_text(size = 14,color = "black"),
          axis.text=element_text(size = 12,color = "black"),
          axis.line.x = element_line(color = "black",size = 0),
          legend.text=element_text(size = 12))+
    theme(legend.position = c(0.2,0.6),
          panel.grid.major = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.text = element_text(size = 12),
          #axis.line.x = element_line(size=1.1),
          #axis.ticks.x = element_line(size=1.1)
          plot.margin = unit(c(1,0,1,0), "lines")) +
    scale_x_continuous(limits = c(1981,2024), breaks=seq(1990,2024,5))+
    scale_y_continuous(expand = c(0.01,0.01)) +
    geom_rootedge(rootedge = 0.1)+
    theme(plot.caption = element_text(hjust=0),
          plot.margin = unit(c(1,1,0,1), "lines"))
  mean66 = mean(root6$age)
  p66 <- ggplot(data=root6,aes(x=age)) + 
    theme_bw()+
    geom_vline(xintercept = mean66)+
    geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
              inherit.aes=FALSE, alpha = 0.4, fill = c("lightgray"))+
    geom_density(color = c4,fill = c4,size = 1,alpha = 0.8)+
    theme(axis.title=element_text(size = 14,color = "black"),
          axis.text=element_text(size = 12,color = "black"),
          axis.line.x = element_line(color = "black",size = 0),
          legend.text=element_text(size = 12))+
    theme(legend.position = c(0.2,0.6),
          panel.grid.major = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.text = element_text(size = 12),
          #axis.line.x = element_line(size=1.1),
          #axis.ticks.x = element_line(size=1.1)
          plot.margin = unit(c(1,0,1,0), "lines")) +
    scale_x_continuous(limits = c(1981,2024), breaks=seq(1990,2024,5))+
    theme(plot.caption = element_text(hjust=0),
          plot.margin = unit(c(1,1,0,1), "lines"))
  
  
plot_grid(p33,p3,p22,p2,p11,p1,
          nrow = 6,align = 'v', axis = 'l', 
          rel_heights = c(0.5,1,0.5, 1,0.5,1))

plot_grid(p66,p6,p55,p5,p44,p4,
          nrow = 6,align = 'v', axis = 'l', 
          rel_heights = c(0.5,1,0.5, 1,0.5,1))



###########A#########
rm(list = ls())
library(ggsignif)
c1=rgb(162,98,172, max = 255)
c2=rgb(208,176,213, max = 255)
c3=rgb(244,132,84, max = 255)
c4=rgb(200,200,200, max = 255)

df<-read_xlsx("\\meanrate.xlsx",sheet = "Sheet1")
Data_summary<-read_xlsx("\\meanrate.xlsx",sheet = "Data_summary")

df$class <- factor(df$class,levels = c("preWild_branch","china_branch","wild_branch","CP_PB2","Wild_PB2"))
colss<-c('wild_branch'=c1,'preWild_branch'=c2,
         'china_branch'=c3,"CP_PB2" = c3, "Wild_PB2" = c1)
compaired <- list(c("preWild_branch", "china_branch"),
                   c("china_branch","wild_branch"),
                   c("preWild_branch","wild_branch"),
                   c("CP_PB2","Wild_PB2"))



p2 <- ggplot(data=df,aes(x=class,y= meanRate,fill = class)) + 
  geom_violin(alpha = 0.8)+
  geom_point(data = Data_summary,aes(x=class, y= meanRate),pch=21,size=3)+ #绘制均值为点图
  geom_errorbar(data = Data_summary,aes(ymin = low95, ymax = high95), #误差条表示95%的置信区间
                width=0.1, #误差条末端短横线的宽度
                color="black",
                alpha = 0.7,
                linewidth=1) +
  #geom_boxplot(alpha = 0.8,outlier.size = 1)+
  geom_signif(comparisons = compaired,
              y_position = c(0.006,0.0070,0.0073,0.0045),
              #tip_length = 0.01,
              tip_length = 0,
              map_signif_level = T,size = 1,textsize = 5,
              test = wilcox.test)+
  theme_bw()+
  labs(x="Branch",y="")+
  scale_fill_manual(values = c(colss))+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        legend.text=element_text(size = 12),
        legend.background = element_blank())+
  theme(panel.grid.major = element_line(color = "lightgray",linewidth = 1),
        legend.position = c(0.8,0.8),
        #legend.position = "right",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0.002,0.0085), breaks=seq(0.003,0.0075,0.0015),expand = c(0,0))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"))
p2




#####################################333
rm(list = ls())
library(ggsignif)
c1=rgb(162,98,172, max = 255)
c2=rgb(208,176,213, max = 255)
c3=rgb(244,132,84, max = 255)


c22=rgb(135,101,144, max = 255)
c11=rgb(202,158,45, max = 255)
c33=rgb(58,141,154, max = 255)




df<-read_xlsx("\\meanrate.xlsx",sheet = "supplement")
Data_summary<-read_xlsx("\\meanrate.xlsx",sheet = "Data_summary_cp")

df$class <- factor(df$class,levels = c("preWild_branch","china_branch","wild_branch",
                                       "BangladeshPoultry",
                                       "Indonesia_poultry_L1","Indonesia_poultry_L2"))
cols<-c('wild_branch'=c1,'preWild_branch'=c2,
        'china_branch'=c3,'BangladeshPoultry'=c11,
        'Indonesia_poultry_L1'=c22,'Indonesia_poultry_L2'=c33)

compaired <- list(c("preWild_branch", "china_branch"),
                  c("china_branch","wild_branch"),
                  c("preWild_branch","wild_branch"),
                  c("china_branch","BangladeshPoultry"),
                  c("china_branch","Indonesia_poultry_L1"),
                  c("china_branch","Indonesia_poultry_L2"),
                  c("Indonesia_poultry_L1","Indonesia_poultry_L2"),
                  c("BangladeshPoultry","Indonesia_poultry_L2"),
                  c("Indonesia_poultry_L1","BangladeshPoultry"),
                  
                  c("preWild_branch","BangladeshPoultry"),
                  c("preWild_branch","Indonesia_poultry_L1"),
                  c("preWild_branch","Indonesia_poultry_L2")
                  
                  
                  )


'''
geom_signif(comparisons = compaired,
              y_position = c(0.006,0.0071,0.0076,0.0081,0.0086,0.0091,0.0071,
                             0.003,0.0032,0.0034,
                             0.0035,0.0037,0.0038),
              #tip_length = 0.01,
              tip_length = 0,
              map_signif_level = T,size = 1,textsize = 5,
              test = wilcox.test)+
'''

p2 <- ggplot(data=df,aes(x=class,y= meanRate,fill = class)) + 
  geom_violin(alpha = 1)+
  geom_point(data = Data_summary,aes(x=class, y= meanRate),size=2,position = position_dodge( .9))+ #绘制均值为点图
  geom_errorbar(data = Data_summary,aes(ymin = low95, ymax = high95), #误差条表示95%的置信区间
                width=0.2, #误差条末端短横线的宽度
                color="black",
                alpha = 1,
                linewidth=1,position = position_dodge( .9)) +
  theme_bw()+
  labs(x="Branch",y="")+
  scale_fill_manual(values = c(cols))+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        legend.text=element_text(size = 12),
        legend.background = element_blank())+
  theme(panel.grid.major = element_line(color = "lightgray",size = 1),
        legend.position = c(0.8,0.8),
        #legend.position = "right",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0.001,0.011), breaks=seq(0.002,0.01,0.002),expand = c(0,0))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"))
################
wild_branch_dense = data.frame(density(subset(df,class == "wild_branch")$meanRate)[c('x','y')])
preWild_branch_dense = data.frame(density(subset(df,class == "preWild_branch")$meanRate)[c('x','y')])
china_branch_dense = data.frame(density(subset(df,class == "china_branch")$meanRate)[c('x','y')])
BangladeshPoultry_dense = data.frame(density(subset(df,class == "BangladeshPoultry")$meanRate)[c('x','y')])
Indonesia_poultry_L1_dense = data.frame(density(subset(df,class == "Indonesia_poultry_L1")$meanRate)[c('x','y')])
Indonesia_poultry_L2_dense = data.frame(density(subset(df,class == "Indonesia_poultry_L2")$meanRate)[c('x','y')])



#############
c5=rgb(200,200,200, max = 255)


p1 <- ggplot(data=subset(df,df$class=="wild_branch" |df$class=="preWild_branch" |df$class=="china_branch"),aes(x= meanRate,fill = class)) + 
  geom_density(alpha = 0.8)+
  geom_area(data = subset(wild_branch_dense,x >= 0.0053130 & x < 0.0065594)
            ,aes(x = x,y, fill = c5),alpha= 0.5)+
  geom_area(data = subset(preWild_branch_dense,x >= 0.0029665 & x < 0.0038265)
            ,aes(x = x,y, fill = c5),alpha= 0.5)+
  geom_area(data = subset(china_branch_dense,x >= 0.0050193 & x < 0.0057632)
            ,aes(x = x,y, fill = c5),alpha= 0.5)+
  geom_vline(xintercept  = 0.0059046,alpha= 1)+
  geom_vline(xintercept  = 0.0053839,alpha= 1)+
  geom_vline(xintercept  = 0.0033869,alpha= 1)+
  theme_bw()+
  labs(x="Branch",y="")+
  scale_fill_manual(values = c(cols))+
  scale_color_manual(values = c(cols))+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        legend.text=element_text(size = 12),
        legend.background = element_blank())+
  theme(panel.grid.major = element_line(color = "lightgray",linewidth = 1),
        legend.position = c(0.2,0.8),
        #legend.position = "right",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(limits = c(0.0018,0.0082), breaks=seq(0.002,0.01,0.001),expand = c(0,0))+
  scale_y_continuous(limits = c(0,2200), breaks=seq(0,2500,500),expand = c(0.02,0))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"))+
  coord_fixed(ratio=0.000001)

p2 <- ggplot(data=subset(df,df$class=="BangladeshPoultry" |df$class=="Indonesia_poultry_L1" |df$class=="Indonesia_poultry_L2"),
       aes(x= meanRate,fill = class)) + 
  geom_density(alpha = 0.8)+
  geom_area(data = subset(BangladeshPoultry_dense,x >= 0.0039720 & x < 0.0054180)
            ,aes(x = x,y, fill = c5),alpha= 0.5)+
  geom_area(data = subset(Indonesia_poultry_L1_dense,x >= 0.0045340 & x < 0.0061538)
            ,aes(x = x,y, fill = c5),alpha= 0.5)+
  geom_area(data = subset(Indonesia_poultry_L2_dense,x >= 0.0042761 & x < 0.0060808)
            ,aes(x = x,y, fill = c5),alpha= 0.5)+
  geom_vline(xintercept  = 0.0047050,alpha= 1)+
  geom_vline(xintercept  = 0.0053080,alpha= 1)+
  geom_vline(xintercept  = 0.0050780,alpha= 1)+
  theme_bw()+
  labs(x="Branch",y="")+
  scale_fill_manual(values = c(cols))+
  scale_color_manual(values = c(cols))+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        legend.text=element_text(size = 12),
        legend.background = element_blank())+
  theme(panel.grid.major = element_line(color = "lightgray",linewidth = 1),
        legend.position = c(0.2,0.8),
        #legend.position = "right",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(limits = c(0.0018,0.0082), breaks=seq(0.002,0.01,0.001),expand = c(0,0))+
  scale_y_continuous(limits = c(0,1500), breaks=seq(0,1200,400),expand = c(0.02,0))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"))+
  coord_fixed(ratio=0.000001)

p1/p2





