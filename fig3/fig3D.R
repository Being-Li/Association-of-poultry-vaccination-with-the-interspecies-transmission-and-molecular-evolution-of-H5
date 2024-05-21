library(ape)
library(readxl)
library(ggplot2)
library(ggpubr)

######################
data <- read.csv("\\main_HA_CP_bootstraps_n_1000_adaptations.csv")

ggplot() +
  geom_point(data = data, aes(x = Time, y = Median),size = 2) +
  geom_line(data = data, aes(x = Time, y = Median)) +
  geom_errorbar(data = data, aes(x = Time, ymin = Lower.Quartile, ymax = Upper.Quartile), width = 0.5, linewidth = 0.5) +
  labs(title = "",
       x = "Time",
       y = "The number of adaptive substitutions") +
  scale_y_continuous(limits = c(-3,35), breaks=seq(0,35,7),expand = c(0,0))+
  scale_x_continuous(limits = c(1998,2020), breaks=seq(1999,2020,2),expand = c(0,0))+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        axis.line.y = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12),
        legend.title=element_blank(),
        panel.grid.major = element_line(colour = "grey80", size = 0.2),
        #panel.grid.minor = element_blank()
        #panel.grid.minor = element_line(colour = "grey80", size = 0.2)
  )+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"),
        legend.key.width = unit(0.8,"cm"),
        legend.key.height = unit(0.6,"cm"),
        legend.position=c(0.18,0.82),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))
#+
#  stat_smooth(data = data[c(1:4),], aes(x = Time, y = Median), method = lm, formula = y ~ x)+
#  stat_poly_eq(data = data[c(1:4),], aes(x = Time, y = Median, label=paste(..eq.label..,..adj.rr.label..,..p.value.label..,sep = "~~~~")),
#               formula = y ~ x,parse=T,
#               label.x = 0.05,label.y = 0.5,size = 4)+
#  stat_smooth(data = data[c(4:11),], aes(x = Time, y = Median), method = lm, formula = y ~ x)+
#  stat_poly_eq(data = data[c(4:11),], aes(x = Time, y = Median, label=paste(..eq.label..,..adj.rr.label..,..p.value.label..,sep = "~~~~")),
#             formula = y ~ x,parse=T,
#             label.x = 0.9,label.y = 0.2,size = 4)
  

library(ggpmisc)

stage_fit_1<-lm (Mean ~  Time, data = data[c(1:4),])
summary(stage_fit_1)
stage_fit_2<-lm (Mean ~  Time, data = data[c(4:11),])
summary(stage_fit_2)