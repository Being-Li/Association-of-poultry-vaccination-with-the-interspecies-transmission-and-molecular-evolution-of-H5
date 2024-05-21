######ccm
rm(list = ls())
library(rEDM)#rEDM 0.7.5v
library(readxl)
library(ggplot2)
library("sampling")
library(cowplot)
library(Rmisc)
library(gmodels)
library(dplyr)

c11=rgb(245,144,116, max = 255)
c22=rgb(235,211,232, max = 255)
c33=rgb(148,144,197, max = 255)
c44=rgb(207,228,183, max = 255)


###########################
to_Wild <- read.csv("\\final_data_to_Wild_MJs.csv")
to_Europe <- read.csv("\\final_data_to_Europe_MJs.csv")

df_to_wild <- aggregate(to_Wild$jump,list(to_Wild$treeId,to_Wild$startLocation,to_Wild$period), sum)
df_to_Europe <- aggregate(to_Europe$jump,list(to_Europe$treeId,to_Europe$startLocation,to_Europe$period), sum)

colnames(df_to_wild)[1:4] <- c("treeId","To","period","jumps")
colnames(df_to_Europe)[1:4] <- c("treeId","To","period","jumps")

dff_to_Wild <- aggregate(df_to_wild$jumps,list(df_to_wild$treeId,df_to_wild$period), sum)
dff_to_Europe <- aggregate(df_to_Europe$jumps,list(df_to_Europe$treeId,df_to_Europe$period), sum)

###########CCM Predicted
colnames(dff_to_Wild)[1:3] = c("treeId","Time","dff_to_Wild")
colnames(dff_to_Europe)[1:3] = c("treeId","Time","dff_to_Europe")

max_year <- 2020
min_year <- 1996
length_year <- max_year-min_year
yearrodent <- full_join(dff_to_Wild[,1:3],dff_to_Europe[,1:3])

yearrodent <- mutate_all(yearrodent, ~replace(., is.na(.), 0))
yearrodent<-yearrodent[yearrodent$Time < max_year & yearrodent$Time >= min_year,]
yearrodent <- arrange(yearrodent, yearrodent$treeId,yearrodent$Time)

treeid_list <- rep(seq(min(yearrodent$treeId),max(yearrodent$treeId),20000),length_year)
treeid_time <- data.frame(treeid_list)
treeid_time$Time <- 0
treeid_time <- arrange(treeid_time, treeid_time$treeid_list)
n = (max(yearrodent$treeId)-min(yearrodent$treeId))/20000+1
treeid_time$Time <- rep(seq(min_year,max_year-1,1),n)
colnames(treeid_time)[1] = "treeId"

yearrodent <- full_join(yearrodent[,1:4],treeid_time[,1:2])
yearrodent <- mutate_all(yearrodent, ~replace(., is.na(.), 0))

treeid <- seq(min(yearrodent$treeId),max(yearrodent$treeId),20000)
treeid_num <- 20
random_treeid <- sample(treeid, treeid_num, replace = FALSE,prob = NULL)
random_state <- filter(yearrodent,yearrodent$treeId %in% random_treeid)

random_state$text <- 0
colnames(random_state)[2] = "period"
random_state <- arrange(random_state, random_state$treeId,random_state$period)
random_state$text <- seq(1,nrow(random_state),1)
colnames(random_state)[5] = "Time"
length = nrow(random_state)

####plot
plot_random_state_ToWild <- random_state[,c(3,5)]
plot_random_state_ToEP <- random_state[,c(4,5)]
plot_random_state_ToWild$direction <- "ToWild"
plot_random_state_ToEP$direction <- "ToEP"
colnames(plot_random_state_ToWild)[1] = "MJs"
colnames(plot_random_state_ToEP)[1] = "MJs"

plot_random_state <- rbind(plot_random_state_ToWild,plot_random_state_ToEP)
p11 <- ggplot(plot_random_state, aes(x = Time, y = MJs, color = direction)) + 
  geom_line(size =1,alpha = 1) + 
  theme_bw()+
  scale_color_manual(values = c(c11,c33))+
  labs(x="randomly selected time series (n=20)",y="markov jumps (per year)")+
  scale_x_continuous(limits = c(0,length), breaks=seq(0,length,25),expand = c(0.01,0))+
  scale_y_continuous(limits = c(0,40), breaks=seq(0,40,10),expand = c(0,0))+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        axis.line.y = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12),
        legend.title=element_blank(),
        panel.grid.major = element_line(colour = "grey80", size = 0.2),
        #panel.grid.minor = element_blank()
        panel.grid.minor = element_line(colour = "grey80", size = 0.2)
  )+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"),
        legend.key.width = unit(0.8,"cm"),
        legend.key.height = unit(0.6,"cm"),
        legend.position = c(0.1,0.9),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA))

##################################################
library(multispatialCCM)

lags <- -10 : 10
result <- data.frame(lags = lags, x_cause_y = NA, y_cause_x = NA)

nrow(data)
times <- 1:11
for (i in 1:length(lags)){
  lag <- lags[i]
  y1 <- times[1:(length(times)-abs(lag))]
  y2 <- times[(abs(lag) + 1):length(times)]
  if (lag > 0){
    xx <- y1
    y1 <- y2
    y2 <- xx
  }
  data1 <- dplyr::filter(data, time %in% y1)
  data2 <- dplyr::filter(data, time %in% y2)
  Accm <- data1$x
  Bccm <- data2$y
  find1 <- which(is.na(Accm))
  find2 <- which(is.na(Bccm))
  if (length(find1) > 0){
    Bccm[find1] <- NA
  }
  if (length(find2) > 0){
    Accm[find2] <- NA
  }
  set.seed(0)
  CCM_boot_A <- CCM_boot(Accm, Bccm, 3, tau=1,DesiredL = 270,iterations = 100)
  result$x_cause_y[i] <- CCM_boot_A$rho
  Bccm <- data1$y
  Accm <- data2$x
  find1 <- which(is.na(Accm))
  find2 <- which(is.na(Bccm))
  if (length(find1) > 0){
    Bccm[find1] <- NA
  }
  if (length(find2) > 0){
    Accm[find2] <- NA
  }
  set.seed(0)
  CCM_boot_B <- CCM_boot(Bccm, Accm, 3, tau=1, DesiredL = 270, iterations = 100)
  result$y_cause_x[i] <- CCM_boot_B$rho
}
result <- reshape2::melt(
  result,
  id.vars = "lags",
  measure.vars = colnames(result)[-1],
  variable.name = "direction",
  value.name = "rho"
)
ggplot(result,
       aes(
         x = lags,
         y = rho,
         group = direction,
         linetype = direction,
         color = direction
       )) + geom_line(size = 1) + 
  theme_bw() + 
  geom_vline(xintercept = 0, color = "gray")

####################################################

vars <- (names(yearrodent)[3:4])  # c('paramecium', 'didinium')

# generate all combinations of lib_column, target_column, tp
params <- expand.grid(lib_column = vars, target_column = vars, tp = -5:5)

# throw out cases where lib == target
params <- params[params$lib_column != params$target_column, ]
params$E <- 2

ccm(random_state, E = 1, lib_sizes = NROW(random_state), 
    random_libs = TRUE, lib_column = "dff_to_Wild", target_column = "dff_to_Europe", 
    tp = -2, silent = TRUE)

output <- do.call(rbind, lapply(seq_len(NROW(params)), function(i) {
  ccm(random_state, E = params$E[i], lib_sizes = NROW(random_state), 
      random_libs = TRUE, lib_column = params$lib_column[i], target_column = params$target_column[i], 
      tp = params$tp[i], silent = TRUE)
}))

output$direction <- paste(output$lib_column, "xmap to", output$target_column)

a <- output %>% group_by(tp,direction) %>%
  dplyr::summarise(mean.rho = mean(rho, na.rm = TRUE),
                   sd.rho = sd(rho, na.rm = TRUE),
                   max.rho = max(rho, na.rm = TRUE),
                   min.rho = min(rho, na.rm = TRUE),
                   n.rho = n()) %>%
  mutate(se.rho = sd.rho / sqrt(n.rho),
         lower.ci.rho = mean.rho - qt(1 - (0.05 / 2), n.rho - 1) * se.rho,
         upper.ci.rho = mean.rho + qt(1 - (0.05 / 2), n.rho - 1) * se.rho)

p3 <- ggplot(a, aes(x = tp, y = mean.rho, color = direction)) + 
  #geom_ribbon(aes(ymin = lower.ci.rho, ymax = upper.ci.rho),alpha = 0.2)+
  #geom_ribbon(aes(ymin = min.rho, ymax = max.rho),alpha = 0.5)+
  geom_vline(xintercept = 0,size = 1,linetype = 2)+
  geom_errorbar(aes(ymin=min.rho, ymax=max.rho), width=0.3,size =1.5) +
  geom_line(size =1.5,alpha = 1) + 
  geom_point(size =3,alpha = 1)+
  theme_bw()+
  scale_color_manual(values = c("gray","black"))+
  #  scale_fill_manual(values = c(c11,c33))+
  labs(x="cross map lag (l)",y="cross map skill (rho)")+
  scale_x_continuous(limits = c(-5.5,5.5), breaks=seq(-5,5,2),expand = c(0,0))+
  scale_y_continuous(limits = c(-0.04,1), breaks=seq(0,1,0.2),expand = c(0.05,0))+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        axis.line.y = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12),
        legend.title=element_blank(),
        panel.grid.major = element_line(colour = "grey80", size = 0.2),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"),
        legend.key.width = unit(0.8,"cm"),
        legend.key.height = unit(0.6,"cm"),
        legend.position = c(0.60,0.15),
        legend.background = element_blank(),
        panel.background = element_rect(fill = NA))

p3

################################################
ts <- random_state$dff_to_Wild
lib <- c(1, length(ts))
pred <- c(1, length(ts))
simplex_output <- simplex(ts, lib, pred, silent = TRUE)
plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
     ylab = "Forecast Skill (rho)")

ts <- random_state$dff_to_Europe
lib <- c(1, length(ts))
pred <- c(1, length(ts))
simplex_output <- simplex(ts, lib, pred, silent = TRUE)
plot(simplex_output$E, simplex_output$rho, type = "l", xlab = "Embedding Dimension (E)", 
     ylab = "Forecast Skill (rho)")

smap_output <- list(s_map(ts, lib, pred, E = 4, silent = TRUE), s_map(ts, lib, 
                                                                      pred, E = 4, silent = TRUE))
plot(smap_output[[1]]$theta, smap_output[[1]]$rho, type = "l", xlim = c(0, 4), 
     xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")
plot(smap_output[[2]]$theta, smap_output[[2]]$rho, type = "l", xlim = c(0, 4), 
     xlab = "Nonlinearity (theta)", ylab = "Forecast Skill (rho)")



################################################
c1=rgb(162,98,172, max = 255)
c2=rgb(132,192,119, max = 255)
c3=rgb(244,132,84, max = 255)

Sys.setlocale("LC_TIME", "English") #print in english
df_2a<-read.csv("D:\\LBY\\fig\\fig2\\fig2a.csv")

plot_2a<- ggplot(df_2a, aes(x = TimeSeries,group = group)) +
  geom_hline(yintercept = 1,size = 1,linetype = 2)+
  geom_ribbon(aes(x = TimeSeries,ymin = lower, ymax = higher,fill = group),alpha = 0.3)+
  geom_line(aes(x = TimeSeries,y = TimePoint,color = group),size = 1.5,alpha = 0.7,stat="identity")+
  #geom_point(aes(x = TimeSeries,y = TimePoint,color = group),size = 3.5,alpha = 0.7,stat="identity")+
  #geom_bar(aes(x = TimeSeries,y = TimePoint,fill = group),size = 0.5,color = "black",alpha = 0.7,stat="identity",position = position_dodge2())+
  #geom_errorbar(aes(x = TimeSeries, ymin = lower, ymax = higher,color = group),size = 1,position = position_dodge(preserve = 'single')) + 
  theme_minimal()+
  theme_bw()+
  scale_fill_manual(values=c(c3,c1)) +
  scale_color_manual(values=c(c3,c1)) +
  labs(x="Time",y="The Markov jumps between host (per year)")+
  theme(axis.title=element_text(size = 14,color = "black"),
        axis.text=element_text(size = 12,color = "black"),
        axis.line.x = element_line(color = "black",size = 0),
        axis.line.y = element_line(color = "black",size = 0),
        legend.text=element_text(size = 12),
        legend.title=element_blank(),
        legend.position=c(0.25,0.8),
        panel.grid.major = element_line(colour = "grey80", size = 0.2),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black",size = 0))+
  scale_x_continuous(limits = c(1995.1,2023), breaks=seq(1996,2023,5),expand = c(0.0,0.0))+
  scale_y_continuous(limits = c(0,75), breaks=seq(0,100,15),expand = c(0.05,0.05))+
  theme(plot.caption = element_text(hjust=0),
        plot.margin = unit(c(1,1,1,1), "lines"),
        legend.key.width = unit(1.0,"cm"),
        legend.key.height = unit(0.7,"cm"),
        legend.background = element_blank())

##########################################################################################
plot_grid(plot_2a,p3, ncol = 2,nrow = 1, align = 'v', axis = 'l', rel_heights = c(1, 1))
