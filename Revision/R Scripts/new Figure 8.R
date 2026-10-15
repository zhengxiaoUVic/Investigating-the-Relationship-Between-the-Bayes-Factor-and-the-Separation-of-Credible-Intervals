setwd("~/Downloads/Psychonomic Bulletin & Review") #YOUR PATH
filepath <- "R Scripts/Within-Subject Designs (12)/1-params.csv"
params <- read.csv(filepath)
#     n mu.b delta.mu sigma.b sigma.e rho power
# 1  24  100      6.7      20    6.67 0.9   0.8
# 2  24  100     13.1      20   13.10 0.7   0.8
# 3  24  100     28.6      20   30.55 0.3   0.8
# 4  24  100      3.8      20    6.67 0.9   0.3
# 5  24  100      7.5      20   13.10 0.7   0.3
# 6  24  100     16.5      20   30.55 0.3   0.3
# 7  48  100      4.6      20    6.67 0.9   0.8
# 8  48  100      9.0      20   13.10 0.7   0.8
# 9  48  100     20.4      20   30.55 0.3   0.8
# 10 48  100      2.7      20    6.67 0.9   0.3
# 11 48  100      5.4      20   13.10 0.7   0.3
# 12 48  100     12.0      20   30.55 0.3   0.3

num <- 1
######SEP and BF benchmarks
BF.lwr <- 2.9; BF.upr <- 3.1; SEP.lwr <- 99; SEP.upr <- 101; err <- .01

tab.SEP.LH.HDI2 <- tab.SEP.LH.HDI3 <- tab.SEP.JZS.HDI2 <- tab.SEP.JZS.HDI3 <- 
  tab.BF.LH.HDI2 <- tab.BF.LH.HDI3 <- tab.BF.JZS.HDI2 <- tab.BF.JZS.HDI3 <- matrix(NaN,nrow=num,ncol=6)

for (i in c(1:num)) {
  filepath <- paste("Datasets/Within-Subject Designs (12)/report sa_",i,".csv",sep="")
  replication <- read.csv(filepath)
  filepath <- paste("Datasets/Within-Subject Designs (12)/report wi_",i,".csv",sep="")
  replication2 <- read.csv(filepath)
  replication <- cbind(replication,replication2[c("BF","BFerror")])
  slicing <- subset(replication,BF>BF.lwr&BF<BF.upr&BFerror<err)
  
  dat <- slicing$SEP.LH.HDI2
  tab.SEP.LH.HDI2[i,] <- c(length(dat),
                           min(dat),
                           max(dat),
                           mean(dat),
                           median(dat),
                           sd(dat))
  
  dat <- slicing$SEP.LH.HDI3
  tab.SEP.LH.HDI3[i,] <- c(length(dat),
                           min(dat),
                           max(dat),
                           mean(dat),
                           median(dat),
                           sd(dat))
  
  dat <- slicing$SEP.JZS.HDI2
  tab.SEP.JZS.HDI2[i,] <- c(length(dat),
                            min(dat),
                            max(dat),
                            mean(dat),
                            median(dat),
                            sd(dat))
  
  dat <- slicing$SEP.JZS.HDI3
  tab.SEP.JZS.HDI3[i,] <- c(length(dat),
                            min(dat),
                            max(dat),
                            mean(dat),
                            median(dat),
                            sd(dat))
  
  dat <- subset(replication,SEP.LH.HDI2>SEP.lwr&SEP.LH.HDI2<SEP.upr&BFerror<err)$BF
  tab.BF.LH.HDI2[i,] <- c(length(dat),
                          min(dat),
                          max(dat),
                          mean(dat),
                          median(dat),
                          sd(dat))
  
  dat <- subset(replication,SEP.LH.HDI3>SEP.lwr&SEP.LH.HDI3<SEP.upr&BFerror<err)$BF
  tab.BF.LH.HDI3[i,] <- c(length(dat),
                          min(dat),
                          max(dat),
                          mean(dat),
                          median(dat),
                          sd(dat))
  
  dat <- subset(replication,SEP.JZS.HDI2>SEP.lwr&SEP.JZS.HDI2<SEP.upr&BFerror<err)$BF
  tab.BF.JZS.HDI2[i,] <- c(length(dat),
                           min(dat),
                           max(dat),
                           mean(dat),
                           median(dat),
                           sd(dat))
  
  dat <- subset(replication,SEP.JZS.HDI3>SEP.lwr&SEP.JZS.HDI3<SEP.upr&BFerror<err)$BF
  tab.BF.JZS.HDI3[i,] <- c(length(dat),
                           min(dat),
                           max(dat),
                           mean(dat),
                           median(dat),
                           sd(dat))
}
{
  tab.SEP.LH.HDI2 <- as.data.frame(tab.SEP.LH.HDI2)
  colnames(tab.SEP.LH.HDI2) <- c("count","min","max","mean","median","sd")
  
  tab.SEP.LH.HDI3 <- as.data.frame(tab.SEP.LH.HDI3)
  colnames(tab.SEP.LH.HDI3) <- c("count","min","max","mean","median","sd")
  
  tab.SEP.JZS.HDI2 <- as.data.frame(tab.SEP.JZS.HDI2)
  colnames(tab.SEP.JZS.HDI2) <- c("count","min","max","mean","median","sd")
  
  tab.SEP.JZS.HDI3 <- as.data.frame(tab.SEP.JZS.HDI3)
  colnames(tab.SEP.JZS.HDI3) <- c("count","min","max","mean","median","sd")
  
  tab.BF.LH.HDI2 <- as.data.frame(tab.BF.LH.HDI2)
  colnames(tab.BF.LH.HDI2) <- c("count","min","max","mean","median","sd")
  
  tab.BF.LH.HDI3 <- as.data.frame(tab.BF.LH.HDI3)
  colnames(tab.BF.LH.HDI3) <- c("count","min","max","mean","median","sd")
  
  tab.BF.JZS.HDI2 <- as.data.frame(tab.BF.JZS.HDI2)
  colnames(tab.BF.JZS.HDI2) <- c("count","min","max","mean","median","sd")
  
  tab.BF.JZS.HDI3 <- as.data.frame(tab.BF.JZS.HDI3)
  colnames(tab.BF.JZS.HDI3) <- c("count","min","max","mean","median","sd")
}

#####Visualization
library(tidyverse)
library(gridExtra)
library(grid)
theme.text.size <- 23; ann.tbl.size <- 17; geom.pt.size <- 0.5

xTruncated <- 150; yTruncated <- 50
fig.LH.HDI2 <- fig.LH.HDI3 <- fig.JZS.HDI2 <- fig.JZS.HDI3 <- vector(mode = "list", length = num)
for (i in c(1:num)) {
  filepath <- paste("Datasets/Within-Subject Designs (12)/report sa_",i,".csv",sep="")
  replication <- read.csv(filepath)
  filepath <- paste("Datasets/Within-Subject Designs (12)/report wi_",i,".csv",sep="")
  replication2 <- read.csv(filepath)
  replication <- cbind(replication,replication2[c("BF","BFerror")])
  
  LH.HDItable2 <- data.frame("min"=c(tab.BF.LH.HDI2$min[i],tab.SEP.LH.HDI2$min[i]),
                             "max"=c(tab.BF.LH.HDI2$max[i],tab.SEP.LH.HDI2$max[i]),
                             "mean"=c(tab.BF.LH.HDI2$mean[i],tab.SEP.LH.HDI2$mean[i]),
                             "sd"=c(tab.BF.LH.HDI2$sd[i],tab.SEP.LH.HDI2$sd[i]))
  LH.HDItable2 <- round(LH.HDItable2,1)
  rownames(LH.HDItable2) <- c("BF","Sep%")
  fig.LH.HDI2[[i]] <- ggplot(subset(replication,BF<=yTruncated & BFerror<err),aes(abs(SEP.LH.HDI2),BF))+
    geom_point(size=geom.pt.size,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated))+
    lims(x=c(0,xTruncated))+
    annotation_custom(tableGrob(format(LH.HDItable2,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=ann.tbl.size)),xmin=15,xmax=90,ymin=35,ymax=50)+
    labs(x=expression("LH-HDI"~italic("Sep%")),y=expression(~italic("BF")[10]))+
    ggtitle("Standard uniform priors")+
    theme_classic()+
    theme(axis.text=element_text(size=theme.text.size-5),
          axis.title=element_text(size=theme.text.size),
          plot.title=element_text(size=theme.text.size-5))
  
  LH.HDItable3 <- data.frame("min"=c(tab.BF.LH.HDI3$min[i],tab.SEP.LH.HDI3$min[i]),
                             "max"=c(tab.BF.LH.HDI3$max[i],tab.SEP.LH.HDI3$max[i]),
                             "mean"=c(tab.BF.LH.HDI3$mean[i],tab.SEP.LH.HDI3$mean[i]),
                             "sd"=c(tab.BF.LH.HDI3$sd[i],tab.SEP.LH.HDI3$sd[i]))
  LH.HDItable3 <- round(LH.HDItable3,1)
  rownames(LH.HDItable3) <- c("BF","Sep%")
  fig.LH.HDI3[[i]] <- ggplot(subset(replication,BF<=yTruncated & BFerror<err),aes(abs(SEP.LH.HDI3),BF))+
    geom_point(size=geom.pt.size,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated))+
    lims(x=c(0,xTruncated))+
    annotation_custom(tableGrob(format(LH.HDItable3,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=ann.tbl.size)),xmin=25,xmax=100,ymin=35,ymax=50)+
    labs(x=expression("LH-HDI"~italic("Sep%")),y=expression(~italic("BF")[10]))+
    ggtitle("Standard half-Cauchy priors")+
    theme_classic()+
    theme(axis.text=element_text(size=theme.text.size-5),
          axis.title=element_text(size=theme.text.size),
          plot.title=element_text(size=theme.text.size-5))
  
  JZS.HDItable2 <- data.frame("min"=c(tab.BF.JZS.HDI2$min[i],tab.SEP.JZS.HDI2$min[i]),
                              "max"=c(tab.BF.JZS.HDI2$max[i],tab.SEP.JZS.HDI2$max[i]),
                              "mean"=c(tab.BF.JZS.HDI2$mean[i],tab.SEP.JZS.HDI2$mean[i]),
                              "sd"=c(tab.BF.JZS.HDI2$sd[i],tab.SEP.JZS.HDI2$sd[i]))
  JZS.HDItable2 <- round(JZS.HDItable2,1)
  rownames(JZS.HDItable2) <- c("BF","Sep%")
  fig.JZS.HDI2[[i]] <- ggplot(subset(replication,BF<=yTruncated & BFerror<err),aes(abs(SEP.JZS.HDI2),BF))+
    geom_point(size=geom.pt.size,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated))+
    lims(x=c(0,xTruncated))+
    annotation_custom(tableGrob(format(JZS.HDItable2,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=ann.tbl.size)),xmin=20,xmax=95,ymin=35,ymax=50)+
    labs(x=expression("JZS-HDI"~italic("Sep%")),y=expression(~italic("BF")[10]))+
    ggtitle("Standard uniform priors")+
    theme_classic()+
    theme(axis.text=element_text(size=theme.text.size-5),
          axis.title=element_text(size=theme.text.size),
          plot.title=element_text(size=theme.text.size-5))
  
  JZS.HDItable3 <- data.frame("min"=c(tab.BF.JZS.HDI3$min[i],tab.SEP.JZS.HDI3$min[i]),
                              "max"=c(tab.BF.JZS.HDI3$max[i],tab.SEP.JZS.HDI3$max[i]),
                              "mean"=c(tab.BF.JZS.HDI3$mean[i],tab.SEP.JZS.HDI3$mean[i]),
                              "sd"=c(tab.BF.JZS.HDI3$sd[i],tab.SEP.JZS.HDI3$sd[i]))
  JZS.HDItable3 <- round(JZS.HDItable3,1)
  rownames(JZS.HDItable3) <- c("BF","Sep%")
  fig.JZS.HDI3[[i]] <- ggplot(subset(replication,BF<=yTruncated & BFerror<err),aes(abs(SEP.JZS.HDI3),BF))+
    geom_point(size=geom.pt.size,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated))+
    lims(x=c(0,xTruncated))+
    annotation_custom(tableGrob(format(JZS.HDItable3,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=ann.tbl.size)),xmin=25,xmax=100,ymin=35,ymax=50)+
    labs(x=expression("JZS-HDI"~italic("Sep%")),y=expression(~italic("BF")[10]))+
    ggtitle("Standard half-Cauchy priors")+
    theme_classic()+
    theme(axis.text=element_text(size=theme.text.size-5),
          axis.title=element_text(size=theme.text.size),
          plot.title=element_text(size=theme.text.size-5))
}

i <- 1
fig.sa <- grid.arrange(grobs=lapply(list(fig.LH.HDI2[[i]],fig.LH.HDI3[[i]]),"+",theme(plot.margin=unit(c(1,0.35,0,0.35),"cm"))),ncol=2,
                       top=textGrob(expr(paste("Within-Subject Design, Case ",!!i,": ",italic(ρ), "=",!!params$rho[i],italic(", n="),!!params$n[i],sep="")),
                                    gp=gpar(fontsize=theme.text.size)))
ggsave("~/Desktop/Figure 8.png", fig.sa, width=3740, height=1650, units="px", device="png", dpi=300)


##CACHE
fig.sa <- grid.arrange(grobs=lapply(list(fig.JZS.HDI2[[i]],fig.JZS.HDI3[[i]]),"+",theme(plot.margin=unit(c(1,0.35,0,0.35),"cm"))),ncol=2,
                       top=textGrob(expr(paste("Within-Subject Design, Report ",!!i,": ",italic(ρ), "=",!!params$rho[i],", power=",!!params$power[i],italic(", n="),!!params$n[i],sep="")),
                                    gp=gpar(fontsize=theme.text.size)))
ggsave("~/Desktop/Figure 8B.png", fig.sa, width=3740, height=1650, units="px", device="png", dpi=300)

