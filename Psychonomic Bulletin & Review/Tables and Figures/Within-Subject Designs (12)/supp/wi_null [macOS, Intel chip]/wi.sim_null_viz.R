library(tidyverse)
library(gridExtra)
library(grid)

filepath <- "~/Downloads/Psychonomic Bulletin & Review/R Scripts/Within-Subject Designs (12)/1-params.csv"
params <- read.csv(filepath)

lwr <- 2.9 #1/3.1
upr <- 3.1 #1/2.9
err <- .25

tab.SEP.LM.CI <- tab.SEP.NKM.HDI <- tab.SEP.LH.HDI <- tab.SEP.HPDI <- tab.SEP.anova.HDI <- tab.SEP.JZS.HDI <- matrix(NaN,nrow=12,ncol=6)
for (i in c(1:3,10:12)) {
  filepath <- paste("~/Downloads/Psychonomic Bulletin & Review/Datasets/Within-Subject Designs (12)/wi.sim_null [macOS, Intel chip]/report wi_null_",i,".csv",sep="")
  replication <- read.csv(filepath)
  slicing <- subset(replication,BF>lwr&BF<upr&BFerror<err)
  dat <- abs(slicing$SEP.LM.CI)
  tab.SEP.LM.CI[i,] <- c(length(dat),
                         min(dat),
                         max(dat),
                         mean(dat),
                         median(dat),
                         sd(dat))
  
  dat <- abs(slicing$SEP.NKM.HDI)
  tab.SEP.NKM.HDI[i,] <- c(length(dat),
                           min(dat),
                           max(dat),
                           mean(dat),
                           median(dat),
                           sd(dat))
  
  dat <- abs(slicing$SEP.LH.HDI)
  tab.SEP.LH.HDI[i,] <- c(length(dat),
                          min(dat),
                          max(dat),
                          mean(dat),
                          median(dat),
                          sd(dat))
  
  dat <- abs(slicing$SEP.HPDI)
  tab.SEP.HPDI[i,] <- c(length(dat),
                        min(dat),
                        max(dat),
                        mean(dat),
                        median(dat),
                        sd(dat))
  
  dat <- abs(slicing$SEP.anova.HDI)
  tab.SEP.anova.HDI[i,] <- c(length(dat),
                             min(dat),
                             max(dat),
                             mean(dat),
                             median(dat),
                             sd(dat))
  
  dat <- abs(slicing$SEP.JZS.HDI)
  tab.SEP.JZS.HDI[i,] <- c(length(dat),
                           min(dat),
                           max(dat),
                           mean(dat),
                           median(dat),
                           sd(dat))
}
tab.SEP.LM.CI <- as.data.frame(tab.SEP.LM.CI)
colnames(tab.SEP.LM.CI) <- c("count","min","max","mean","median","sd")

tab.SEP.NKM.HDI <- as.data.frame(tab.SEP.NKM.HDI)
colnames(tab.SEP.NKM.HDI) <- c("count","min","max","mean","median","sd")

tab.SEP.LH.HDI <- as.data.frame(tab.SEP.LH.HDI)
colnames(tab.SEP.LH.HDI) <- c("count","min","max","mean","median","sd")

tab.SEP.HPDI <- as.data.frame(tab.SEP.HPDI)
colnames(tab.SEP.HPDI) <- c("count","min","max","mean","median","sd")

tab.SEP.anova.HDI <- as.data.frame(tab.SEP.anova.HDI)
colnames(tab.SEP.anova.HDI) <- c("count","min","max","mean","median","sd")

tab.SEP.JZS.HDI <- as.data.frame(tab.SEP.JZS.HDI)
colnames(tab.SEP.JZS.HDI) <- c("count","min","max","mean","median","sd")





lwr <- 99
upr <- 101
err <- .25

tab.BF.LM.CI <- tab.BF.NKM.HDI <- tab.BF.LH.HDI <- tab.BF.anova.HDI <- tab.BF.JZS.HDI <- matrix(NaN,nrow=12,ncol=6)
for (i in c(1:3,10:12)) {
  filepath <- paste("~/Downloads/Psychonomic Bulletin & Review/Datasets/Within-Subject Designs (12)/wi.sim_null [macOS, Intel chip]/report wi_null_",i,".csv",sep="")
  replication <- read.csv(filepath)
  dat <- subset(replication,abs(SEP.LM.CI)>lwr&abs(SEP.LM.CI)<upr&BFerror<err)$BF
  tab.BF.LM.CI[i,] <- c(length(dat),
                        min(dat),
                        max(dat),
                        mean(dat),
                        median(dat),
                        sd(dat))
  
  dat <- subset(replication,abs(SEP.NKM.HDI)>lwr&abs(SEP.NKM.HDI)<upr&BFerror<err)$BF
  tab.BF.NKM.HDI[i,] <- c(length(dat),
                          min(dat),
                          max(dat),
                          mean(dat),
                          median(dat),
                          sd(dat))
  
  dat <- subset(replication,abs(SEP.LH.HDI)>lwr&abs(SEP.LH.HDI)<upr&BFerror<err)$BF
  tab.BF.LH.HDI[i,] <- c(length(dat),
                         min(dat),
                         max(dat),
                         mean(dat),
                         median(dat),
                         sd(dat))
  
  dat <- subset(replication,abs(SEP.anova.HDI)>lwr&abs(SEP.anova.HDI)<upr&BFerror<err)$BF
  tab.BF.anova.HDI[i,] <- c(length(dat),
                            min(dat),
                            max(dat),
                            mean(dat),
                            median(dat),
                            sd(dat))
  
  dat <- subset(replication,abs(SEP.JZS.HDI)>lwr&abs(SEP.JZS.HDI)<upr&BFerror<err)$BF
  tab.BF.JZS.HDI[i,] <- c(length(dat),
                          min(dat),
                          max(dat),
                          mean(dat),
                          median(dat),
                          sd(dat))
}
tab.BF.LM.CI <- as.data.frame(tab.BF.LM.CI)
colnames(tab.BF.LM.CI) <- c("count","min","max","mean","median","sd")

tab.BF.NKM.HDI <- as.data.frame(tab.BF.NKM.HDI)
colnames(tab.BF.NKM.HDI) <- c("count","min","max","mean","median","sd")

tab.BF.LH.HDI <- as.data.frame(tab.BF.LH.HDI)
colnames(tab.BF.LH.HDI) <- c("count","min","max","mean","median","sd")

tab.BF.anova.HDI <- as.data.frame(tab.BF.anova.HDI)
colnames(tab.BF.anova.HDI) <- c("count","min","max","mean","median","sd")

tab.BF.JZS.HDI <- as.data.frame(tab.BF.JZS.HDI)
colnames(tab.BF.JZS.HDI) <- c("count","min","max","mean","median","sd")





xTruncated <- 150 #200 for NKM.HDI
yTruncated <- 50; err <- .25
fig.LM.CI <- fig.NKM.HDI <- fig.LH.HDI <- fig.HPDI <- fig.anova.HDI <- fig.JZS.HDI <- vector(mode = "list", length = 12)
for (i in c(1:3,10:12)) {
  filepath <- paste("~/Downloads/Psychonomic Bulletin & Review/Datasets/Within-Subject Designs (12)/wi.sim_null [macOS, Intel chip]/report wi_null_",i,".csv",sep="")
  replication <- read.csv(filepath)
  
  LM.CItable <- data.frame("min"=c(tab.BF.LM.CI$min[i],tab.SEP.LM.CI$min[i]),
                           "max"=c(tab.BF.LM.CI$max[i],tab.SEP.LM.CI$max[i]),
                           "mean"=c(tab.BF.LM.CI$mean[i],tab.SEP.LM.CI$mean[i]),
                           "sd"=c(tab.BF.LM.CI$sd[i],tab.SEP.LM.CI$sd[i]))
  LM.CItable <- round(LM.CItable,1)
  rownames(LM.CItable) <- c("BF","Sep%")
  fig.LM.CI[[i]] <- ggplot(subset(replication,BF<=yTruncated & BFerror<err & abs(SEP.LM.CI)<150),aes(abs(SEP.LM.CI),BF))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated),limits=c(0,50))+
    lims(x=c(0,xTruncated))+
    annotation_custom(tableGrob(format(LM.CItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=20)),xmin=15,xmax=90,ymin=35,ymax=50)+
    labs(x=expression("LM-CI"~italic("Sep%")),y=expression(~italic("BF")[10]))+
    theme_classic()+
    theme(text = element_text(size=23))
  
  NKM.HDItable <- data.frame("min"=c(tab.BF.NKM.HDI$min[i],tab.SEP.NKM.HDI$min[i]),
                             "max"=c(tab.BF.NKM.HDI$max[i],tab.SEP.NKM.HDI$max[i]),
                             "mean"=c(tab.BF.NKM.HDI$mean[i],tab.SEP.NKM.HDI$mean[i]),
                             "sd"=c(tab.BF.NKM.HDI$sd[i],tab.SEP.NKM.HDI$sd[i]))
  NKM.HDItable <- round(NKM.HDItable,1)
  rownames(NKM.HDItable) <- c("BF","Sep%")
  fig.NKM.HDI[[i]] <- ggplot(subset(replication,BF<=yTruncated & BFerror<err),aes(abs(SEP.NKM.HDI),BF))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated),limits=c(0,50))+
    lims(x=c(0,200))+
    annotation_custom(tableGrob(format(NKM.HDItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=20)),xmin=30,xmax=105,ymin=35,ymax=50)+
    labs(x=expression("NKM-HDI"~italic("Sep%")),y=expression(~italic("BF")[10]))+
    theme_classic()+
    theme(text = element_text(size=23))
  
  LH.HDItable <- data.frame("min"=c(tab.BF.LH.HDI$min[i],tab.SEP.LH.HDI$min[i]),
                            "max"=c(tab.BF.LH.HDI$max[i],tab.SEP.LH.HDI$max[i]),
                            "mean"=c(tab.BF.LH.HDI$mean[i],tab.SEP.LH.HDI$mean[i]),
                            "sd"=c(tab.BF.LH.HDI$sd[i],tab.SEP.LH.HDI$sd[i]))
  LH.HDItable <- round(LH.HDItable,1)
  rownames(LH.HDItable) <- c("BF","Sep%")
  fig.LH.HDI[[i]] <- ggplot(subset(replication,BF<=yTruncated & BFerror<err),aes(abs(SEP.LH.HDI),BF))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated),limits=c(0,50))+
    lims(x=c(0,xTruncated))+
    annotation_custom(tableGrob(format(LH.HDItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=20)),xmin=15,xmax=90,ymin=35,ymax=50)+
    labs(x=expression("LH-HDI"~italic("Sep%")),y=expression(~italic("BF")[10]))+
    theme_classic()+
    theme(text = element_text(size=23))
  
  HPDItable <- data.frame("min"=tab.SEP.HPDI$min[i],
                          "max"=tab.SEP.HPDI$max[i],
                          "mean"=tab.SEP.HPDI$mean[i],
                          "sd"=tab.SEP.HPDI$sd[i])
  HPDItable <- round(HPDItable,1)
  rownames(HPDItable) <- c("Sep%")
  fig.HPDI[[i]] <- ggplot(subset(replication,BF<=yTruncated & BFerror<err),aes(abs(SEP.HPDI),BF))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated),limits=c(0,50))+
    lims(x=c(0,xTruncated))+
    annotation_custom(tableGrob(format(HPDItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=20)),xmin=75,xmax=150,ymin=35,ymax=50)+
    labs(x=expression("Standard HDI"~italic("Sep%")),y=expression(~italic("BF")[10]))+
    theme_classic()+
    theme(text = element_text(size=23))
  
  anova.HDItable <- data.frame("min"=c(tab.BF.anova.HDI$min[i],tab.SEP.anova.HDI$min[i]),
                               "max"=c(tab.BF.anova.HDI$max[i],tab.SEP.anova.HDI$max[i]),
                               "mean"=c(tab.BF.anova.HDI$mean[i],tab.SEP.anova.HDI$mean[i]),
                               "sd"=c(tab.BF.anova.HDI$sd[i],tab.SEP.anova.HDI$sd[i]))
  anova.HDItable <- round(anova.HDItable,1)
  rownames(anova.HDItable) <- c("BF","Sep%")
  fig.anova.HDI[[i]] <- ggplot(subset(replication,BF<=yTruncated & BFerror<err),aes(abs(SEP.anova.HDI),BF))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated),limits=c(0,50))+
    lims(x=c(0,xTruncated))+
    annotation_custom(tableGrob(format(anova.HDItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=20)),xmin=15,xmax=90,ymin=35,ymax=50)+
    labs(x=expression("JZS-HDI"~italic("Sep%")~"(BayesFactor)"),y=expression(~italic("BF")[10]))+
    theme_classic()+
    theme(text = element_text(size=23))
  
  JZS.HDItable <- data.frame("min"=c(tab.BF.JZS.HDI$min[i],tab.SEP.JZS.HDI$min[i]),
                             "max"=c(tab.BF.JZS.HDI$max[i],tab.SEP.JZS.HDI$max[i]),
                             "mean"=c(tab.BF.JZS.HDI$mean[i],tab.SEP.JZS.HDI$mean[i]),
                             "sd"=c(tab.BF.JZS.HDI$sd[i],tab.SEP.JZS.HDI$sd[i]))
  JZS.HDItable <- round(JZS.HDItable,1)
  rownames(JZS.HDItable) <- c("BF","Sep%")
  fig.JZS.HDI[[i]] <- ggplot(subset(replication,BF<=yTruncated & BFerror<err),aes(abs(SEP.JZS.HDI),BF))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated),limits=c(0,50))+
    lims(x=c(0,xTruncated))+
    annotation_custom(tableGrob(format(JZS.HDItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=20)),xmin=20,xmax=95,ymin=35,ymax=50)+
    labs(x=expression("JZS-HDI"~italic("Sep%")~"(rmBayes)"),y=expression(~italic("BF")[10]))+
    theme_classic()+
    theme(text = element_text(size=23))
}

i <- 1
fig3a <- grid.arrange(grobs=lapply(list(fig.LM.CI[[i]],fig.NKM.HDI[[i]],fig.LH.HDI[[i]],fig.JZS.HDI[[i]],fig.anova.HDI[[i]],fig.HPDI[[i]]),"+",theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))),ncol=2,
                      top=textGrob(expr(paste("Within-Subject Design, Report ",!!i,"*: ",italic(ρ), "=",!!params$rho[i],italic(", n="),!!params$n[i],sep="")),
                                   gp=gpar(fontsize=27)))
ggsave("~/Desktop/Figure 3.png", fig3a, width=4800, height=4900, units="px", device="png", dpi=300)

