setwd("~/Downloads/Psychonomic Bulletin & Review") #YOUR PATH
source("R Scripts/Within-Subject Designs (12)/4-BF benchmarks.R",echo=F)
source("R Scripts/Within-Subject Designs (12)/3-SEP benchmarks.R",echo=F)

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

library(tidyverse)
library(gridExtra)
library(grid)

xTruncated <- 150 #200 for NKM.HDI
yTruncated <- 50; err <- .25
fig.LM.CI <- fig.NKM.HDI <- fig.LH.HDI <- fig.HPDI <- fig.anova.HDI <- fig.JZS.HDI <- vector(mode = "list", length = 12)
for (i in c(1:12)) {
  filepath <- paste("Datasets/Within-Subject Designs (12)/report wi_",i,".csv",sep="")
  replication <- read.csv(filepath)
  
  LM.CItable <- data.frame("min"=c(tab.BF.LM.CI$min[i],tab.SEP.LM.CI$min[i]),
                           "max"=c(tab.BF.LM.CI$max[i],tab.SEP.LM.CI$max[i]),
                           "mean"=c(tab.BF.LM.CI$mean[i],tab.SEP.LM.CI$mean[i]),
                           "sd"=c(tab.BF.LM.CI$sd[i],tab.SEP.LM.CI$sd[i]))
  LM.CItable <- round(LM.CItable,1)
  rownames(LM.CItable) <- c("BF","Sep%")
  fig.LM.CI[[i]] <- ggplot(subset(replication,BF<=yTruncated & BFerror<err),aes(abs(SEP.LM.CI),BF))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated))+
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
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated))+
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
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated))+
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
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated))+
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
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated))+
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
    scale_y_continuous(breaks=c(0,3,10,20,30,40,yTruncated))+
    lims(x=c(0,xTruncated))+
    annotation_custom(tableGrob(format(JZS.HDItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=20)),xmin=20,xmax=95,ymin=35,ymax=50)+
    labs(x=expression("JZS-HDI"~italic("Sep%")~"(rmBayes)"),y=expression(~italic("BF")[10]))+
    theme_classic()+
    theme(text = element_text(size=23))
}

i <- 1
fig3a <- grid.arrange(grobs=lapply(list(fig.LM.CI[[i]],fig.NKM.HDI[[i]],fig.LH.HDI[[i]],fig.JZS.HDI[[i]],fig.anova.HDI[[i]],fig.HPDI[[i]]),"+",theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))),ncol=2,
                      top=textGrob(expr(paste("Within-Subject Design, Report ",!!i,": ",italic(ρ), "=",!!params$rho[i],", power=",!!params$power[i],italic(", n="),!!params$n[i],sep="")),
                                   gp=gpar(fontsize=27)))
ggsave("~/Desktop/Figure 3.png", fig3a, width=4800, height=4900, units="px", device="png", dpi=300)





#CACHE#
########################################
i <- 1
fig.anova.HDI[[i]] <- fig.anova.HDI[[i]] + labs(x=expression("JZS-HDI"~italic("Sep%")),y=expression(~italic("BF")[10]))
fig3a <- grid.arrange(grobs=lapply(list(fig.LM.CI[[i]],fig.LH.HDI[[i]],fig.anova.HDI[[i]],fig.HPDI[[i]]),"+",theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))),ncol=2,
                      top=textGrob(expr(paste("Within-Subject Design, Report ",!!i,": ",italic(ρ), "=",!!params$rho[i],", power=",!!params$power[i],italic(", n="),!!params$n[i],sep="")),
                                   gp=gpar(fontsize=27)))
ggsave("~/Desktop/Figure 3.png", fig3a, width=4800, height=3260, units="px", device="png", dpi=300)

