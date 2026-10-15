setwd("~/Downloads/Psychonomic Bulletin & Review") #YOUR PATH
source("R Scripts/Between-Subjects Designs (4)/4-BF benchmarks_Btw.R",echo=F)
source("R Scripts/Between-Subjects Designs (4)/3-SEP benchmarks_Btw.R",echo=F)

filepath <- "R Scripts/Between-Subjects Designs (4)/1-params_Btw.csv"
params <- read.csv(filepath)

library(tidyverse)
library(gridExtra)

i <- 4
filepath <- paste("Datasets/Between-Subjects Designs (4)/report_",i,".csv",sep="")
replication <- read.csv(filepath)
CItable <- data.frame("min"=c(tab.BF.CI$min[i],tab.SEP.CI$min[i]),
                      "max"=c(tab.BF.CI$max[i],tab.SEP.CI$max[i]),
                      "mean"=c(tab.BF.CI$mean[i],tab.SEP.CI$mean[i]),
                      "sd"=c(tab.BF.CI$sd[i],tab.SEP.CI$sd[i]))
CItable <- round(CItable,1)
rownames(CItable) <- c("BF","Sep%")
fig.CI <- ggplot(subset(replication,BF<=50),aes(abs(SEP.CI),BF))+
  geom_point(size=0.5,color="#8263d9",alpha=.2)+
  geom_hline(yintercept=3,linetype="dashed")+
  annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
  scale_y_continuous(breaks=c(0,3,10,20,30,40,50))+
  lims(x=c(0,150))+
  annotation_custom(tableGrob(format(CItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=20)),xmin=15,xmax=90,ymin=35,ymax=50)+
  labs(x=expression("CI"~italic("Sep%")),y=expression(~italic("BF")[10]))+
  theme_classic()+
  theme(text = element_text(size=23))
HPDItable <- data.frame("min"=c(tab.BF.HPDI$min[i],tab.SEP.HPDI$min[i]),
                        "max"=c(tab.BF.HPDI$max[i],tab.SEP.HPDI$max[i]),
                        "mean"=c(tab.BF.HPDI$mean[i],tab.SEP.HPDI$mean[i]),
                        "sd"=c(tab.BF.HPDI$sd[i],tab.SEP.HPDI$sd[i]))
HPDItable <- round(HPDItable,1)
rownames(HPDItable) <- c("BF","Sep%")
fig.HPDI <- ggplot(subset(replication,BF<=50),aes(abs(SEP.HPDI),BF))+
  geom_point(size=0.5,color="#8263d9",alpha=.2)+
  geom_hline(yintercept=3,linetype="dashed")+
  annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
  scale_y_continuous(breaks=c(0,3,10,20,30,40,50))+
  lims(x=c(0,150))+
  annotation_custom(tableGrob(format(HPDItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=20)),xmin=15,xmax=90,ymin=35,ymax=50)+
  labs(x=expression("HDI"~italic("Sep%")),y=expression(~italic("BF")[10]))+
  theme_classic()+
  theme(text = element_text(size=23))

library(grid)
fig2 <- grid.arrange(grobs=lapply(list(fig.CI,fig.HPDI),"+",theme(plot.margin=unit(c(0.35,0.35,0,0.35),"cm"))),nrow=1,
                     top=textGrob(expr(paste("Between-Subjects Design, Report ",!!i,": power=",!!params$power[i],italic(", n="),!!params$n[i],sep="")),gp=gpar(fontsize=27)))
ggsave("~/Desktop/Figure 2.png", fig2, width=3740, height=1640, units="px", device="png", dpi=300)





#CACHE#
########################################
fig.CI <- fig.HPDI <- vector(mode = "list", length = 4)
for (i in 1:4) {
  filepath <- paste("Datasets/Between-Subjects Designs (4)/report_",i,".csv",sep="")
  replication <- read.csv(filepath)
  CItable <- data.frame("min"=c(tab.BF.CI$min[i],tab.SEP.CI$min[i]),
                        "max"=c(tab.BF.CI$max[i],tab.SEP.CI$max[i]),
                        "mean"=c(tab.BF.CI$mean[i],tab.SEP.CI$mean[i]),
                        "sd"=c(tab.BF.CI$sd[i],tab.SEP.CI$sd[i]))
  CItable <- round(CItable,1)
  rownames(CItable) <- c("BF","Sep%")
  fig.CI[[i]] <- ggplot(subset(replication,BF<=50),aes(abs(SEP.CI),BF))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=3,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,50))+
    lims(x=c(0,150))+
    annotation_custom(tableGrob(format(CItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=20)),xmin=15,xmax=90,ymin=35,ymax=50)+
    labs(x=expression("Between-Subjects CI"~italic("Sep%")),y=expression(~italic("BF")))+
    ggtitle(expr(paste("Report ",!!i,": power=",!!params$power[i],italic(", n="),!!params$n[i],sep="")))+
    theme_classic()+
    theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),text=element_text(size=18))
  HPDItable <- data.frame("min"=c(tab.BF.HPDI$min[i],tab.SEP.HPDI$min[i]),
                          "max"=c(tab.BF.HPDI$max[i],tab.SEP.HPDI$max[i]),
                          "mean"=c(tab.BF.HPDI$mean[i],tab.SEP.HPDI$mean[i]),
                          "sd"=c(tab.BF.HPDI$sd[i],tab.SEP.HPDI$sd[i]))
  HPDItable <- round(HPDItable,1)
  rownames(HPDItable) <- c("BF","Sep%")
  fig.HPDI[[i]] <- ggplot(subset(replication,BF<=50),aes(abs(SEP.HPDI),BF))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=3,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,50))+
    lims(x=c(0,150))+
    annotation_custom(tableGrob(format(HPDItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=20)),xmin=15,xmax=90,ymin=35,ymax=50)+
    labs(x=expression("Between-Subjects HDI"~italic("Sep%")),y=expression(~italic("BF")))+
    ggtitle(expr(paste("Report ",!!i,": power=",!!params$power[i],italic(", n="),!!params$n[i],sep="")))+
    theme_classic()+
    theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),text=element_text(size=18))
}
p <- grid.arrange(fig.CI[[1]],fig.CI[[2]],fig.CI[[3]],fig.CI[[4]],
                  fig.HPDI[[1]],fig.HPDI[[2]],fig.HPDI[[3]],fig.HPDI[[4]],
                  nrow=2)
#PNG two by four, 2560 x 1438 px
#PNG individual, 900 x 600 px


########################################
fig.CI <- fig.HPDI <- vector(mode = "list", length = 4)
for (i in 1:4) {
  filepath <- paste("Datasets/Between-Subjects Designs (4)/report_",i,".csv",sep="")
  replication <- read.csv(filepath)
  CItable <- data.frame("min"=c(tab.BF.CI$min[i],tab.SEP.CI$min[i]),
                        "max"=c(tab.BF.CI$max[i],tab.SEP.CI$max[i]),
                        "mean"=c(tab.BF.CI$mean[i],tab.SEP.CI$mean[i]),
                        "sd"=c(tab.BF.CI$sd[i],tab.SEP.CI$sd[i]))
  CItable <- round(CItable,1)
  rownames(CItable) <- c("BF","Sep%")
  fig.CI[[i]] <- ggplot(subset(replication,BF<=50),aes(abs(SEP.CI),BF))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,50))+
    lims(x=c(0,150))+
    annotation_custom(tableGrob(format(CItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=20)),xmin=15,xmax=90,ymin=35,ymax=50)+
    labs(x=expression("Between-Subjects CI"~italic("Sep%")),y=expression(~italic("BF")))+
    theme_classic()+
    theme(text = element_text(size=18))
  HPDItable <- data.frame("min"=c(tab.BF.HPDI$min[i],tab.SEP.HPDI$min[i]),
                          "max"=c(tab.BF.HPDI$max[i],tab.SEP.HPDI$max[i]),
                          "mean"=c(tab.BF.HPDI$mean[i],tab.SEP.HPDI$mean[i]),
                          "sd"=c(tab.BF.HPDI$sd[i],tab.SEP.HPDI$sd[i]))
  HPDItable <- round(HPDItable,1)
  rownames(HPDItable) <- c("BF","Sep%")
  fig.HPDI[[i]] <- ggplot(subset(replication,BF<=50),aes(abs(SEP.HPDI),BF))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_hline(yintercept=3,linetype="dashed")+
    annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
    scale_y_continuous(breaks=c(0,3,10,20,30,40,50))+
    lims(x=c(0,150))+
    annotation_custom(tableGrob(format(HPDItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=20)),xmin=15,xmax=90,ymin=35,ymax=50)+
    labs(x=expression("Between-Subjects HDI"~italic("Sep%")),y=expression(~italic("BF")))+
    theme_classic()+
    theme(text = element_text(size=18))
}

i <- 1
grid.arrange(grobs=lapply(list(fig.CI[[i]],fig.HPDI[[i]]),"+",theme(plot.margin=unit(c(0.35,1,0,1),"cm"))),nrow=1,
             top=textGrob(expr(paste("Report ",!!i,": power=",!!params$power[i],italic(", n="),!!params$n[i],sep="")),gp=gpar(fontsize=20,font=8)))
#PNG one by two, 1035 x 410 px

