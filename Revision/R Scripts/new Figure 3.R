setwd("~/Downloads/Revision/Datasets") #YOUR PATH
replication <- read.csv("btw_dat.csv",stringsAsFactors=T)
# colnames(replication)[c(1,9)] <- c("BF_JZS","BF") #Can also check the Pearson Bayes factor (shape=0)

{
  ## Find descriptive statistics such as 
  ##  (1) count, (2) min, (3) max, (4) arithmetic mean, (5) median, (6) sample standard deviation
  ##  of two types of intervals' separation percentages (CI, HPDI),
  ##  when their corresponding Bayes factor value is in the range of "lwr" and "upr".
  lwr <- 2.9
  upr <- 3.1
  tab.SEP.CI <- tab.SEP.HPDI <- matrix(NaN,nrow=2,ncol=6)
  for (i in 1:2) {
    slicing <- subset(replication,BF>lwr&BF<upr&Case==paste0("Case",i))
    dat <- abs(slicing$SEP.CI)
    tab.SEP.CI[i,] <- c(length(dat),
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
  }
  tab.SEP.CI <- as.data.frame(tab.SEP.CI)
  colnames(tab.SEP.CI) <- c("count","min","max","mean","median","sd")
  
  tab.SEP.HPDI <- as.data.frame(tab.SEP.HPDI)
  colnames(tab.SEP.HPDI) <- c("count","min","max","mean","median","sd")
  
  
  ## Find descriptive statistics such as 
  ##  (1) count, (2) min, (3) max, (4) arithmetic mean, (5) median, (6) sample standard deviation
  ##  of two types of intervals' Bayes factor values (CI, HPDI),
  ##  when their separation percentage is in the range of "lwr" and "upr".
  lwr <- 99
  upr <- 101
  tab.BF.CI <- tab.BF.HPDI <- matrix(NaN,nrow=2,ncol=6)
  for (i in 1:2) {
    dat <- subset(replication,abs(SEP.CI)>lwr&abs(SEP.CI)<upr&Case==paste0("Case",i))$BF
    tab.BF.CI[i,] <- c(length(dat),
                       min(dat),
                       max(dat),
                       mean(dat),
                       median(dat),
                       sd(dat))
    
    dat <- subset(replication,abs(SEP.HPDI)>lwr&abs(SEP.HPDI)<upr&Case==paste0("Case",i))$BF
    tab.BF.HPDI[i,] <- c(length(dat),
                         min(dat),
                         max(dat),
                         mean(dat),
                         median(dat),
                         sd(dat))
  }
  tab.BF.CI <- as.data.frame(tab.BF.CI)
  colnames(tab.BF.CI) <- c("count","min","max","mean","median","sd")
  
  tab.BF.HPDI <- as.data.frame(tab.BF.HPDI)
  colnames(tab.BF.HPDI) <- c("count","min","max","mean","median","sd")
}

## Visualization
library(tidyverse)
library(gridExtra)
ann.text.size <- 5; theme.text.size <- 23; ann.tbl.size <- 17; geom.pt.size <- 0.5

i <- 2
case <- subset(replication,Case==paste0("Case",i)) #Case i
reg.CI <- summary(lm(log(BF)~abs(SEP.CI)+I(SEP.CI^2),data=case))$coefficients
CItable <- data.frame("min"=c(tab.BF.CI$min[i],tab.SEP.CI$min[i]),
                      "max"=c(tab.BF.CI$max[i],tab.SEP.CI$max[i]),
                      "mean"=c(tab.BF.CI$mean[i],tab.SEP.CI$mean[i]),
                      "sd"=c(tab.BF.CI$sd[i],tab.SEP.CI$sd[i]))
CItable <- round(CItable,1)
rownames(CItable) <- c("BF","Sep%")
fig.CI <- ggplot(subset(case,BF<=50),aes(abs(SEP.CI),BF))+
  geom_point(size=geom.pt.size,color="#8263d9",alpha=.2)+
  geom_hline(yintercept=3,linetype="dashed")+
  annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
  scale_y_continuous(breaks=c(0,3,10,20,30,40,50))+
  lims(x=c(0,150))+
  annotation_custom(tableGrob(format(CItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=ann.tbl.size)),xmin=15,xmax=90,ymin=35,ymax=50)+
  geom_function(fun=function(x) exp(reg.CI[1,1]+reg.CI[2,1]*x+reg.CI[3,1]*x*x),xlim=c(0,134-3*i))+
  geom_function(fun=function(x) 2*exp((qnorm(1-.05/2)*(x/100))^2)/sqrt(24*i*pi),xlim=c(0,114+4*i),colour="red",linetype="dotdash")+
  labs(x=expression("CI"~italic("Sep%")),y=expression(~italic("BF")[10]))+
  theme_classic()+
  theme(text = element_text(size=theme.text.size))
reg.HPDI <- summary(lm(log(BF)~abs(SEP.HPDI)+I(SEP.HPDI^2),data=case))$coefficients
HPDItable <- data.frame("min"=c(tab.BF.HPDI$min[i],tab.SEP.HPDI$min[i]),
                        "max"=c(tab.BF.HPDI$max[i],tab.SEP.HPDI$max[i]),
                        "mean"=c(tab.BF.HPDI$mean[i],tab.SEP.HPDI$mean[i]),
                        "sd"=c(tab.BF.HPDI$sd[i],tab.SEP.HPDI$sd[i]))
HPDItable <- round(HPDItable,1)
rownames(HPDItable) <- c("BF","Sep%")
fig.HPDI <- ggplot(subset(case,BF<=50),aes(abs(SEP.HPDI),BF))+
  geom_point(size=geom.pt.size,color="#8263d9",alpha=.2)+
  geom_hline(yintercept=3,linetype="dashed")+
  annotate("segment",x=100,xend=100,y=0,yend=20,colour="red",linewidth=1.2,alpha=.3)+
  scale_y_continuous(breaks=c(0,3,10,20,30,40,50))+
  lims(x=c(0,150))+
  annotation_custom(tableGrob(format(HPDItable,nsmall=1,core.just="right"),theme=ttheme_minimal(base_size=ann.tbl.size)),xmin=15,xmax=90,ymin=35,ymax=50)+
  geom_function(fun=function(x) exp(reg.HPDI[1,1]+reg.HPDI[2,1]*x+reg.HPDI[3,1]*x*x),xlim=c(0,116.7+1.3*i))+
  geom_function(fun=function(x) 2*exp((qnorm(1-.05/2)*(x/100))^2)/sqrt(24*i*pi),xlim=c(0,114+4*i),colour="red",linetype="dotdash")+
  annotate("segment",x=118,xend=133,y=12,yend=12)+
  annotate("text",x=135.5,y=12,label="Fitted",hjust=0,size=ann.text.size)+
  annotate("segment",x=118,xend=133,y=8,yend=8,colour="red",linetype="dotdash")+
  annotate("text",x=135.5,y=8,label="Analytic",hjust=0,size=ann.text.size)+
  labs(x=expression("HDI"~italic("Sep%")),y=expression(~italic("BF")[10]))+
  theme_classic()+
  theme(text = element_text(size=theme.text.size))

library(grid)
fig3 <- grid.arrange(grobs=lapply(list(fig.CI,fig.HPDI),"+",theme(plot.margin=unit(c(0.35,0.35,0,0.35),"cm"))),nrow=1,
                     top=textGrob(expr(paste("Between-Subjects Design, Case ",!!i,italic(": n="),!!(i*24),sep="")),gp=gpar(fontsize=theme.text.size)))
ggsave("~/Desktop/Figure 3.png", fig3, width=3740, height=1640, units="px", device="png", dpi=300)


reg.CI <- summary(lm(log(BF)~abs(SEP.CI/100)+I((SEP.CI/100)^2),data=case))$coefficients
polyroot(c(reg.CI[1,1]-log(2/sqrt(24*i*pi)),reg.CI[2,1],reg.CI[3,1]-qnorm(1-.05/2)^2))





#CACHE#
########################################
ymin <- 0
ymax <- 1/3
xmax.CI <- max(subset(replication,BF<=ymax)$SEP.CI)
xmin.CI <- min(abs(subset(replication,BF>=ymin)$SEP.CI))
ggplot(subset(replication,BF<=ymax&BF>=ymin),aes(abs(SEP.CI),BF))+
  geom_point(size=geom.pt.size,color="#8263d9",alpha=.6)+
  geom_function(fun=function(x) exp(reg.CI[1,1]+reg.CI[2,1]*x+reg.CI[3,1]*x*x),xlim=c(xmin.CI,xmax.CI))+
  geom_function(fun=function(x) 2*exp((qnorm(1-.05/2)*(x/100))^2)/sqrt(24*i*pi),xlim=c(xmin.CI,xmax.CI),colour="red",linetype="dotdash")+
  labs(x=expression("CI"~italic("Sep%")),y=expression(~italic("BF")[10]))+
  theme_classic()+
  theme(text = element_text(size=theme.text.size))

## two intersection points in the first quadrant
#http://www.chim.lu/formules/en0234.php

xmax.HPDI <- max(subset(replication,BF<=ymax)$SEP.HPDI)
xmin.HPDI <- min(abs(subset(replication,BF>=ymin)$SEP.HPDI))
ggplot(subset(replication,BF<=ymax&BF>=ymin),aes(abs(SEP.HPDI),BF))+
  geom_point(size=geom.pt.size,color="#8263d9",alpha=.6)+
  geom_function(fun=function(x) exp(reg.HPDI[1,1]+reg.HPDI[2,1]*x+reg.HPDI[3,1]*x*x),xlim=c(xmin.HPDI,xmax.HPDI))+
  geom_function(fun=function(x) 2*exp((qnorm(1-.05/2)*(x/100))^2)/sqrt(24*i*pi),xlim=c(xmin.HPDI,xmax.HPDI),colour="red",linetype="dotdash")+
  labs(x=expression("HDI"~italic("Sep%")),y=expression(~italic("BF")[10]))+
  theme_classic()+
  theme(text = element_text(size=theme.text.size))

