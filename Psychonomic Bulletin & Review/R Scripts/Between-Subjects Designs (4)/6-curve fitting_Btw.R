setwd("~/Downloads/Psychonomic Bulletin & Review") #YOUR PATH
filepath <- "R Scripts/Between-Subjects Designs (4)/1-params_Btw.csv"
params <- read.csv(filepath)

fun <- y~poly(x,2,raw=T) #Polynomial interpolation
library(polynom)
library(ggplot2)
library(gridExtra)
library(tidyverse)
fig <- pic <- vector(mode = "list", length = 4)
for (i in 1:4) {
  filepath <- paste("Datasets/Between-Subjects Designs (4)/report_",i,".csv",sep="")
  replication <- read.csv(filepath)
  df <- data.frame("x"=abs(replication$SEP.CI)/100,"y"=log(replication$BF))
  fit <- lm(fun,df)
  exp <- as.character(signif(as.polynomial(coef(fit)),3))
  tex <- paste(gsub("x","~italic(x)",exp,fixed=T),
               paste("italic(R)^2",format(summary(fit)$adj.r.squared,digits=4),
                     sep="~`=`~"),sep="~~~~")
  fig[[i]] <- ggplot(df,aes(x,y))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_smooth(method="lm",se=F,formula=fun,color="red",linewidth=0.5)+
    lims(x=c(0,3),y=c(-3,20))+
    labs(x=expression("Between-Subjects CI"~italic("Sep")),y=expression("ln("~italic("BF")[10]~")"))+
    ggtitle(expr(paste("Report ",!!i,": power=",!!params$power[i],italic(", n="),!!params$n[i],sep="")))+
    annotate(geom="text",x=0.5,y=-2,label=tex, 
             family="serif",hjust=0,parse=T,size=9)+
    theme_classic()+
    theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),text=element_text(size=25))
  
  df <- data.frame("x"=abs(replication$SEP.HPDI)/100,"y"=log(replication$BF))
  fit <- lm(fun,df)
  exp <- as.character(signif(as.polynomial(coef(fit)),3))
  tex <- paste(gsub("x","~italic(x)",exp,fixed=T),
               paste("italic(R)^2",format(summary(fit)$adj.r.squared,digits=4),
                     sep="~`=`~"),sep="~~~~")
  pic[[i]] <- ggplot(df,aes(x,y))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_smooth(method="lm",se=F,formula=fun,color="red",linewidth=0.5)+
    lims(x=c(0,3),y=c(-3,20))+
    labs(x=expression("Between-Subjects HDI"~italic("Sep")),y=expression("ln("~italic("BF")[10]~")"))+
    ggtitle(expr(paste("Report ",!!i,": power=",!!params$power[i],italic(", n="),!!params$n[i],sep="")))+
    annotate(geom="text",x=0.5,y=-2,label=tex, 
             family="serif",hjust=0,parse=T,size=9)+
    theme_classic()+
    theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),text=element_text(size=25))
}
p <- grid.arrange(fig[[1]],fig[[2]],fig[[3]],fig[[4]],
                  pic[[1]],pic[[2]],pic[[3]],pic[[4]],
                  nrow=2)
ggsave("~/Desktop/[btw] curve fitting.png", p, width=10240, height=5752, units="px", device="png", dpi=300)





#CACHE#
########################################
fig <- pic <- vector(mode = "list", length = 4)
for (i in 1:4) {
  filepath <- paste("Datasets/Between-Subjects Designs (4)/report_",i,".csv",sep="")
  replication <- read.csv(filepath)
  df <- data.frame("x"=abs(replication$SEP.CI)/100,"y"=log(replication$BF))
  fit <- lm(fun,df)
  exp <- as.character(signif(as.polynomial(coef(fit)),3))
  tex <- paste(gsub("x","~italic(x)",exp,fixed=T),
               paste("italic(R)^2",format(summary(fit)$adj.r.squared,digits=4),
                     sep="~`=`~"),sep="~~~~")
  fig[[i]] <- ggplot(df,aes(x,y))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_smooth(method="lm",se=F,formula=fun,color="red",linewidth=0.5)+
    lims(x=c(0,3),y=c(-3,20))+
    labs(x=expression("Between-Subjects CI"~italic("Sep")),y=expression("ln("~italic("BF")[10]~")"))+
    annotate(geom="text",x=0.5,y=-2,label=tex, 
             family="serif",hjust=0,parse=T,size=5)+
    theme_classic()+
    theme(text=element_text(size=18))
  
  df <- data.frame("x"=abs(replication$SEP.HPDI)/100,"y"=log(replication$BF))
  fit <- lm(fun,df)
  exp <- as.character(signif(as.polynomial(coef(fit)),3))
  tex <- paste(gsub("x","~italic(x)",exp,fixed=T),
               paste("italic(R)^2",format(summary(fit)$adj.r.squared,digits=4),
                     sep="~`=`~"),sep="~~~~")
  pic[[i]] <- ggplot(df,aes(x,y))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_smooth(method="lm",se=F,formula=fun,color="red",linewidth=0.5)+
    lims(x=c(0,3),y=c(-3,20))+
    labs(x=expression("Between-Subjects HDI"~italic("Sep")),y=expression("ln("~italic("BF")[10]~")"))+
    annotate(geom="text",x=0.5,y=-2,label=tex, 
             family="serif",hjust=0,parse=T,size=5)+
    theme_classic()+
    theme(text=element_text(size=18))
}

library(grid)
i <- 1
grid.arrange(grobs=lapply(list(fig[[i]],pic[[i]]),"+",theme(plot.margin=unit(c(0.35,1,0,1),"cm"))),nrow=1,
             top=textGrob(expr(paste("Report ",!!i,": power=",!!params$power[i],italic(", n="),!!params$n[i],sep="")),gp=gpar(fontsize=20,font=8)))
#PNG one by two, 1035 x 410 px

