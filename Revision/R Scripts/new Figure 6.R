setwd("~/Downloads/Revision/Datasets") #YOUR PATH
dat <- read.csv("wi_dat.csv", stringsAsFactors=T)

library(polynom)
library(ggplot2)
library(gridExtra)
theme.text.size <- 25; ann.eq.size <- 9; geom.pt.size <- 0.5

rho <- rep(c(.9,.3),2); N <- rep(c(24,48),each=2)
err <- .01
fig <- vector(mode = "list", length = 4)
fun <- y~poly(x,2,raw=T) #Polynomial interpolation
for (i in c(1:4)) {
  replication <- subset(dat,BFerror<err&Case==paste0("Case",i))
  df <- data.frame("x"=abs(replication$SEP.anova.HDI)/100,"y"=log(replication$BF))
  fit <- lm(fun,df)
  exp <- as.character(signif(as.polynomial(coef(fit)),3))
  tex <- paste(gsub("x","~italic(x)",exp,fixed=T),
               paste("italic(R)^2",format(summary(fit)$adj.r.squared,digits=4),
                     sep="~`=`~"),sep="~~~~")
  fig[[i]] <- ggplot(df,aes(x,y))+
    geom_point(size=geom.pt.size,color="#8263d9",alpha=.2)+
    geom_smooth(method="lm",se=F,formula=fun,color="red",linewidth=0.5)+
    lims(x=c(0,3.5),y=c(-3,22))+
    labs(x="",y="")+
    ggtitle(expr(paste("Case ",!!i,": ",italic(ρ), "=",!!rho[i],italic(", n="),!!N[i],sep="")))+
    annotate(geom="text",x=3.4,y=-2,label=tex, 
             family="serif",hjust=1,parse=T,size=ann.eq.size)+
    theme_classic()+
    theme(text=element_text(size=theme.text.size))
}
library(grid)
fig5 <- grid.arrange(fig[[1]],fig[[2]],fig[[3]],fig[[4]], nrow=2,
                     bottom=grid.text(expression("Within-Subject JZS-HDI"~italic("Sep")), 
                                      gp = gpar(fontsize=theme.text.size+2)),
                     left=grid.text(expression("ln("~italic("BF")[10]~")"), rot=90, gp=gpar(fontsize=27)))
ggsave("~/Desktop/Figure 6.png", fig5, width=5120, height=3000, units="px", device="png", dpi=300)





#CACHE#
########################################
fun <- y~poly(x,2,raw=T)
for (i in c(1:4)) {
  replication <- subset(dat,BFerror<err&Case==paste0("Case",i))
  df <- data.frame("x"=abs(replication$SEP.anova.HDI)/100,"y"=log(replication$BF))
  fit <- lm(fun,df)
  coef.const <- round(coef(fit)[1],2)
  coef.1st <- round(coef(fit)[2],2)
  coef.2nd <- round(coef(fit)[3],2)
  adj.R2 <- round(summary(fit)$adj.r.squared,4)
  tex <- sprintf("'%0.2f' + '%0.2f' ~italic(x) + '%0.2f' ~italic(x)^2 ~~~~ ~italic(R)^2 == '%0.4f'",
                 coef.const,coef.1st,coef.2nd,adj.R2)
  fig[[i]] <- ggplot(df,aes(x,y))+
    geom_point(size=geom.pt.size,color="#8263d9",alpha=.2)+
    geom_smooth(method="lm",se=F,formula=fun,color="red",linewidth=0.5)+
    lims(x=c(0,3.5),y=c(-3,22))+
    labs(x="",y="")+
    ggtitle(expr(paste("Case ",!!i,": ",italic(ρ), "=",!!rho[i],italic(", n="),!!N[i],sep="")))+
    annotate(geom="text",x=3.4,y=-2,label=tex, 
             family="serif",hjust=1,parse=T,size=ann.eq.size)+
    theme_classic()+
    theme(text=element_text(size=theme.text.size))
}
fig5 <- grid.arrange(fig[[1]],fig[[2]],fig[[3]],fig[[4]], nrow=2,
                     bottom=grid.text(expression("Within-Subject JZS-HDI"~italic("Sep")), 
                                      gp = gpar(fontsize=theme.text.size+2)),
                     left=grid.text(expression("ln("~italic("BF")[10]~")"), rot=90, gp=gpar(fontsize=27)))
ggsave("~/Desktop/Figure 6.png", fig5, width=5120, height=3000, units="px", device="png", dpi=300)

