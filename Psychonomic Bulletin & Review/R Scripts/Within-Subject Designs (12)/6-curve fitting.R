setwd("~/Downloads/Psychonomic Bulletin & Review") #YOUR PATH
filepath <- "R Scripts/Within-Subject Designs (12)/1-params.csv"
params <- read.csv(filepath)

fig <- vector(mode = "list", length = 12)
fun <- y~poly(x,2,raw=T) #Polynomial interpolation
library(polynom)
library(ggplot2)
library(gridExtra)
for (i in c(1:12)) {
  filepath <- paste("Datasets/Within-Subject Designs (12)/report wi_",i,".csv",sep="")
  replication <- read.csv(filepath)
  replication <- subset(replication, BFerror<.25)
  df <- data.frame("x"=abs(replication$SEP.anova.HDI)/100,"y"=log(replication$BF))
  fit <- lm(fun,df)
  exp <- as.character(signif(as.polynomial(coef(fit)),3))
  tex <- paste(gsub("x","~italic(x)",exp,fixed=T),
               paste("italic(R)^2",format(summary(fit)$adj.r.squared,digits=4),
                     sep="~`=`~"),sep="~~~~")
  fig[[i]] <- ggplot(df,aes(x,y))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_smooth(method="lm",se=F,formula=fun,color="red",linewidth=0.5)+
    lims(x=c(0,3.5),y=c(-3,21))+
    labs(x="",y="")+
    ggtitle(expr(paste("Report ",!!i,": ",italic(ρ), "=",!!params$rho[i],", power=",!!params$power[i],italic(", n="),!!params$n[i],sep="")))+
    annotate(geom="text",x=3.4,y=-2,label=tex, 
             family="serif",hjust=1,parse=T,size=9)+
    theme_classic()+
    theme(text=element_text(size=25))
}
library(grid)
fig4 <- grid.arrange(fig[[1]],fig[[2]],fig[[3]],fig[[4]],
                     fig[[5]],fig[[6]],fig[[7]],fig[[8]],
                     fig[[9]],fig[[10]],fig[[11]],fig[[12]],
                     nrow=3,
                     bottom=grid.text(expression("Within-Subject JZS-HDI"~italic("Sep")), 
                                      gp = gpar(fontsize=27)),
                     left=grid.text(expression("ln("~italic("BF")[10]~")"), rot=90, gp=gpar(fontsize=27)))
ggsave("~/Desktop/Figure 4.png", fig4, width=10240, height=5756, units="px", device="png", dpi=300)





#CACHE#
########################################
fig <- vector(mode = "list", length = 12)
fun <- y~poly(x,2,raw=T) #Polynomial interpolation
for (i in c(1:12)) {
  filepath <- paste("Datasets/Within-Subject Designs (12)/report wi_",i,".csv",sep="")
  replication <- read.csv(filepath)
  replication <- subset(replication, BFerror<.25)
  df <- data.frame("x"=abs(replication$SEP.anova.HDI)/100,"y"=log(replication$BF))
  fit <- lm(fun,df)
  exp <- as.character(signif(as.polynomial(coef(fit)),3))
  tex <- paste(gsub("x","~italic(x)",exp,fixed=T),
               paste("italic(R)^2",format(summary(fit)$adj.r.squared,digits=4),
                     sep="~`=`~"),sep="~~~~")
  fig[[i]] <- ggplot(df,aes(x,y))+
    geom_point(size=0.5,color="#8263d9",alpha=.2)+
    geom_smooth(method="lm",se=F,formula=fun,color="red",linewidth=0.5)+
    lims(x=c(0,3.5),y=c(-3,21))+
    labs(x=expression("JZS-HDI"~italic("Sep")),y=expression("ln("~italic("BF")[10]~")"))+
    ggtitle(expr(paste("Report ",!!i,": ",italic(ρ), "=",!!params$rho[i],", power=",!!params$power[i],italic(", n="),!!params$n[i],sep="")))+
    annotate(geom="text",x=0.5,y=-2,label=tex, 
             family="serif",hjust=0,parse=T,size=7)+
    theme_classic()+
    theme(text = element_text(size=20))
}

fig[[1]]
#PNG 900 600

#https://www.datanovia.com/en/blog/how-to-plot-a-smooth-line-using-ggplot2/
# method : smoothing method to be used. Possible values are lm, glm, gam, loess, rlm.
# method = “loess”: This is the default value for small number of observations. It computes a smooth local regression. You can read more about loess using the R code ?loess.
# method =“lm”: It fits a linear model. Note that, it’s also possible to indicate the formula as formula = y ~ poly(x, 3) to specify a degree 3 polynomial.
# se : logical value. If TRUE, confidence interval is displayed around smooth.
# fullrange : logical value. If TRUE, the fit spans the full range of the plot
# level : level of confidence interval to use. Default value is 0.95

