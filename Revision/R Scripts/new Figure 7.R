setwd("~/Downloads/Revision/Datasets") #YOUR PATH
replication <- read.csv("vary_dat.csv", stringsAsFactors=T)

library(ggplot2)
ann.text.size <- 9; theme.text.size <- 30; legend.size <- 13; geom.pt.size <- 0.5; geom.pt2.size <- 7

fig.a <- vector(mode="list",length=2)
for (id in 1:2) {
  if (id==1) {
    case=1; rho=0.9
  } else if (id==2) {
    case=4; rho=0.3
  }
  fig.a[[id]] <- ggplot(subset(replication, BF<exp(4)&BFerror<.01&Rho==rho),
                        aes(abs(SEP.anova.HDI),log(BF),color=factor(Sample.Size)))+
    geom_point(size=geom.pt.size,alpha=.1)+
    scale_color_manual(values=c("#CC79A7","#D55E00","#56B4E9","#F0E442","#0072B2"))+
    annotate("segment",x=0,xend=100,y=-log(3),yend=-log(3),colour="gray")+
    annotate("segment",x=0,xend=100,y=log(3),yend=log(3),colour="gray")+
    geom_vline(xintercept=100,linetype="dotted")+
    labs(x=expression("JZS-HDI"~italic("Sep%")),y=expression("ln("~italic("BF")[10]~")"))+
    scale_y_continuous(breaks=c(-2,-1,0,1,2,3,4),limits=c(-2.1,4.1))+
    scale_x_continuous(limits=c(0,150),expand=c(0,0))+
    annotate("text",x=105,y=log(3),size=ann.text.size,label="ln3")+
    annotate("text",x=105,y=-log(3),size=ann.text.size,label="-ln3")+
    ggtitle(expr(paste("Within-Subject Design, Case ",!!case,": ",italic(ρ), "=",!!rho,sep="")))+
    theme_classic()+
    theme(text=element_text(size=theme.text.size+2),legend.position=c(0.9,0.3),
          legend.direction="horizontal",legend.title=element_text(size=theme.text.size),
          plot.margin=margin(0.5,2,0.5,2,"cm"),legend.spacing.y=unit(0.5,"cm"))+
    guides(colour=guide_legend(override.aes=list(size=legend.size/2,alpha=1),ncol=1,byrow=T,title="Sample Size"))
}


viz.bench <- function(panel="A", err=.25) {
  if (panel=="A") {
    case=1
  } else if (panel=="B") {
    case=4
  }
  bf <- bf.sd <- sep <- sep.sd <- sep.null <- sep.null.sd <- numeric(15); i <- 1
  rho <- c(0.9, 0.3, 0.9, 0.3)
  small.es <- c(3.8, 16.5, 2.7, 12)
  large.es <- c(6.7, 28.6, 4.6, 20.4)
  
  for (es in c(0,small.es[case],large.es[case])) {
    for (n in c(5,10,24,36,48)) {
      dat <- subset(replication, Case==paste0("Case",case)&BFerror<err&
                      Effect.Size==es&Sample.Size==n)
      
      dat1 <- subset(dat, BF>2.9 & BF<3.1)
      sep[i] <- mean(abs(dat1$SEP.anova.HDI),na.rm=T)
      sep.sd[i] <- sd(abs(dat1$SEP.anova.HDI),na.rm=T)
      
      dat2 <- subset(dat, abs(SEP.anova.HDI)>99 & abs(SEP.anova.HDI)<101)
      bf[i] <- mean(dat2$BF)
      bf.sd[i] <- sd(dat2$BF)
      
      dat0 <- subset(dat, BF<1/2.9 & BF>1/3.1)
      sep.null[i] <- mean(abs(dat0$SEP.anova.HDI),na.rm=T)
      sep.null.sd[i] <- sd(abs(dat0$SEP.anova.HDI),na.rm=T)
      
      i = i + 1
    }
  }
  
  df <- data.frame("n"=c(3.5, 8.5, 22.5, 34.5, 46.5, 5, 10, 24, 36, 48, 6.5, 11.5, 25.5, 37.5, 49.5),
                   "BF"=bf,"BF.sd"=bf.sd,"SEP"=sep,"SEP.sd"=sep.sd,
                   "SEP0"=sep.null,"SEP0.sd"=sep.null.sd,
                   "ES"=factor(rep(c("Null","Small","Large"), each=5),
                               levels=c("Null","Small","Large"))) #manually jittered
  # scale.BF.sd <- range(df$BF.sd, na.rm=T)
  # scale.SEP.sd <- range(df$SEP.sd, na.rm=T)
  # scale.SEP0.sd <- range(df$SEP0.sd, na.rm=T)
  
  fig.c <- ggplot(df, aes(n,SEP))+
    geom_point(aes(shape=ES,color=ES),size=geom.pt2.size)+
    #scale_size_continuous(range=c(3,5)*scale.SEP.sd)+
    scale_color_manual(values=c("#999999","#E69F00","#56B4E9"))+
    scale_x_continuous(breaks=c(5,10,24,36,48),limits=c(0,50),expand=c(0,0))+
    scale_y_continuous(breaks=c(55,60,65,70,75,80),limits=c(55,80),expand=c(0,0))+
    coord_cartesian(clip="off")+
    labs(x="Sample Size",y=expression(italic("Sep%")~~"for"~italic("BF")["10"]~"≈ 3"),
         color="Effect Size",shape="Effect Size")+
    ggtitle(expr(paste(italic(ρ),"=",!!rho[case],sep="")))+
    guides(color=guide_legend(override.aes=list(size=legend.size)))+
    theme_classic()+theme(text=element_text(size=theme.text.size-3),
                          plot.margin=margin(0.25,4.5,0.25,4.5,"cm"),
                          legend.margin=margin(1,2,0.5,2,"cm"),
                          legend.spacing.x=unit(0.7,"cm"),
                          legend.title=element_text(size=theme.text.size))
  
  fig.b <- ggplot(df, aes(n,BF))+
    geom_point(aes(shape=ES,color=ES),size=geom.pt2.size)+
    #scale_size_continuous(range=c(3,5)*scale.BF.sd)+
    scale_color_manual(values=c("#999999","#E69F00","#56B4E9"))+
    scale_x_continuous(breaks=c(5,10,24,36,48),limits=c(0,50),expand=c(0,0))+
    scale_y_continuous(breaks=c(7,11,15,19),limits=c(7,19),expand=c(0,0))+
    coord_cartesian(clip="off")+
    labs(x="Sample Size",y=expression(italic("BF")[10]~~"for"~italic("Sep%")~"≈ 100"),
         color="Effect Size",shape="Effect Size")+
    ggtitle(expr(paste(italic(ρ),"=",!!rho[case],sep="")))+
    guides(color=guide_legend(override.aes=list(size=legend.size)))+
    theme_classic()+theme(text=element_text(size=theme.text.size-3),
                          plot.margin=margin(0.25,4.5,0.25,4.5,"cm"),
                          legend.margin=margin(1,2,0.5,2,"cm"),
                          legend.spacing.x=unit(0.7,"cm"),
                          legend.title=element_text(size=theme.text.size))
  
  fig.d <- ggplot(df, aes(n,SEP0))+
    geom_point(aes(shape=ES,color=ES),size=geom.pt2.size)+
    #scale_size_continuous(range=c(3,5)*scale.SEP0.sd)+
    scale_color_manual(values=c("#999999","#E69F00","#56B4E9"))+
    scale_x_continuous(breaks=c(5,10,24,36,48),limits=c(0,50),expand=c(0,0))+
    scale_y_continuous(breaks=c(16,20,24,28,32),limits=c(16,32),expand=c(0,0))+
    coord_cartesian(clip="off")+
    labs(x="Sample Size",y=expression(italic("Sep%")~~"for"~italic("BF")["01"]~"≈ 3"),
         color="Effect Size",shape="Effect Size")+
    ggtitle(expr(paste(italic(ρ),"=",!!rho[case],sep="")))+
    guides(color=guide_legend(override.aes=list(size=legend.size)))+
    theme_classic()+theme(text=element_text(size=theme.text.size-3),
                          plot.margin=margin(0.25,4.5,0.25,4.5,"cm"),
                          legend.margin=margin(1,2,0.5,2,"cm"),
                          legend.spacing.x=unit(0.7,"cm"),
                          legend.title=element_text(size=theme.text.size))
  list(fig.b,fig.c,fig.d)
}

fig.case1 <- viz.bench()
fig.case2 <- viz.bench("B")

library(ggpubr)
ggarrange(ggarrange(fig.a[[1]],fig.a[[2]],labels=c("A","B"),font.label=list(size=theme.text.size+2)),
          ggarrange(fig.case1[[1]],fig.case2[[1]],fig.case1[[2]],
                    fig.case2[[2]],fig.case1[[3]],fig.case2[[3]],
                    nrow=3,ncol=2,labels=c("C","D","E","F","G","H"),align="h",
                    common.legend=T,legend="bottom",font.label=list(size=theme.text.size+2)),
          nrow=2,heights=c(1,2))
ggsave("~/Desktop/Figure 7.png", width=8000, height=9000, units="px", device="png", dpi=300)

