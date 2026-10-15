setwd("~/Downloads/Revision/Datasets") #YOUR PATH
replication <- read.csv("vary_dat.btw.csv", stringsAsFactors=T)

library(ggplot2)
ann.text.size <- 9; theme.text.size <- 30; legend.size <- 13; geom.pt.size <- 0.5; geom.pt2.size <- 7

fig.a <- ggplot(subset(replication, BF<exp(4)&BFerror<.01),
                aes(abs(SEP.HPDI),log(BF),color=factor(Sample.Size)))+
  geom_point(size=geom.pt.size,alpha=.1)+
  scale_color_manual(values=c("#CC79A7","#D55E00","#56B4E9","#F0E442","#0072B2"))+
  annotate("segment",x=0,xend=100,y=-log(3),yend=-log(3),colour="gray")+
  annotate("segment",x=0,xend=100,y=log(3),yend=log(3),colour="gray")+
  geom_vline(xintercept=100,linetype="dotted")+
  labs(x=expression("Standard HDI"~italic("Sep%")),y=expression("ln("~italic("BF")[10]~")"))+
  scale_y_continuous(breaks=c(-2,-1,0,1,2,3,4),limits=c(-2.1,4.1))+
  scale_x_continuous(limits=c(0,150),expand=c(0,0))+
  annotate("text",x=105,y=log(3),size=ann.text.size,label="ln3")+
  annotate("text",x=105,y=-log(3),size=ann.text.size,label="-ln3")+
  ggtitle("Between-Subjects Design")+
  theme_classic()+
  theme(text=element_text(size=theme.text.size+2),legend.position=c(0.9,0.3),
        legend.direction="horizontal",legend.title=element_text(size=theme.text.size),
        plot.margin=margin(0.5,12,0.5,12,"cm"),legend.spacing.y=unit(0.5,"cm"))+
  guides(colour=guide_legend(override.aes=list(size=legend.size/2,alpha=1),ncol=1,byrow=T,title="Sample Size"))


bf <- bf.sd <- sep <- sep.sd <- sep.null <- sep.null.sd <- numeric(15); i <- 1

for (es in c("Null","Small","Large")) {
  for (n in c(5,10,24,36,48)) {
    dat <- subset(replication, BFerror<0.01&Sample.Size==n&Effect.Size==es)
    
    dat1 <- subset(dat, BF>2.9 & BF<3.1)
    sep[i] <- mean(abs(dat1$SEP.HPDI),na.rm=T)
    sep.sd[i] <- sd(abs(dat1$SEP.HPDI),na.rm=T)
    
    dat2 <- subset(dat, abs(SEP.HPDI)>99 & abs(SEP.HPDI)<101)
    bf[i] <- mean(dat2$BF)
    bf.sd[i] <- sd(dat2$BF)
    
    dat0 <- subset(dat, BF<1/2.9 & BF>1/3.1)
    sep.null[i] <- mean(abs(dat0$SEP.HPDI),na.rm=T)
    sep.null.sd[i] <- sd(abs(dat0$SEP.HPDI),na.rm=T)
    
    i = i + 1
  }
}

df <- data.frame("n"=c(3.5, 8.5, 22.5, 34.5, 46.5, 5, 10, 24, 36, 48, 6.5, 11.5, 25.5, 37.5, 49.5),
                 "BF"=bf,"BF.sd"=bf.sd,"SEP"=sep,"SEP.sd"=sep.sd,
                 "SEP0"=sep.null,"SEP0.sd"=sep.null.sd,
                 "ES"=factor(rep(c("Null","Small","Large"), each=5),
                             levels=c("Null","Small","Large"))) #manually jittered


fig.c <- ggplot(df, aes(n,SEP))+
  geom_point(aes(shape=ES,color=ES),size=geom.pt2.size)+
  scale_color_manual(values=c("#999999","#E69F00","#56B4E9"))+
  scale_x_continuous(breaks=c(5,10,24,36,48),limits=c(0,50),expand=c(0,0))+
  scale_y_continuous(breaks=c(60,65,70,75,80),limits=c(60,81),expand=c(0,0))+
  coord_cartesian(clip="off")+
  labs(x="Sample Size",y=expression(italic("Sep%")~~"for"~italic("BF")["10"]~"≈ 3"),
       color="Effect Size",shape="Effect Size")+
  guides(color=guide_legend(override.aes=list(size=legend.size)))+
  theme_classic()+theme(text=element_text(size=theme.text.size-3),
                        plot.margin=margin(1,0.5,0.5,0.5,"cm"),
                        legend.margin=margin(0,0.85,0.5,0.85,"cm"),
                        legend.spacing.x=unit(0.7,"cm"),
                        legend.title=element_text(size=theme.text.size))

fig.b <- ggplot(df, aes(n,BF))+
  geom_point(aes(shape=ES,color=ES),size=geom.pt2.size)+
  scale_color_manual(values=c("#999999","#E69F00","#56B4E9"))+
  scale_x_continuous(breaks=c(5,10,24,36,48),limits=c(0,50),expand=c(0,0))+
  scale_y_continuous(breaks=c(10,13,16,19),limits=c(10,19),expand=c(0,0))+
  coord_cartesian(clip="off")+
  labs(x="Sample Size",y=expression(italic("BF")[10]~~"for"~italic("Sep%")~"≈ 100"),
       color="Effect Size",shape="Effect Size")+
  guides(color=guide_legend(override.aes=list(size=legend.size)))+
  theme_classic()+theme(text=element_text(size=theme.text.size-3),
                        plot.margin=margin(1,0.5,0.5,0.5,"cm"),
                        legend.margin=margin(0,0.85,0.5,0.85,"cm"),
                        legend.spacing.x=unit(0.7,"cm"),
                        legend.title=element_text(size=theme.text.size))

fig.d <- ggplot(df, aes(n,SEP0))+
  geom_point(aes(shape=ES,color=ES),size=geom.pt2.size)+
  scale_color_manual(values=c("#999999","#E69F00","#56B4E9"))+
  scale_x_continuous(breaks=c(5,10,24,36,48),limits=c(0,50),expand=c(0,0))+
  scale_y_continuous(breaks=c(17,21,25,29,33),limits=c(17,33),expand=c(0,0))+
  coord_cartesian(clip="off")+
  labs(x="Sample Size",y=expression(italic("Sep%")~~"for"~italic("BF")["01"]~"≈ 3"),
       color="Effect Size",shape="Effect Size")+
  guides(color=guide_legend(override.aes=list(size=legend.size)))+
  theme_classic()+theme(text=element_text(size=theme.text.size-3),
                        plot.margin=margin(1,0.5,0.5,0.5,"cm"),
                        legend.margin=margin(0,0.85,0.5,0.85,"cm"),
                        legend.spacing.x=unit(0.7,"cm"),
                        legend.title=element_text(size=theme.text.size))

library(ggpubr)
ggarrange(fig.a,
          ggarrange(fig.b,fig.c,fig.d,ncol=3,labels=c("b","c","d"),align="h",
                    common.legend=T,legend="bottom",font.label=list(size=23)),
          nrow=2,labels="a",font.label=list(size=23),heights=c(1,0.8))
ggsave("~/Desktop/Figure 7 supplementary.png", width=8000, height=5500, units="px", device="png", dpi=300)

