library(BayesFactor) #version 0.9.12-4.4
library(rmBayes) #version 0.1.15, macOS (Apple chip)

setwd("~/Desktop/wi.sim") #YOUR PATH

NSim <- 1e4
for (delta.mu in c(0, 3.8, 6.7)) {
  for (n in c(5, 10, 24, 36, 48)) {
    BFs <- SEPs <- BFerrors <- numeric(NSim)
    
    for (i in 1:NSim) {
      set.seed(i+NSim*n)
      TrueScore <- rnorm(n, 100, 20)
      eij1 <- rnorm(n, 0, 6.67)
      eij2 <- rnorm(n, 0, 6.67)
      # Long format data:
      dat <- data.frame("Subject" = factor(rep(c(1:n), 2)),
                        "Level" = factor(rep(c("Level1", "Level2"), each = n)),
                        "Response" = c(TrueScore + eij1 + delta.mu,
                                       TrueScore + eij2))
      
      BF <- anovaBF(Response ~ Level + Subject, data = dat, whichRandom = "Subject", progress = F)
      BFs[i] <- extractBF(BF)$bf
      BFerrors[i] <- extractBF(BF)$error
      
      HDI <- rmHDI(dat, chains = 2, seed = i+NSim*n, treat = "fixed", permuted = F)
      SEPs[i] <- 50 * abs(HDI$`posterior means`[1] - HDI$`posterior means`[2]) / HDI$width
    }
    
    report <- data.frame("BF" = BFs,
                         "SEP" = SEPs,
                         "BFerror" = BFerrors)
    repid <- paste("report wi_es",delta.mu,"_n",n,".csv",sep="")
    write.csv(report, repid, row.names = F)
  }
}


bf <- bf.sd <- sep <- sep.sd <- numeric(15)
i <- 1
setwd("~/Downloads/Psychonomic Bulletin & Review/Datasets/Within-Subject Designs (12)/varying sample sizes") #YOUR PATH
for (delta.mu in c(0, 3.8, 6.7)) {
  for (n in c(5, 10, 24, 36, 48)) {
    filepath <- paste("report wi_es",delta.mu,"_n",n,".csv",sep="")
    replication <- read.csv(filepath)
    dat1 <- subset(replication, BF>2.9 & BF<3.1 & BFerror<.25)
    sep[i] <- mean(abs(dat1$SEP))
    sep.sd[i] <- sd(abs(dat1$SEP))
    dat2 <- subset(replication, abs(SEP)>99 & abs(SEP)<101 & BFerror<.25)
    bf[i] <- mean(dat2$BF)
    bf.sd[i] <- sd(dat2$BF)
    i = i + 1
  }
}
df <- data.frame("n"=c(3.5, 8.5, 22.5, 34.5, 46.5, 5, 10, 24, 36, 48, 6.5, 11.5, 25.5, 37.5, 49.5),
                 "BF"=bf,"BF.sd"=bf.sd,"SEP"=sep,"SEP.sd"=sep.sd,
                 "ES"=factor(rep(c("Null","Small", "Larger"), each=5),
                             levels=c("Null","Small","Larger"))) #manually jittered (geom_jitter is uncontrollable)
df$SEM <- 6.67/sqrt(df$n)
df[1,3] <- 1

library(ggplot2)
fig5b <- ggplot(df, aes(n,SEP,shape=ES,color=ES,size=SEP.sd))+
  geom_point()+
  scale_size_continuous(range=c(1,8))+
  scale_color_manual(values=c("#999999","#E69F00","#56B4E9"))+
  scale_x_continuous(breaks=c(5,10,24,36,48),limits=c(0,50),expand=c(0,0))+
  labs(x="Sample Size",y=expression(italic("Sep%")),
       color="Effect Size",shape="Effect Size",size="SD")+
  guides(color=guide_legend(override.aes=list(size=5)))+
  theme_classic()+theme(text=element_text(size=23),
                        plot.margin=margin(1,0.5,0.5,0.5,"cm"),
                        legend.margin=margin(0,0.85,0.5,0.85,"cm"))

fig5a <- ggplot(df, aes(n,BF,shape=ES,color=ES,size=BF.sd))+
  geom_point()+
  scale_size_continuous(range=c(1,8))+
  scale_color_manual(values=c("#999999","#E69F00","#56B4E9"))+
  scale_x_continuous(breaks=c(5,10,24,36,48),limits=c(0,50),expand=c(0,0))+
  labs(x="Sample Size",y=expression(italic("BF")[10]),
       color="Effect Size",shape="Effect Size",size="SD")+
  guides(color=guide_legend(override.aes=list(size=5)))+
  theme_classic()+theme(text=element_text(size=23),
                        plot.margin=margin(1,0.5,0.5,0.5,"cm"),
                        legend.margin=margin(0,0.85,0.5,0.85,"cm"))

library(ggpubr)
ggarrange(fig5a,fig5b,ncol=2,nrow=1,labels=c("A","B"),align="h",
          common.legend=T,legend="bottom",font.label=list(size=23))
ggsave("~/Desktop/Figure 5.png", width=3700, height=1600, units="px", device="png", dpi=300)
 
