library(ggplot2)

setwd("~/Downloads/Revision/Datasets") #YOUR PATH
err <- .01; yTruncated <- 1000
ann.text.size <- 7; theme.text.size <- 23; legend.size <- 4; geom.pt.size <- 0.5

## Between-Subjects Designs
dat.btw <- read.csv("btw_dat.csv", stringsAsFactors=T)
dat.btw$Effect.Size <- factor(dat.btw$Effect.Size,
                              levels=c("Null","Small","Large","Extreme"))
dat.btw <- subset(dat.btw, BFerror<err)
set.seed(277)
dat.btw <- dat.btw[sample(1:nrow(dat.btw)),]
panelA.btw <- ggplot(subset(dat.btw, BF<yTruncated & Case!="None"),
             aes(abs(SEP.CI), log(BF), color=Effect.Size))+
  geom_point(size=geom.pt.size, alpha=.1)+
  scale_color_manual(values=c("#F9DC4A", "#EC5A40", "#204FF5", "black"), drop=F)+
  labs(x=expression("CI"~italic("Sep%")), y=expression("ln("~italic("BF")[10]~")"),color="Effect Size")+
  annotate(geom="text",x=15,y=-1.8,parse=T,size=ann.text.size,
           label=as.character(expression(italic("n")~"=48")))+
  annotate(geom="text",x=15,y=-0.7,parse=T,size=ann.text.size,
           label=as.character(expression(italic("n")~"=24")))+
  annotate(geom="text",x=170,y=6,parse=T,size=ann.text.size,
           label=as.character(expression(italic("n")~"=24")))+
  annotate(geom="text",x=135,y=6,parse=T,size=ann.text.size,
           label=as.character(expression(italic("n")~"=48")))+
  ggtitle("Between-Subjects Designs")+
  theme_classic()+
  theme(text=element_text(size=theme.text.size),legend.position="bottom")+
  guides(colour=guide_legend(override.aes=list(size=legend.size, alpha=1)))

panelE <- ggplot(subset(dat.btw, Report %in% c(paste0("Report",c(1,3,5)),"Null24")),
               aes(abs(SEP.CI),log(BF), color=Effect.Size))+
  geom_point(size=geom.pt.size, alpha=.1)+
  scale_color_manual(values=c("#F9DC4A", "#EC5A40", "#204FF5", "black"))+
  labs(x="",y="")+
  ggtitle(expr(paste(bold("E"),"   Zoom out"~italic("n"),"=24",sep="")))+
  theme_classic()+
  theme(text=element_text(size=theme.text.size-5),legend.position="Null",
        plot.title=element_text(hjust=-0.05),
        plot.margin=unit(c(0,0,0,0),"null"),
        panel.spacing=unit(c(0,0,0,0),"null"))





## Within-Subjects Designs
twelve_in_one <- read.csv("wi_dat.csv", stringsAsFactors=T)
twelve_in_one$Effect.Size...Rho <- factor(twelve_in_one$Effect.Size...Rho,
                                          levels=c("Null & 0.3","Small & 0.3","Large & 0.3",
                                                   "Null & 0.7","Small & 0.7","Large & 0.7",
                                                   "Null & 0.9","Small & 0.9","Large & 0.9"))
dat.wi <- subset(twelve_in_one, BF<yTruncated & BFerror<err & Rho!="0.7")
set.seed(277)
dat.wi <- dat.wi[sample(1:nrow(dat.wi)),]
panelB.wi <- ggplot(dat.wi, aes(abs(SEP.anova.HDI), log(BF), color=Effect.Size...Rho))+
  geom_point(size=geom.pt.size, alpha=.1)+
  scale_color_manual(values=c("#bac2ff", "#4e91fd", "#0229bf", "#ffb1b1", "#ff4848", "#ff0000"))+
  labs(x=expression("JZS-HDI"~italic("Sep%")), y=expression("ln("~italic("BF")[10]~")"))+
  annotate(geom="text",x=15,y=-1.8,parse=T,size=ann.text.size,
           label=as.character(expression(italic("n")~"=48")))+
  annotate(geom="text",x=15,y=-0.7,parse=T,size=ann.text.size,
           label=as.character(expression(italic("n")~"=24")))+
  annotate(geom="text",x=167,y=6,parse=T,size=ann.text.size,
           label=as.character(expression(italic("n")~"=24")))+
  annotate(geom="text",x=125,y=6,parse=T,size=ann.text.size,
           label=as.character(expression(italic("n")~"=48")))+
  ggtitle("Within-Subject Designs")+
  theme_classic()+
  theme(text=element_text(size=theme.text.size),legend.position="bottom")+
  guides(colour=guide_legend(override.aes=list(size=legend.size, alpha=1),nrow=2,byrow=T,
                             title=expression("Effect Size &"~italic(ρ)~" ")))

panelD.n24 <- ggplot(subset(dat.wi, BF>10 & Report %in% c(paste0("Report", c(1,3,4,6)),"Null1","Null3")),
              aes(abs(SEP.anova.HDI), log(BF), color=factor(Rho)))+
  geom_point(size=geom.pt.size, alpha=.08)+
  scale_color_manual(values=c("#0229bf", "#ff0000"))+
  labs(x="",y="")+
  scale_x_continuous(expand=c(0,0),breaks=c(90,110,130,150,170),limits=c(89,171))+
  scale_y_continuous(breaks=c(2:7))+
  ggtitle(expr(paste(bold("D"),"   Zoom in"~italic("n"),"=24",sep="")))+
  theme_classic()+
  theme(text=element_text(size=theme.text.size-5),legend.position=c(0.9,0.3),
        plot.title=element_text(hjust=-0.05),
        legend.direction="horizontal",
        plot.margin=unit(c(0,0,0,0),"null"),
        panel.spacing=unit(c(0,0,0,0),"null"))+
  guides(colour=guide_legend(override.aes=list(size=legend.size/2, alpha=1),nrow=2,byrow=T,
                             title=expression(italic(ρ)~" ")))

panelC.n48 <- ggplot(subset(dat.wi, BF>10 & Report %in% c(paste0("Report", c(7,9,10,12)),"Null10","Null12")),
              aes(abs(SEP.anova.HDI), log(BF), color=factor(Rho)))+
  geom_point(size=geom.pt.size, alpha=.08)+
  scale_color_manual(values=c("#0229bf", "#ff0000"))+
  labs(x="",y="")+
  scale_x_continuous(expand=c(0,0),breaks=c(90,110,130,150,170),limits=c(89,171))+
  scale_y_continuous(breaks=c(2:7))+
  ggtitle(expr(paste(bold("C"),"   Zoom in"~italic("n"),"=48",sep="")))+
  theme_classic()+
  theme(text=element_text(size=theme.text.size-5),legend.position=c(0.9,0.3),
        plot.title=element_text(hjust=-0.05),
        legend.direction="horizontal",
        plot.margin=unit(c(0,0,0,0),"null"),
        panel.spacing=unit(c(0,0,0,0),"null"))+
  guides(colour=guide_legend(override.aes=list(size=legend.size/2, alpha=1),nrow=2,byrow=T,
                             title=expression(italic(ρ)~" ")))


library(cowplot)
library(ggpubr)

p <- ggarrange(panelA.btw,panelB.wi,ncol=2,nrow=1,labels=c("A","B"),align="h",font.label=list(size=theme.text.size))

p.inserted <- ggdraw()+
  draw_plot(p)+
  draw_plot(panelC.n48, x=0.57, y=.6, width=.2, height=.28)+
  draw_plot(panelD.n24, x=0.78, y=.22, width=.2, height=.28)+
  draw_plot(panelE, x=0.3, y=.22, width=.2, height=.28)

ggsave("~/Desktop/Figure 2.png", width=5550, height=2400, units="px", device="png", dpi=300)





## CACHE
BFmin <- 1
BFmax <- 3
ggplot(subset(twelve_in_one, BF<BFmax & BF>BFmin & BFerror<err & Rho!="0.7"),
       aes(abs(SEP.anova.HDI),BF,color=Effect.Size...Rho))+
  geom_point(size=geom.pt.size, alpha=.1)+
  scale_color_manual(values=c("#CCBB44", "#EE6677", "#4477AA", "#228833", "#AA3377", "#66CCEE"))+
  labs(x=expression("JZS-HDI"~italic("Sep%")),y=expression(italic("BF")[10]))+
  ggtitle("Within-Subject Designs")+
  theme_classic()+
  theme(text=element_text(size=theme.text.size),legend.position="bottom")+
  guides(colour=guide_legend(override.aes=list(size=legend.size, alpha=1),nrow=2,byrow=T,
                             title=expression("Effect Size +"~italic(ρ)~" ")))

BF01min <- 3
BF01max <- 5
ggplot(subset(twelve_in_one, 1/BF<BF01max & 1/BF>BF01min & BFerror<err & Rho!="0.7"),
       aes(abs(SEP.anova.HDI),1/BF,color=Effect.Size...Rho))+
  geom_point(size=geom.pt.size, alpha=.1)+
  scale_color_manual(values=c("#CCBB44", "#EE6677", "#4477AA", "#228833", "#AA3377", "#66CCEE"))+
  labs(x=expression("JZS-HDI"~italic("Sep%")),y=expression(italic("BF")["01"]))+
  ggtitle("Within-Subject Designs")+
  theme_classic()+
  theme(text=element_text(size=theme.text.size),legend.position="bottom")+
  guides(colour=guide_legend(override.aes=list(size=legend.size, alpha=1),nrow=2,byrow=T,
                             title=expression("Effect Size +"~italic(ρ)~" ")))

