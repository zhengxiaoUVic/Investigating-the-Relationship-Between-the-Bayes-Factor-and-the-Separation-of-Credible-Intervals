{
  n <- 24 #number of subjects in a within-subject design
  mu.b <- 100 #condition mean for Level 1
  delta.mu <- 6.7 #effect size
  sigma.b <- 20 #between-subjects standard deviation
  sigma.e <- 6.67 #error standard deviation
  NSim <- 1e4; rindex <- 1; i <- 239 #reproducibility
  set.seed(i+NSim*rindex)
  TrueScore <- rnorm(n, mu.b, sigma.b)
  eij1 <- rnorm(n, 0, sigma.e)
  eij2 <- rnorm(n, 0, sigma.e)
} #parameters for simulated data


# Wide format data:
mat <- cbind("Level1" = TrueScore + eij1 + delta.mu,
             "Level2" = TrueScore + eij2)
cor(mat[,1], mat[,2]) #sample corrrelation 0.9346669

{
  row.means <- rowMeans(mat)
  col.means <- colMeans(mat) #sample condition means
  grand.mean <- mean(mat)
  resid <- mat - outer(row.means, col.means, "+")
  SS <- sum((resid + grand.mean)^2)
  LM.halfwidth <- qt(1-.05/2, df = (n-1)) * sqrt(SS / (n * (n - 1))) #interval width of LM-CI
} #sum-of-Squares for Subject x Condition interaction

# Long format data:
dat <- data.frame("Subject" = factor(rep(c(1:n), 2)),
                  "Level" = factor(rep(c("Level1", "Level2"), each = n)),
                  "DV" = as.vector(mat))


library(BayesFactor) #v0.9.12-4.7
set.seed(277)
(BF <- anovaBF(DV ~ Level + Subject, data=dat,
               whichRandom="Subject", iterations=1e5, progress=F)) #50.40034 ±0.48%

library(rmBayes) #v0.1.16; rstan v2.32.5; macOS (Apple chip)
NKM <- rmHDI(dat, whichResponse="DV", method=0) #NKM-HDI
LH <- rmHDI(dat, whichResponse="DV", method=4, seed=277) #LH-HDI
JZS <- rmHDI(dat, whichResponse="DV", seed=277, treat="fixed") #JZS-HDI
HDI <- rmHDI(dat, whichResponse="DV", design="between", seed=277) #standard HDI


df <- data.frame("level"=factor(rep(c("Level 1","Level 2"),5)),
                 "means"=c(t(rbind(col.means,
                                   NKM$`arithmetic means`,
                                   LH$`posterior means`,
                                   JZS$`posterior means`,
                                   HDI$`posterior means`))),
                 "Method"=factor(rep(c("LM-CI","NKM-HDI","LH-HDI","JZS-HDI","Standard HDI"),each=2),
                                 levels=c("LM-CI","NKM-HDI","LH-HDI","JZS-HDI","Standard HDI")),
                 "upper"=c(col.means+LM.halfwidth,
                           NKM$HDI[,2],
                           LH$HDI[,2],
                           JZS$HDI[,2],
                           HDI$HDI[,2]),
                 "lower"=c(col.means-LM.halfwidth,
                           NKM$HDI[,1],
                           LH$HDI[,1],
                           JZS$HDI[,1],
                           HDI$HDI[,1]))

library(ggplot2) #v3.5.0
fig <- ggplot(df,aes(level,means))+
  geom_hline(yintercept=c(85,90,95,100,105), color="gray")+
  geom_errorbar(aes(ymin=lower,ymax=upper,color=Method),width=.35,
                position=position_dodge(.5),linewidth=1.7)+
  scale_color_manual(values=c("#a1a1a1", "#5494cb", "#ff7411", "#8263d9", "#4f4f4f"))+
  geom_point(aes(color=Method),size=3.5,position=position_dodge(.5))+
  labs(x="Within-Subject Factor",y="Response",color="Method  ",
       title=expression(italic("BF")[10] == 50 ~ ",   " ~ hat(italic(ρ)) == .9 ~ ",   " ~ italic("n") == 24))+
  theme_bw()+
  theme(text=element_text(size=22),legend.position="bottom",
        legend.key.width=unit(2.5,"line"),legend.text=element_text(margin=margin(0,0.6,0,0.6,"cm")))+
  guides(color=guide_legend(nrow=2))
ggsave("~/Desktop/BF50.png", fig, width=2500, height=2000, units="px", device="png", dpi=300)





## CACHE
library(ggplot2) #v3.4.4
fig <- ggplot(df,aes(level,means))+
  geom_hline(yintercept=c(85,90,95,100,105), color="gray")+
  geom_errorbar(aes(ymin=lower,ymax=upper,color=Method),width=.35,
                position=position_dodge(.5),linewidth=1.7)+
  scale_color_manual(values=c("#a1a1a1", "#5494cb", "#ff7411", "#8263d9", "#4f4f4f"))+
  geom_point(aes(color=Method),size=3.5,position=position_dodge(.5))+
  labs(x="Within-Subject Factor",y="Response",
       title=expression(italic("BF")[10] == 50 ~ ",   " ~ hat(italic(ρ)) == .9 ~ ",   " ~ italic("n") == 24))+
  theme_bw()+
  theme(text=element_text(size=22),legend.position="bottom",
        legend.key.width=unit(2.5,"line"),legend.spacing.x=unit(0.6,"cm"))+
  guides(color=guide_legend(nrow=2))
ggsave("~/Desktop/BF50.png", fig, width=2500, height=2000, units="px", device="png", dpi=300)

