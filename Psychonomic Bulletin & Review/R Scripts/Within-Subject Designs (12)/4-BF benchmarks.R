
## Find descriptive statistics such as 
##  (1) count, (2) min, (3) max, (4) arithmetic mean, (5) median, (6) standard deviation
##  of five types of intervals' Bayes factor values (LM.CI, NKM.HDI, LH.HDI, anova.HDI, JZS.HDI),
##  when their separation percentage is in the range of "lwr" and "upr",
##  and the Monte Carlo error is less than "err".

lwr <- 99
upr <- 101
err <- .25
setwd("~/Downloads/Psychonomic Bulletin & Review") #YOUR PATH

tab.BF.LM.CI <- tab.BF.NKM.HDI <- tab.BF.LH.HDI <- tab.BF.HPDI <- tab.BF.anova.HDI <- tab.BF.JZS.HDI <- matrix(NaN,nrow=12,ncol=6)
for (i in c(1:12)) {
  filepath <- paste("Datasets/Within-Subject Designs (12)/report wi_",i,".csv",sep="")
  replication <- read.csv(filepath)
  dat <- subset(replication,abs(SEP.LM.CI)>lwr&abs(SEP.LM.CI)<upr&BFerror<err)$BF
  tab.BF.LM.CI[i,] <- c(length(dat),
                        min(dat),
                        max(dat),
                        mean(dat),
                        median(dat),
                        sd(dat))
  
  dat <- subset(replication,abs(SEP.NKM.HDI)>lwr&abs(SEP.NKM.HDI)<upr&BFerror<err)$BF
  tab.BF.NKM.HDI[i,] <- c(length(dat),
                          min(dat),
                          max(dat),
                          mean(dat),
                          median(dat),
                          sd(dat))
  
  dat <- subset(replication,abs(SEP.LH.HDI)>lwr&abs(SEP.LH.HDI)<upr&BFerror<err)$BF
  tab.BF.LH.HDI[i,] <- c(length(dat),
                         min(dat),
                         max(dat),
                         mean(dat),
                         median(dat),
                         sd(dat))
  
  dat <- subset(replication,abs(SEP.anova.HDI)>lwr&abs(SEP.anova.HDI)<upr&BFerror<err)$BF
  tab.BF.anova.HDI[i,] <- c(length(dat),
                            min(dat),
                            max(dat),
                            mean(dat),
                            median(dat),
                            sd(dat))
  
  dat <- subset(replication,abs(SEP.JZS.HDI)>lwr&abs(SEP.JZS.HDI)<upr&BFerror<err)$BF
  tab.BF.JZS.HDI[i,] <- c(length(dat),
                          min(dat),
                          max(dat),
                          mean(dat),
                          median(dat),
                          sd(dat))
  
  # dat <- subset(replication,abs(SEP.HPDI)>lwr&abs(SEP.HPDI)<upr&BFerror<err)$BF
  # tab.BF.HPDI[i,] <- c(length(dat),
  #                      min(dat),
  #                      max(dat),
  #                      mean(dat),
  #                      median(dat),
  #                      sd(dat))
}
tab.BF.LM.CI <- as.data.frame(tab.BF.LM.CI)
colnames(tab.BF.LM.CI) <- c("count","min","max","mean","median","sd")

tab.BF.NKM.HDI <- as.data.frame(tab.BF.NKM.HDI)
colnames(tab.BF.NKM.HDI) <- c("count","min","max","mean","median","sd")

tab.BF.LH.HDI <- as.data.frame(tab.BF.LH.HDI)
colnames(tab.BF.LH.HDI) <- c("count","min","max","mean","median","sd")

tab.BF.anova.HDI <- as.data.frame(tab.BF.anova.HDI)
colnames(tab.BF.anova.HDI) <- c("count","min","max","mean","median","sd")

tab.BF.JZS.HDI <- as.data.frame(tab.BF.JZS.HDI)
colnames(tab.BF.JZS.HDI) <- c("count","min","max","mean","median","sd")

# tab.BF.HPDI <- as.data.frame(tab.BF.HPDI)
# colnames(tab.BF.HPDI) <- c("count","min","max","mean","median","sd")

