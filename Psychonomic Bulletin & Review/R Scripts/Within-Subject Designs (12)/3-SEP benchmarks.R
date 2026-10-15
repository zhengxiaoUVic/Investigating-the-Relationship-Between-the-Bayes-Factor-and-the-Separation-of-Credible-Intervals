lwr <- 2.9 #1/3.1
upr <- 3.1 #1/2.9
err <- .25
setwd("~/Downloads/Psychonomic Bulletin & Review") #YOUR PATH

## Find descriptive statistics such as 
##  (1) count, (2) min, (3) max, (4) arithmetic mean, (5) median, (6) sample standard deviation
##  of six types of intervals' separation percentages (LM.CI, NKM.HDI, LH.HDI, anova.HDI, JZS.HDI, HPDI),
##  when their corresponding Bayes factor value is in the range of "lwr" and "upr",
##  and the Monte Carlo error is less than "err".

tab.SEP.LM.CI <- tab.SEP.NKM.HDI <- tab.SEP.LH.HDI <- tab.SEP.HPDI <- tab.SEP.anova.HDI <- tab.SEP.JZS.HDI <- matrix(NaN,nrow=12,ncol=6)
for (i in c(1:12)) {
  filepath <- paste("Datasets/Within-Subject Designs (12)/report wi_",i,".csv",sep="")
  replication <- read.csv(filepath)
  slicing <- subset(replication,BF>lwr&BF<upr&BFerror<err)
  dat <- abs(slicing$SEP.LM.CI)
  tab.SEP.LM.CI[i,] <- c(length(dat),
                         min(dat),
                         max(dat),
                         mean(dat),
                         median(dat),
                         sd(dat))
  
  dat <- abs(slicing$SEP.NKM.HDI)
  tab.SEP.NKM.HDI[i,] <- c(length(dat),
                           min(dat),
                           max(dat),
                           mean(dat),
                           median(dat),
                           sd(dat))
  
  dat <- abs(slicing$SEP.LH.HDI)
  tab.SEP.LH.HDI[i,] <- c(length(dat),
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
  
  dat <- abs(slicing$SEP.anova.HDI)
  tab.SEP.anova.HDI[i,] <- c(length(dat),
                             min(dat),
                             max(dat),
                             mean(dat),
                             median(dat),
                             sd(dat))
  
  dat <- abs(slicing$SEP.JZS.HDI)
  tab.SEP.JZS.HDI[i,] <- c(length(dat),
                           min(dat),
                           max(dat),
                           mean(dat),
                           median(dat),
                           sd(dat))
}
tab.SEP.LM.CI <- as.data.frame(tab.SEP.LM.CI)
colnames(tab.SEP.LM.CI) <- c("count","min","max","mean","median","sd")

tab.SEP.NKM.HDI <- as.data.frame(tab.SEP.NKM.HDI)
colnames(tab.SEP.NKM.HDI) <- c("count","min","max","mean","median","sd")

tab.SEP.LH.HDI <- as.data.frame(tab.SEP.LH.HDI)
colnames(tab.SEP.LH.HDI) <- c("count","min","max","mean","median","sd")

tab.SEP.HPDI <- as.data.frame(tab.SEP.HPDI)
colnames(tab.SEP.HPDI) <- c("count","min","max","mean","median","sd")

tab.SEP.anova.HDI <- as.data.frame(tab.SEP.anova.HDI)
colnames(tab.SEP.anova.HDI) <- c("count","min","max","mean","median","sd")

tab.SEP.JZS.HDI <- as.data.frame(tab.SEP.JZS.HDI)
colnames(tab.SEP.JZS.HDI) <- c("count","min","max","mean","median","sd")

