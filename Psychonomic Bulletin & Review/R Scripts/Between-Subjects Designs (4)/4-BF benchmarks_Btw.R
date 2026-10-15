
## Find descriptive statistics such as 
##  (1) count, (2) min, (3) max, (4) arithmetic mean, (5) median, (6) sample standard deviation
##  of two types of intervals' Bayes factor values (CI, HPDI),
##  when their separation percentage is in the range of "lwr" and "upr".

lwr <- 99
upr <- 101
setwd("~/Downloads/Psychonomic Bulletin & Review") #YOUR PATH

tab.BF.CI <- tab.BF.HPDI <- matrix(NaN,nrow=4,ncol=6)
for (i in 1:4) {
  filepath <- paste("Datasets/Between-Subjects Designs (4)/report_",i,".csv",sep="")
  replication <- read.csv(filepath)
  dat <- subset(replication,abs(SEP.CI)>lwr&abs(SEP.CI)<upr)$BF
  tab.BF.CI[i,] <- c(length(dat),
                     min(dat),
                     max(dat),
                     mean(dat),
                     median(dat),
                     sd(dat))
  
  dat <- subset(replication,abs(SEP.HPDI)>lwr&abs(SEP.HPDI)<upr)$BF
  tab.BF.HPDI[i,] <- c(length(dat),
                       min(dat),
                       max(dat),
                       mean(dat),
                       median(dat),
                       sd(dat))
}
tab.BF.CI <- as.data.frame(tab.BF.CI)
colnames(tab.BF.CI) <- c("count","min","max","mean","median","sd")

tab.BF.HPDI <- as.data.frame(tab.BF.HPDI)
colnames(tab.BF.HPDI) <- c("count","min","max","mean","median","sd")

