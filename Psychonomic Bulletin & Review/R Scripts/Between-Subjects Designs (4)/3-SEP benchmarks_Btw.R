lwr <- 2.9
upr <- 3.1
setwd("~/Downloads/Psychonomic Bulletin & Review") #YOUR PATH

## Find descriptive statistics such as 
##  (1) count, (2) min, (3) max, (4) arithmetic mean, (5) median, (6) sample standard deviation
##  of two types of intervals' separation percentages (CI, HPDI),
##  when their corresponding Bayes factor value is in the range of "lwr" and "upr".

tab.SEP.CI <- tab.SEP.HPDI <- matrix(NaN,nrow=4,ncol=6)
for (i in 1:4) {
  filepath <- paste("Datasets/Between-Subjects Designs (4)/report_",i,".csv",sep="")
  replication <- read.csv(filepath)
  slicing <- subset(replication,BF>lwr&BF<upr)
  dat <- abs(slicing$SEP.CI)
  tab.SEP.CI[i,] <- c(length(dat),
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
}
tab.SEP.CI <- as.data.frame(tab.SEP.CI)
colnames(tab.SEP.CI) <- c("count","min","max","mean","median","sd")

tab.SEP.HPDI <- as.data.frame(tab.SEP.HPDI)
colnames(tab.SEP.HPDI) <- c("count","min","max","mean","median","sd")

