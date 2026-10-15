library(readr)
setwd("~/Downloads/Psychonomic Bulletin & Review") #YOUR PATH
dat1 <- read_table("R Scripts/Appendices/Appendix D/UpDown12 R align.txt")
dat1 <- as.data.frame(dat1)
dat2 <- subset(dat1, resp=="1hand")
dat2$subj <- as.factor(dat2$subj)
dat2$align <- as.factor(dat2$align)


library(rmBayes) #version 0.1.16, rstan 2.32.5, macOS (Apple chip)
rmHDI(dat2,whichSubject="subj",whichLevel="align",whichResponse="RT",seed=277)
# $HDI
#      lower    upper
# A 634.5416 647.0398
# M 650.4478 662.9460
# 
# $`posterior means`
#        A        M 
# 640.7907 656.6969 
# 
# $width
# [1] 6.249123
# 
# Warning messages:
# 1: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#bulk-ess 
# 2: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#tail-ess 

rmHDI(dat2,whichSubject="subj",whichLevel="align",whichResponse="RT",seed=277,iter=1e4)
# $HDI
#      lower    upper
# A 632.7494 645.2037
# M 648.5900 661.0442
# 
# $`posterior means`
#        A        M 
# 638.9765 654.8171
# 
# $width
# [1] 6.227113


library(BayesFactor) #version 0.9.12-4.7
set.seed(277)
BF <- anovaBF(RT ~ align + subj, data = dat2, whichRandom = "subj", 
              iterations = 1e5, progress = F)
chains <- posterior(BF, iterations = 2000, progress = F)
alpha <- .05
n <- length(unique(dat2$subj))
c <- 2
tcrit <- qt(1 - alpha / 2, df = c * (n - 1))
widths <- tcrit * sqrt(chains[,"sig2"] / n)
mu.1 <- chains[,1] + chains[,2]
mu.2 <- chains[,1] + chains[,3]
mean(mu.1) #639.8386
mean(mu.2) #654.9602
mean(widths) #6.384938

