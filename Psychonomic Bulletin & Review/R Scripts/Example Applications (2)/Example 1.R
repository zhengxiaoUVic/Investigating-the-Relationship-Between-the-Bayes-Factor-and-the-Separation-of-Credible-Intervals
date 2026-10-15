dat <- read.table("~/Downloads/Psychonomic Bulletin & Review/R Scripts/Example Applications (2)/data_from_Bub_et_al.txt",
                  header = T, stringsAsFactors = T)
library(ez)
ezANOVA(data = dat, dv = RT, wid = subj, within = .(cond), detailed = TRUE)
# $ANOVA
#        Effect DFn DFd         SSn        SSd          F            p p<.05        ges
# 1 (Intercept)   1  36 51403224.89 1745662.11 1060.06545 2.654765e-28     * 0.96631308
# 2        cond   3 108    23016.68   46320.32   17.88848 1.696219e-09     * 0.01268137
# 
# $`Mauchly's Test for Sphericity`
#   Effect         W         p p<.05
# 2   cond 0.8976548 0.5862303      
# 
# $`Sphericity Corrections`
#   Effect       GGe        p[GG] p[GG]<.05      HFe        p[HF] p[HF]<.05
# 2   cond 0.9313836 5.432191e-09         * 1.017728 1.696219e-09         *

ezStats(data = dat, dv = RT, wid = subj, within = .(cond))
#   cond  N     Mean       SD     FLSD
# 1 ACOM 37 573.1081 109.1077 9.543984
# 2 AINC 37 581.3243 108.8462 9.543984
# 3 MCOM 37 600.9189 115.1567 9.543984
# 4 MINC 37 602.0000 112.9789 9.543984 

library(rmBayes) #version 0.1.16, rstan 2.32.5, macOS (Apple chip)
rmHDI(data = dat, whichSubject = "subj", whichLevel = "cond", whichResponse = "RT", method = 0)
# $HDI
#         lower    upper
# ACOM 567.2801 578.9361
# AINC 575.4964 587.1523
# MCOM 595.0910 606.7469
# MINC 596.1720 607.8280
# 
# $`arithmetic means`
#     ACOM     AINC     MCOM     MINC 
# 573.1081 581.3243 600.9189 602.0000 
# 
# $width
# [1] 5.827966

rmHDI(data = dat, whichSubject = "subj", whichLevel = "cond", whichResponse = "RT", warmup = 2000, iter = 10000, method = 1, seed = 277)
# $HDI
#         lower    upper
# ACOM 566.2211 579.7843
# AINC 574.0967 587.6598
# MCOM 592.8239 606.3871
# MINC 593.8744 607.4376
# 
# $`posterior means`
#     ACOM     AINC     MCOM     MINC 
# 573.0027 580.8782 599.6055 600.6560 
# 
# $width
# [1] 6.781591

rmHDI(data = dat, whichSubject = "subj", whichLevel = "cond", whichResponse = "RT", warmup = 2000, iter = 10000, method = 2, seed = 277)
# $HDI
#         lower    upper
# ACOM 566.5446 580.1106
# AINC 574.4644 588.0305
# MCOM 593.2820 606.8481
# MINC 594.3109 607.8770
# 
# $`posterior means`
#     ACOM     AINC     MCOM     MINC 
# 573.3276 581.2475 600.0650 601.0939
# 
# $width
# [1] 6.783037

rmHDI(data = dat, whichSubject = "subj", whichLevel = "cond", whichResponse = "RT", warmup = 2000, iter = 10000, method = 3, seed = 277)
# $HDI
#         lower    upper
# ACOM 567.7311 581.3125
# AINC 575.2580 588.8394
# MCOM 593.2537 606.8351
# MINC 594.2531 607.8345
# 
# $`posterior means`
#     ACOM     AINC     MCOM     MINC 
# 574.5218 582.0487 600.0444 601.0438 
# 
# $width
# [1] 6.790702


library(BayesFactor) #version 0.9.12-4.7
set.seed(277)
anovaBF(RT ~ cond + subj, data = dat, whichRandom = "subj", iterations = 100000)
# Bayes factor analysis
# --------------
# [1] cond + subj : 5714759 ±0.24%
# 
# Against denominator:
#   RT ~ subj 
# ---
# Bayes factor type: BFlinearModel, JZS

dat1 <- droplevels(subset(dat, cond %in% c("ACOM", "AINC")))
head(dat1)
#     RT subj cond
# 1  576  s01 ACOM
# 2  583  s01 AINC
# 5  450  s02 ACOM
# 6  442  s02 AINC
# 9  492  s03 ACOM
# 10 494  s03 AINC

anovaBF(RT ~ cond + subj, data = dat1, whichRandom = "subj", iterations = 100000)
# Bayes factor analysis
# --------------
# [1] cond + subj : 1.084064 ±0.8%
# 
# Against denominator:
#   RT ~ subj 
# ---
# Bayes factor type: BFlinearModel, JZS

rmHDI(data = dat1,
      whichSubject = "subj", whichLevel = "cond", whichResponse = "RT", 
      warmup = 2000, iter = 10000, method = 1, seed = 1)
# $HDI
#         lower    upper
# ACOM 566.7026 579.0215
# AINC 574.5815 586.9004
# 
# $`posterior means`
#     ACOM     AINC 
# 572.8621 580.7410 
# 
# $width
# [1] 6.159446

##SEPARATION PERCENTAGE
(580.7410 - 572.8621) / (2*6.159446) #0.6395786

rmHDI(data = dat1,
      whichSubject = "subj", whichLevel = "cond", whichResponse = "RT", 
      warmup = 2000, iter = 10000, method = 1, seed = 2)
# $HDI
#         lower    upper
# ACOM 565.0592 577.4174
# AINC 572.8918 585.2500
# 
# $`posterior means`
#     ACOM     AINC 
# 571.2383 579.0709 
# 
# $width
# [1] 6.179093


(579.0709 - 571.2383) / (2*6.179093) #0.6337985


dat2 <- droplevels(subset(dat, cond %in% c("ACOM", "MCOM")))
head(dat2)
#     RT subj cond
# 1  576  s01 ACOM
# 3  566  s01 MCOM
# 5  450  s02 ACOM
# 7  463  s02 MCOM
# 9  492  s03 ACOM
# 11 513  s03 MCOM

anovaBF(RT ~ cond + subj, data = dat2, whichRandom = "subj", iterations = 100000)
# Bayes factor analysis
# --------------
# [1] cond + subj : 5175.329 ±0.72%
# 
# Against denominator:
#   RT ~ subj 
# ---
# Bayes factor type: BFlinearModel, JZS

