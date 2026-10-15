set.seed(445)
x <- rnorm(48, 100, 20)  #set origin scores
x1 <- rnorm(48, 0, 10)   #error variability
y1 <- round(x + x1)      #first set of scores
x2 <- rnorm(48, 5, 10)   #error variability
y2 <- round(x + x2)      #second set of scores
x3 <- rnorm(48, 7, 10)   #error variability
y3 <- round(x + x3)      #third set of scores

cor(y1, y2) #0.8705897
cor(y1, y3) #0.8736831
cor(y3, y2) #0.8936414
round(cov(data.frame(y1, y2, y3)))  # show var and covar
#     y1  y2  y3
# y1 499 467 430
# y2 467 577 473
# y3 430 473 486

dat <- data.frame("subj" = rep(paste0("s", 1:48), 3),
                  "score" = c(y1, y2, y3),
                  "cond" = rep(paste0("c", 1:3), each = 48), stringsAsFactors = TRUE)

library(ez)
ezANOVA(data = dat, dv = score, wid = subj, within = .(cond), detailed = TRUE)
# $ANOVA
#        Effect DFn DFd         SSn       SSd          F            p p<.05        ges
# 1 (Intercept)   1  47 1349275.840 67391.160 941.013105 9.797138e-33     * 0.94841721
# 2        cond   2  94    1034.347  5993.653   8.110967 5.628749e-04     * 0.01389894
# 
# $`Mauchly's Test for Sphericity`
#   Effect         W         p p<.05
# 2   cond 0.9864562 0.7307849      
# 
# $`Sphericity Corrections`
#   Effect       GGe        p[GG] p[GG]<.05      HFe        p[HF] p[HF]<.05
# 2   cond 0.9866372 0.0006005852         * 1.029579 0.0005628749         *

ezStats(data = dat, dv = score, wid = subj, within = .(cond))
#   cond  N     Mean       SD     FLSD
# 1   c1 48 93.20833 22.33493 3.236319
# 2   c2 48 97.54167 24.01326 3.236319
# 3   c3 48 99.64583 22.04298 3.236319

library(rmBayes) #version 0.1.16, rstan 2.32.5, macOS (Apple chip)
rmHDI(data = dat, whichSubject = "subj", whichLevel = "cond", whichResponse = "score", warmup = 2000, iter = 10000, method = 1, seed = 277)
# $HDI
#       lower     upper
# c1 91.02366  95.61049
# c2 95.16026  99.74709
# c3 97.18111 101.76794
# 
# $`posterior means`
#       c1       c2       c3 
# 93.31708 97.45368 99.47452 
# 
# $width
# [1] 2.293415


dat1 <- droplevels(subset(dat, cond != "c3"))
library(BayesFactor) #version 0.9.12-4.7
set.seed(277)
anovaBF(score ~ cond + subj, data = dat1, whichRandom = "subj", iterations = 10000)
# Bayes factor analysis
# --------------
# [1] cond + subj : 2.923266 ±0.73%
# 
# Against denominator:
#   score ~ subj 
# ---
# Bayes factor type: BFlinearModel, JZS

rmHDI(data = dat1, whichSubject = "subj", whichLevel = "cond", whichResponse = "score", warmup = 2000, iter = 10000, method = 1, seed = 277)
# $HDI
#       lower    upper
# c1 90.78038 95.69127
# c2 94.95834 99.86923
# 
# $`posterior means`
#       c1       c2 
# 93.23582 97.41379 
# 
# $width
# [1] 2.455443


dat2 <- droplevels(subset(dat, cond != "c2"))
anovaBF(score ~ cond + subj, data = dat2, whichRandom = "subj", iterations = 10000)
# Bayes factor analysis
# --------------
# [1] cond + subj : 98.41587 ±0.92%
# 
# Against denominator:
#   score ~ subj 
# ---
# Bayes factor type: BFlinearModel, JZS

rmHDI(data = dat2, whichSubject = "subj", whichLevel = "cond", whichResponse = "score", warmup = 2000, iter = 10000, method = 1, seed = 277)
# $HDI
#       lower     upper
# c1 90.97664  95.59618
# c3 97.21662 101.83616
# 
# $`posterior means`
#       c1       c3 
# 93.28641 99.52639 
# 
# $width
# [1] 2.309771

