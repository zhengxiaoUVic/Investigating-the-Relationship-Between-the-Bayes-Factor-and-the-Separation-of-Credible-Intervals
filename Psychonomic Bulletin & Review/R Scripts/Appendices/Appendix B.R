library(rmBayes) #version 0.1.16, rstan 2.32.5, macOS (Apple chip)

## Dataset is in the long format.
rmHDI(recall.long, seed = 277, whichSubject = "Subject", whichLevel = "Level", whichResponse = "Response")
# $HDI
#           lower    upper
# Level1 10.45911 11.58175
# Level2 12.37533 13.49797
# Level3 13.53146 14.65410
# 
# $`posterior means`
#   Level1   Level2   Level3 
# 11.02043 12.93665 14.09278 
# 
# $width
# [1] 0.5613217


library(rmBayes) #version 0.1.15, macOS (Apple chip)

rmHDI(recall.long, seed = 277, whichSubject = "Subject", whichLevel = "Level", whichResponse = "Response")
# $HDI
#           lower    upper
# Level1 10.47101 11.59361
# Level2 12.39176 13.51436
# Level3 13.55086 14.67346
# 
# $`posterior means`
#   Level1   Level2   Level3 
# 11.03231 12.95306 14.11216 
# 
# $width
# [1] 0.5613014

## Dataset is in the wide format.
rmHDI(data.wide = recall.wide, seed = 277)


library(BayesFactor) #version 0.9.12-4.7

anovaHDI <- function(data, whichSubject, whichLevel, whichResponse, cred, iter) {
  #' input arguments are same defined as in the rmHDI function
  n <- length(unique(data[,whichSubject]))
  a <- length(unique(data[,whichLevel]))
  BF <- BayesFactor::anovaBF(as.formula(paste(whichResponse, "~", whichLevel, "+", whichSubject)), data = data, whichRandom = whichSubject, iterations = iter, progress = FALSE)
  chains <- BayesFactor::posterior(BF, iterations = iter, progress = FALSE)
  mu.chains <- chains[,2:(a+1)] + chains[,1]
  widths <- qt((1 + cred) / 2, df = a * (n - 1)) * sqrt(chains[,"sig2"] / n)
  uprs <- mu.chains + widths
  lwrs <- mu.chains - widths
  matrix(c(colMeans(lwrs),colMeans(uprs)), nrow = a, dimnames = list(paste("Level",1:a), c("lower","upper")))
}
set.seed(277)
anovaHDI(recall.long, "Subject", "Level", "Response", .95, 1e5)
#            lower    upper
# Level 1 10.50752 11.64187
# Level 2 12.41498 13.54934
# Level 3 13.56208 14.69644

set.seed(277)
(BF <- anovaBF(Response ~ Level + Subject, data = recall.long, whichRandom = "Subject", iterations = 1e5, progress = FALSE))
# Bayes factor analysis
# --------------
# [1] Level + Subject : 36469.12 ±0.32%
#
# Against denominator:
#   Response ~ Subject
# ---
# Bayes factor type: BFlinearModel, JZS


head(recall.long, 3)
#   Subject  Level Response
# 1      s1 Level1       10
# 2      s2 Level1        6
# 3      s3 Level1       11

head(recall.wide, 3)
#    Level1 Level2 Level3
# s1     10     13     13
# s2      6      8      8
# s3     11     14     14

