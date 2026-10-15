library(BayesFactor) #version 0.9.12-4.4

setwd("~/Downloads/Psychonomic Bulletin & Review") #YOUR PATH
filepath <- "R Scripts/Between-Subjects Designs (4)/1-params_Btw.csv"
params <- read.csv(filepath)
#    n  m1   m2 sd power
# 1 24 100 91.5 20   0.3
# 2 48 100 94.1 20   0.3
# 3 24 100 83.5 20   0.8
# 4 48 100 88.4 20   0.8

alpha <- .05; m1 <- 100; sd <- 20; NSim <- 1e4
setwd("~/Desktop/btw.sim") #YOUR PATH

for (rindex in 1:4) {
  n <- params$n[rindex]
  m2 <- params$m2[rindex]
  BFs <- SEPs.ci <- SEPs.hpdi <- BFerrors <- numeric(NSim)
  for (i in 1:NSim) {
    # Generate normally distributed response data with two conditions for the between-subjects design.
    # Long format data frame.
    set.seed(i+NSim*rindex)
    data <- data.frame("DV" = c(rnorm(n = n, mean = m1, sd = sd),
                                rnorm(n = n, mean = m2, sd = sd)),
                       "Level" = factor(rep(c("Level1","Level2"), each = n)))
    
    # Variance within samples.
    MSw <- anova(lm(DV ~ Level, data = data))["Residuals", "Mean Sq"]
    
    # (1) Between-subjects confidence interval.
    CI.halfwidth <- qt(1-alpha/2, df = 2*(n-1)) * sqrt(MSw / n)
    mean.Level1 <- mean(subset(data, Level=="Level1")$DV)
    mean.Level2 <- mean(subset(data, Level=="Level2")$DV)
    
    # Separation percentage.
    SEPs.ci[i] <- 50 * (mean.Level1 - mean.Level2) / CI.halfwidth
    
    # (2) Standard highest posterior density interval (HPDI).
    # Bayes factor object.
    #BF <- ttestBF(formula = DV ~ Level, data = data, progress = FALSE)
    BF <- anovaBF(DV ~ Level, data = data, progress = FALSE)
    BFs[i] <- extractBF(BF)$bf
    BFerrors[i] <- extractBF(BF)$error
    
    # Sample from the posterior distribution of the numerator of the Bayes factor object.
    chains <- posterior(BF, iterations = 1e5, progress = FALSE)
    mu.chains <- chains[,2:3] + chains[,1]
    HPDI <- HPDinterval(mu.chains, prob = 1 - alpha)
    
    length <- sqrt(sum((HPDI[,2] - HPDI[,1])^2) / 2)
    SEPs.hpdi[i] <- unname(-100 * diff(rowMeans(HPDI)) / length)
  }
  report <- data.frame("BF" = BFs,
                       "SEP.CI" = SEPs.ci,
                       "SEP.HPDI" = SEPs.hpdi,
                       "BFerror" = BFerrors)
  repid <- paste("report_", rindex, ".csv", sep="")
  write.csv(report, repid, row.names = FALSE)
}

