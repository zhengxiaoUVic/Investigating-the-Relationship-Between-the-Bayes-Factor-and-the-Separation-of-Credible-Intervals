library(BayesFactor) #version 0.9.12-4.4
library(rmBayes) #version 0.1.15, macOS (Apple chip)

setwd("~/Downloads/Psychonomic Bulletin & Review") #YOUR PATH
filepath <- "R Scripts/Within-Subject Designs (12)/1-params.csv"
params <- read.csv(filepath)

alpha <- .05; mu.b <- 100; sigma.b <- 20; NSim <- 1e4
setwd("~/Desktop/wi.sim") #YOUR PATH

for (rindex in 1:12) {
  n <- params$n[rindex]
  delta.mu <- params$delta.mu[rindex]
  sigma.e <- params$sigma.e[rindex]
  tcrit1 <- qt(1-alpha/2, df = (n-1))
  tcrit2 <- qt(1-alpha/2, df = 2 * (n-1))
  
  BFs <- SEPs.LM.CI <- SEPs.NKM.HDI <- SEPs.LH.HDI <- SEPs.anova.HDI <- SEPs.JZS.HDI <- SEPs.HPDI <- BFerrors <- numeric(NSim)
  
  for (i in 1:NSim) {
    set.seed(i+NSim*rindex)
    TrueScore <- rnorm(n, mu.b, sigma.b)
    eij1 <- rnorm(n, 0, sigma.e)
    eij2 <- rnorm(n, 0, sigma.e)
    
    # Wide format data:
    mat <- cbind("Level1" = TrueScore + eij1 + delta.mu,
                 "Level2" = TrueScore + eij2)
    
    # Long format data:
    dat <- data.frame("Subject" = factor(rep(c(1:n), 2)),
                      "Level" = factor(rep(c("Level1", "Level2"), each = n)),
                      "DV" = as.vector(mat))
    
    # Sum-of-Squares for Subject x Condition interaction.
    row.means <- rowMeans(mat)
    col.means <- colMeans(mat)
    grand.mean <- mean(mat)
    resid <- mat - outer(row.means, col.means, "+")
    SS <- sum((resid + grand.mean)^2)
    
    # (1) Within-subject confidence interval (Loftus & Masson, 1994).
    CI.halfwidth <- tcrit1 * sqrt(SS / (n * (n - 1)))
    # Separation percentage.
    SEPs.LM.CI[i] <- 50 * (col.means[1] - col.means[2]) / CI.halfwidth
    
    # (2) Conditional Bayesian within-subject HDI (Nathoo, Kilshaw, & Masson, 2018).
    HDI.halfwidth <- tcrit2 * sqrt(SS / (n * (n - 1) * 2))
    SEPs.NKM.HDI[i] <- 50 * (col.means[1] - col.means[2]) / HDI.halfwidth
    
    # (3) Bayesian within-subject HDI with MCMC sampling (Heck, 2019).
    HDI.mcmc <- rmHDI(dat, whichResponse="DV", permuted=F, chains=2, method=4, seed=i)
    SEPs.LH.HDI[i] <- 50 * (HDI.mcmc$`posterior means`[1] - HDI.mcmc$`posterior means`[2]) / HDI.mcmc$width
   
    # (4') JZS-HDI
    HDI.JZS <- rmHDI(dat, whichResponse="DV", permuted=F, treat="fixed", chains=2, seed=i)
    SEPs.JZS.HDI[i] <- 50 * (HDI.JZS$`posterior means`[1] - HDI.JZS$`posterior means`[2]) / HDI.JZS$width
    
    # (5) Standard highest posterior density interval (HPDI).
    # Bayes factor object.
    set.seed(277)
    BF <- anovaBF(DV ~ Level + Subject, data = dat, whichRandom = "Subject",
                  iterations = 1e5, progress = FALSE)
    BFs[i] <- extractBF(BF)$bf
    BFerrors[i] <- extractBF(BF)$error
    
    # Sample from the posterior distribution of the numerator of the Bayes factor object.
    set.seed(277)
    chains <- posterior(BF, iterations = 1e5, progress = FALSE)
    mu.chains <- chains[,2:3] + chains[,1]
    HPDI <- HPDinterval(mu.chains, prob = 1 - alpha)
    
    length <- sqrt(sum((HPDI[,2] - HPDI[,1])^2) / 2)
    SEPs.HPDI[i] <- unname(-100 * diff(rowMeans(HPDI)) / length)
    
    # (4) using the default g-priors (Morey & Rouder, 2022)
    widths <- tcrit2 * sqrt(chains[,"sig2"] / n)
    SEPs.anova.HDI[i] <- unname(-50 * diff(colMeans(mu.chains)) / mean(widths))
  }
  
  report <- data.frame("BF" = BFs,
                       "SEP.LM.CI" = SEPs.LM.CI,
                       "SEP.NKM.HDI" = SEPs.NKM.HDI,
                       "SEP.LH.HDI" = SEPs.LH.HDI,
                       "SEP.anova.HDI" = SEPs.anova.HDI,
                       "SEP.JZS.HDI" = SEPs.JZS.HDI,
                       "SEP.HPDI" = SEPs.HPDI,
                       "BFerror" = BFerrors)
  repid <- paste("report wi_",rindex,".csv",sep="")
  write.csv(report, repid, row.names = FALSE)
}

