library(BayesFactor) #version 0.9.12-4.4

mu.b <- 100; sigma.b <- 20; delta.mu <- 4.6; sigma.e <- 6.67; n <- 48
set.seed(77)
TrueScore <- rnorm(n, mu.b, sigma.b)
eij1 <- rnorm(n, 0, sigma.e)
eij2 <- rnorm(n, 0, sigma.e)
datL <- data.frame("Subject" = factor(paste("s", rep(1:n,2), sep="")),
                   "Level" = factor(rep(c("Level1", "Level2"), each = n)),
                   "Response" = c(TrueScore + eij1 + delta.mu,
                                  TrueScore + eij2))
runs <- 500
BF <- rep(NA, runs)
for (i in 1:runs) {
  set.seed(i)
  BF[i] <- anovaBF(Response ~ Level + Subject, data = datL, whichRandom = "Subject", 
                   iterations = 1e5, progress = F)
}
df <- data.frame("BF" = BF)

library(ggplot2)
ggplot(df, aes(x = BF)) + geom_histogram() +
  ggtitle("500 runs of anovaBF()") + theme_classic()

range(BF)
#4.317892 11.710303

head(sort(BF))
#4.317892 4.320595 4.321721 4.325345 4.325833 4.325994

tail(sort(BF))
#4.869353  4.882890  5.132271  5.248468  5.447687 11.710303

