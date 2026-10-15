library(BayesFactor) #version 0.9.12-4.7
library(rstan) #version 2.32.5, macOS (Intel chip)

stan_code <- "
data {
  int<lower=1> N;        // number of subjects
  int<lower=2> C;        // number of conditions
  // vector[C] Y[N];        // responses [removed features]
  array[N] vector[C] Y;  // responses [Stan 2.26+ syntax for array declarations]
  real tcrit;            // critical value
  matrix[C,C-1] Q;       // projecting C fixed effects into C-1
  real<lower=0> ht;      // (square root) prior scale on the variability of the standardized treatment effects
  real<lower=0> hb;      // (square root) prior scale on the variability of the standardized subject-specific random effects
}

parameters {
  real mu;              // grand mean
  real<lower=0> sigma;  // standard deviation of the error
  real<lower=0> gt;     // variance of the standardized treatment effects
  real<lower=0> gb;     // variance of the standardized subject-specific random effects
  vector[C] t;          // treatment effects
  vector[N] b;          // subject-specific random effects
}

transformed parameters {
  real<lower=0> eta;    // sd of the treatment effects
  real<lower=0> tau;    // sd of the subject-specific random effects
  eta = sigma * sqrt(gt);
  tau = sigma * sqrt(gb);
}

model {
  // linear mixed-effects model
  for (i in 1:N) {
    Y[i] ~ normal(mu + t + b[i], sigma);
  }
  t ~ normal(0, eta);
  b ~ normal(0, tau);

  // priors
  // mu ~ implicit uniform prior     // Jeffreys prior
  target += -log(sigma);             // Jeffreys prior
  gt ~ scaled_inv_chi_square(1, ht);
  gb ~ scaled_inv_chi_square(1, hb); // Rouder et al. (2012)
}

// compute HDI statistics based on Nathoo, Kilshaw, & Masson (2018) and Heck (2019)
generated quantities {
  real<lower=0> width;  // HDI width
  vector[C] mu_t;       // posterior condition means

  width = tcrit * sigma / sqrt(N);
  mu_t = mu + t;
}"


anova_stan <- stan_model(model_code = stan_code)

mu.b <- 100; sigma.b <- 20; delta.mu <- 4.6; sigma.e <- 6.67; n <- 48
set.seed(77)
TrueScore <- rnorm(n, mu.b, sigma.b)
eij1 <- rnorm(n, 0, sigma.e)
eij2 <- rnorm(n, 0, sigma.e)
datL <- data.frame("Subject" = factor(paste("s", rep(1:n,2), sep="")),
                   "Level" = factor(rep(c("Level1", "Level2"), each = n)),
                   "Response" = c(TrueScore + eij1 + delta.mu,
                                  TrueScore + eij2))
datW <- matrix(datL$Response, ncol=2)

set.seed(277)
BF <- anovaBF(Response ~ Level + Subject, data = datL, whichRandom = "Subject", 
              iterations = 1e5, progress = F)
alpha <- .05
tcrit <- qt(1 - alpha / 2, df = 2 * (n - 1))
datlist <- list("N" = n, "C" = 2, "Y" = datW, "tcrit" = tcrit,
                "Q" = matrix(c(sqrt(2)/2,-sqrt(2)/2),ncol=1), "ht" = 0.5, "hb" = 1)
runs <- 100; warmup <- 2e3; iter <- 2e4

m1_rmHDI <- m2_rmHDI <- width_rmHDI <- 
  m1_anovaBF <- m2_anovaBF <- width_anovaBF <-
  m1_rmHDI.permuted <- m2_rmHDI.permuted <- width_rmHDI.permuted <- numeric(runs)

m1.iter2e3_rmHDI <- m2.iter2e3_rmHDI <- width.iter2e3_rmHDI <- 
  m1.iter2e3_anovaBF <- m2.iter2e3_anovaBF <- width.iter2e3_anovaBF <-
  m1.iter2e3_rmHDI.permuted <- m2.iter2e3_rmHDI.permuted <- width.iter2e3_rmHDI.permuted <- numeric(runs)

for (i in 1:runs) {
  mcmc <- sampling(anova_stan, data = datlist,
                   pars = c("mu_t", "width"), refresh = 0,
                   warmup = warmup, iter = warmup + iter/2, chains = 2, seed = i)
  lists <- As.mcmc.list(mcmc)
  chain1 <- lists[[1]]
  chain2 <- lists[[2]]
  
  set.seed(i)
  chains <- posterior(BF, iterations = iter, progress = F)
  widths <- tcrit * sqrt(chains[,"sig2"] / n)
  mu.1 <- chains[,1] + chains[,2]
  mu.2 <- chains[,1] + chains[,3]
  
  set.seed(i)
  dat <- datW[sample(1:n,n),]
  datlist$Y <- dat
  mcmc.permuted <- sampling(anova_stan, data = datlist,
                            pars = c("mu_t", "width"), refresh = 0,
                            warmup = warmup, iter = warmup + iter/2, chains = 2, seed = 277)
  lists.permuted <- As.mcmc.list(mcmc.permuted)
  chain1.permuted <- lists.permuted[[1]]
  chain2.permuted <- lists.permuted[[2]]
  
  m1_rmHDI[i] <- mean(c(chain1[,1],chain2[,1]))
  m2_rmHDI[i] <- mean(c(chain1[,2],chain2[,2]))
  width_rmHDI[i] <- mean(c(chain1[,3],chain2[,3]))
  m1_anovaBF[i] <- mean(mu.1)
  m2_anovaBF[i] <- mean(mu.2)
  width_anovaBF[i] <- mean(widths)
  m1_rmHDI.permuted[i] <- mean(c(chain1.permuted[,1],chain2.permuted[,1]))
  m2_rmHDI.permuted[i] <- mean(c(chain1.permuted[,2],chain2.permuted[,2]))
  width_rmHDI.permuted[i] <- mean(c(chain1.permuted[,3],chain2.permuted[,3]))
  
  m1.iter2e3_rmHDI[i] <- mean(c(chain1[1:2000,1],chain2[1:2000,1]))
  m2.iter2e3_rmHDI[i] <- mean(c(chain1[1:2000,2],chain2[1:2000,2]))
  width.iter2e3_rmHDI[i] <- mean(c(chain1[1:2000,3],chain2[1:2000,3]))
  m1.iter2e3_anovaBF[i] <- mean(mu.1[1:2000])
  m2.iter2e3_anovaBF[i] <- mean(mu.2[1:2000])
  width.iter2e3_anovaBF[i] <- mean(widths[1:2000])
  m1.iter2e3_rmHDI.permuted[i] <- mean(c(chain1.permuted[1:2000,1],chain2.permuted[1:2000,1]))
  m2.iter2e3_rmHDI.permuted[i] <- mean(c(chain1.permuted[1:2000,2],chain2.permuted[1:2000,2]))
  width.iter2e3_rmHDI.permuted[i] <- mean(c(chain1.permuted[1:2000,3],chain2.permuted[1:2000,3]))
}


BFvalue <- round(extractBF(BF)$bf, 2)
report <- data.frame("m1" = c(m1_rmHDI,m1_anovaBF,m1_rmHDI.permuted),
                     "m2" = c(m2_rmHDI,m2_anovaBF,m2_rmHDI.permuted),
                     "width" = c(width_rmHDI,width_anovaBF,width_rmHDI.permuted),
                     "sep" = c(50 * abs(m1_rmHDI - m2_rmHDI) / width_rmHDI,
                               50 * abs(m1_anovaBF - m2_anovaBF) / width_anovaBF,
                               50 * abs(m1_rmHDI.permuted - m2_rmHDI.permuted) / width_rmHDI.permuted),
                     "package" = factor(rep(c("rmBayes", "BayesFactor", "rmBayes (permuted)"), each=runs)))
write.csv(report,paste0("~/Desktop/combined_iter2e4_BF",BFvalue,".csv"),row.names=F)

report2 <- data.frame("m1" = c(m1.iter2e3_rmHDI,m1.iter2e3_anovaBF,m1.iter2e3_rmHDI.permuted),
                      "m2" = c(m2.iter2e3_rmHDI,m2.iter2e3_anovaBF,m2.iter2e3_rmHDI.permuted),
                      "width" = c(width.iter2e3_rmHDI,width.iter2e3_anovaBF,width.iter2e3_rmHDI.permuted),
                      "sep" = c(50 * abs(m1.iter2e3_rmHDI - m2.iter2e3_rmHDI) / width.iter2e3_rmHDI,
                                50 * abs(m1.iter2e3_anovaBF - m2.iter2e3_anovaBF) / width.iter2e3_anovaBF,
                                50 * abs(m1.iter2e3_rmHDI.permuted - m2.iter2e3_rmHDI.permuted) / width.iter2e3_rmHDI.permuted),
                      "package" = factor(rep(c("rmBayes", "BayesFactor", "rmBayes (permuted)"), each=runs)))
write.csv(report2,paste0("~/Desktop/combined_iter2e3_BF",BFvalue,".csv"),row.names=F)

