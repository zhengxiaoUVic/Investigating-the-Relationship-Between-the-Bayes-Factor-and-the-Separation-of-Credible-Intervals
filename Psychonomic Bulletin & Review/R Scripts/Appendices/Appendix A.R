setwd("~/Downloads/Psychonomic Bulletin & Review") #YOUR PATH

coefs <- matrix(NaN,nrow=2,ncol=3,dimnames=list(c("Intercept","Slope"),c("n=24","n=48","n=100")))
j <- 1; n <- c(24, 48, 100)
for (i in n) {
  filepath <- paste("Datasets/Between-Subjects Designs (4)/btw.sim_null/report_",i,".csv",sep="")
  replication <- read.csv(filepath)
  df <- data.frame("x"=abs(replication$SEP.CI)/100,"y"=log(replication$BF))
  result <- summary(lm(y~I(x^2), data=df))
  coefs[,j] <- result$coefficients[,1]
  j <- j + 1
}
coefs
#                n=24      n=48     n=100
# Intercept -1.237818 -1.533965 -1.869476
# Slope      3.181450  3.425969  3.607424

x <- 0.5
A <- gamma(1.5+x)/(gamma(1+x)*sqrt(n))
B <- qnorm(1-.05/2)^2

library(rlang)
png("~/Desktop/Figure A1.png",width=2000,height=900,units="px",res=300)
par(mfrow=c(1,3),oma=c(2,2,0,0))
for (i in 1:3) {
  curve(coefs[1,i]+coefs[2,i]*x,0,2.25,xlab="",ylab="",main=expr(paste(italic("n="),!!n[i])),ylim=c(-2.3,6))
  curve(log(A[i])+B*x,0,2.25,add=T,col="red",lty=2)
}
mtext(expression("Between-Subjects CI"~italic("Sep")^2),side=1,line=-1.5,outer=T)
mtext(expression("ln("~italic("BF")[10]~")"),side=2,line=-1,outer=T)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = 'l', bty = 'n', xaxt = 'n', yaxt = 'n')
legend("bottom", legend=c("Fitted","Analytic"), col=c("black","red"), lty=c(1,2), bty="n", lwd=2, horiz=T)

dev.off()

