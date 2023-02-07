# Model for the example: Nor(mu,mu), mu>0
library(HistogramTools)

mu0 = 1 # under Ho
mu = seq(1,50,0.01) # under Ha
lmu= length(mu)

M = 10^5
alpha = 0.05

x = rnorm(M,mu0,mu0)
u = abs(x)*tanh(abs(x))
histu = hist(u,1000, plot=FALSE)
critical.val.u = ApproxQuantile(histu, 1-alpha)

pow.u = rep(NA,lmu) # power

for (i in 1:lmu){
  x = rnorm(M,mu[i],mu[i])
  u = abs(x)*tanh(abs(x))
  pow.u[i] = sum(u>=critical.val.u)/M  
}

# Power for x
critical.val.x = qnorm(1-alpha,mu0,mu0)
pow.x = rep(NA,lmu)
for( i in 1:lmu){
  pow.x[i] = pnorm(critical.val.x, mu[i], mu[i], lower.tail =FALSE)
}

par(pty="s")
rbPal <- colorRampPalette(c('black','orangered'))

#This adds a column of color values
# based on the y values
mucol <- rbPal(10)[as.numeric(cut(pow.u,breaks = 10))]

par(pty = "s")
plot(pow.x,pow.u, col=mucol, pch=20,
     xlim = c(min(pow.x,pow.u),1), ylim = c(min(pow.x,pow.u),1),
     main='Rao-Blackwellization and Power',
     xlab = 'Power of single observation \n as the test statistic',
     ylab = 'Power of single observation \n as the test statistic conditional \n on its absolute value')
abline(0,1)
