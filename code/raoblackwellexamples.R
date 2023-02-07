n = 15
lambda=5
xsupport = c(0:n)
# pmf of unconditional test statistic 
plot(xsupport,dpois(xsupport,lambda), type='h')

# Distribution of xbar is unknown,
# derive by simulation
M = 10^5
x=rep(NA,M)
for(i in 1:M){
  x = rpois(n,lambda)
  xbar[i]= mean(x)
}

# pmf of the conditional test statistic 
lines(xbar,)
     
uncond.values = ppois(c(0:n), lambda)
critical.value.uncond = min(which(((1-uncond.values)<0.05) == TRUE))
alpha.uncond = 1-ppois(critical.value.uncond-1, lambda)


alpha.cond = 1-pbinom(critical.value.uncond-1, sum(x),1/n)

alphaobscond = c()
alphaobsuncond = c()

M = 10^4
countuncond = 0
countcond = 0
for(j in 1:M){
  x = rpois(n,lambda)
    # unconditional test statistic x1
    if(x[1]>=critical.value){
      countuncond = countuncond + 1
    }
    # conditional test statistic x1|sum(x) is binomial with 
    # sum(x) number of trials and probability of success 1/n
    # its ecpectation (rao-blackwell says this is the lowest
    # variance estimator) is sum(x)
    if(mean(x)>=critical.value){
      countcond = countcond + 1
    }
  }  
  countuncond/M
  countcond/M
  