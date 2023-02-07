n = 50
xstar = runif(n, 0, 10)
beta0 = 2
# True effect:
beta1 = c(20)

# Original study estimate
betafalse = seq(from=0,to=19,length.out=100)

M = 1000
sigma_eps = 1
sigma_eta = seq(from=sigma_eps*0.1,to=sigma_eps*10,length.out=100)

alpha=0.05
coef_lb =qnorm(alpha/2) 
coef_ub = (-1)*coef_lb

countall = matrix(NA, nrow=length(sigma_eta), ncol=length(betafalse))

for(k in 1:length(betafalse)){
for(j in 1:length(sigma_eta)){
  count = 0
 for(i in 1:M){
 eps = rnorm(n, 0, sigma_eps)
 eta = rnorm(n, 0, sigma_eta[j])
 x = xstar+eta
 y = beta0 + beta1*xstar + eps
 reg = summary(lm(y ~ x))
 interval_beta1hat = c(reg$coefficients[[2]] +(coef_lb)*reg$coefficients[[4]], 
                       reg$coefficients[[2]] +(coef_ub)*reg$coefficients[[4]]) 
 if(interval_beta1hat[1]<betafalse[k] & interval_beta1hat[2]>betafalse[k]){
   count = count+1
 }
countall[j,k]=count/M
 }
}
}



reprate = rep(NA,length(betafalse))
sigma_etaplotvec = rep(NA, length(betafalse))

for (k in 1:length(betafalse)){
  reprate[k] =  max(countall[,k])
  ind = which(countall[,k]==reprate[k])
  sigma_etaplotvec[k] = sigma_eta[ind]
}

save(countall, sigma_eta,
     file="~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/ErrorInVars.1.3.n50.RData")


