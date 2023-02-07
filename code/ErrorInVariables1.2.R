n = 50
xstar = runif(n, 0, 10)
beta0 = 2
# True effect:
beta1 = c(2, 5, 10, 20)

M = 1000
sigma_eps = 1
sigma_eta = seq(sigma_eps*0.01,sigma_eps*1.5,0.01)

alpha=0.05
coef_lb =qnorm(alpha/2) 
coef_ub = (-1)*coef_lb

countall = matrix(NA, nrow =length(sigma_eta), ncol = length(beta1))
for(k in 1:length(beta1)){
for(j in 1:length(sigma_eta)){
  count = 0
 for(i in 1:M){
 eps = rnorm(n, 0, sigma_eps)
 eta = rnorm(n, 0, sigma_eta[j])
 x = xstar+eta
 y = beta0 + beta1[k]*xstar + eps
 reg = summary(lm(y ~ x))
 interval_beta1hat = c(reg$coefficients[[2]] +(coef_lb)*reg$coefficients[[4]], 
                       reg$coefficients[[2]] +(coef_ub)*reg$coefficients[[4]]) 
 if(interval_beta1hat[1]<beta1[k] & interval_beta1hat[2]>beta1[k]){
   count = count+1
 }
countall[j,k]=count/M
 }
}
}

plot(sigma_eta,countall[,1] ,'l', col="black",
     main="Reproducibility rate of a true result under error-in-variables model",
     ylab="Estimated reproducibility rate of true result", 
     xlab="Ratio of measurement error variance to sampling error variance")
lines(sigma_eta,countall[,2],'l', col="orangered")
lines(sigma_eta,countall[,3], 'l', col="cyan")
lines(sigma_eta,countall[,4], 'l', col="magenta")

save(countall, sigma_eta,
file="~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/ErrorInVars.NoEffect.n50.RData")