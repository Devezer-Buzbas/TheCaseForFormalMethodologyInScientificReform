library(evd)
file.names = c("~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/DataPeekingPart1Simulation_TrueModel_Normal_Distribution.RData",
               "~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/DataPeekingPart1Simulation_TrueModel_Cauchy_Distribution.RData",
               "~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/DataPeekingPart1Simulation_TrueModel_T_Distribution.RData",
               "~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/DataPeekingPart1Simulation_TrueModel_Gumbel_Distribution.RData")
dist.names = c("normal", "cauchy", "t", "gumbel")
#---------------------------------
n = 7 # sample size
df =10 # for data generating mechanism t distribution
mux = 0 # population mean  x
muy = 0 # population mean y
sdx = 1 # population standard deviation x
sdy = 1 # population standard deviation y
#
# Simulation parameters
M = 1e6 # number of simulations
#
# permutation distribution
nperms = choose(n+n,n)


for(k in 1:length(file.names)){
perms.y = matrix(NA, nrow = n, ncol = nperms)
pvalue.perms.uncond = rep(NA,M)
pvalue.perms.cond = rep(NA,M)
pvalue.perms.uncond.incorrect = rep(NA,M)
if(dist.names[k]=="normal"){
  x = matrix(rnorm(n*M,mux,sdx),nrow=n,ncol=M)
  y = matrix(rnorm(n*M,mux,sdx),nrow=n,ncol=M)
}
if(dist.names[k]=="cauchy"){
  x = matrix(rcauchy(n*M,mux,sdx),nrow=n,ncol=M)
  y = matrix(rcauchy(n*M,mux,sdx),nrow=n,ncol=M)
}
if(dist.names[k]=="t"){
  x = matrix(rt(n*M,df),nrow=n,ncol=M)
  y = matrix(rt(n*M,df),nrow=n,ncol=M)
}
if(dist.names[k]=="gumbel"){
  x = matrix(rgumbel(n*M,mux,sdx),nrow=n,ncol=M)
  y = matrix(rgumbel(n*M,mux,sdx),nrow=n,ncol=M)
}
#-----------------------------------------------------
# Parametric two sample z-tests under model mispecification 
sdpool = sqrt((sdx^2)/n + (sdy^2)/n) 
z = (colMeans(x)-colMeans(y))/sdpool
pvalue.model.misspec=pnorm(z,lower.tail=FALSE)
#-----------------------------------------------------
# Permutation tests under no model mispecification
# but unconditional, conditional, and inccorect unconditional
for(i in 1:M){
  xx = x[,i]
  yy = y[,i]
  obs.ts = sum(xx)
  w = c(xx,yy)
  perms.x = combn(w,n)
  for (j in 1:nperms){perms.y[,j] = w[-match(perms.x[,j],w)]}
  # unconditional distribution on 
  # deriving hypotheses from data
  perms.ts.uncond = colSums(perms.x)
  pvalue.perms.uncond[i] = sum(perms.ts.uncond>=obs.ts)/nperms
  #
  # conditional on deriving hypotheses from the data
  colMeans.x = colMeans(perms.x)  
  colMeans.y = colMeans(perms.y)
  if(mean(xx)>mean(yy)){
  perms.ts.cond = perms.ts.uncond[colMeans.x > colMeans.y]
  pvalue.perms.cond[i] = sum(perms.ts.cond>=obs.ts)/length(perms.ts.cond)
  pvalue.perms.uncond.incorrect[i] = sum(perms.ts.uncond>=obs.ts)/nperms
  }else{
    pvalue.perms.cond[i]=NA
    pvalue.perms.uncond.incorrect[i]=NA
  }
}  
save.image(file = file.names[k])
}
# end DataPeekingPart1Simulation
#--------------------------------------------- 