# Permutation Test for the Location shift
# When:
# 1. The observed value of the test statistic
# 2. The distribution of the test statistic 
#    under Ho
# 3. The hypotheses Ho, and Ha are 
#    all derived from the observed data
#    and the test is valid statistically in the 
#    sense that it achieves nominal Type I error 
#    probability

# Simulation Parameters
M = 1

# Model Parameters
n = 10
a = 0
b = 4

# Other parameters
alpha = 0.05

pvalue.cond = rep(NA,M)
pvalue.uncond = rep(NA,M)

for(i in 1:M){
# Data
x = runif(n,0,4)
y = runif(n,0,4)

# observed values
sample.loc.x = median(x)
sample.loc.y = median(y)

# observed value of the test statistic
loc.stat.obs = sum(x) 

#-------------------------------------------
# obtain all permutations

nperm = choose(n+n,n) 
# number of permutations


# permutation distribution of the test statistic

w = c(x,y)
perms.x = combn(w,n)
perms.y = matrix(NA,nrow=n,ncol=nperm)

for (j in 1:nperm){perms.y[,j] =w[-match(perms.x[,j],w,nomatch=0)]}

loc.stat.perm.x = colSums(perms.x)
loc.stat.perm.y = colSums(perms.y)

# determine the hypotheses
if(sample.loc.x>sample.loc.y){
  # hypothesize Ha: x is associated wth larger values,
  # we expect to reject for large value of loc.stat.obs
  # Correct analysis that conditions
  loc.stat.perm.cond = loc.stat.perm.x[loc.stat.perm.x>loc.stat.perm.y]
  pvalue.cond[i] = sum(loc.stat.perm.cond>loc.stat.obs)/length(loc.stat.perm.cond)
  # Inorrect analysis that does not condition
  pvalue.uncond[i] = sum(loc.stat.perm.x>loc.stat.obs)/nperm
  }else
    {
  pvalue.cond[i]=NA
  pvalue.uncond[i]=NA
}
# if(sample.loc.x<sample.loc.y){
#   # hypothesize Ha: x is associated wth smaller values,
#   # we expect to reject for small value of loc.stat.obs
#   loc.stat.perm.cond = loc.stat.perm.x[loc.stat.perm.x<loc.stat.perm.y]
#   pvalue.cond = sum(loc.stat.perm.cond<loc.stat.obs)/nperm
#   pvalue.uncond = sum(loc.stat.perm.x<loc.stat.obs)/nperm
# }
}
ntest=sum(!is.na(pvalue.cond))
realized.alpha.cond = sum(pvalue.cond[!is.na(pvalue.cond)]<alpha)/ntest 
realized.alpha.uncond = sum(pvalue.uncond[!is.na(pvalue.uncond)]<alpha)/ntest

realized.alpha.cond
realized.alpha.uncond