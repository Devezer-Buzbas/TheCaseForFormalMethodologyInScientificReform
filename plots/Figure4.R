# Section 2.2
# Has two parts:
# 
# Part 1: Permutation test (test for the location parameter using the median). 
# showing that a test where all properties are derived from the data
# achieves Type I nominal error rate and hence is valid
# loads data obtained from DataPeekingPart1Simulation.R
# 
# Part 2: Normal, HalfNormal plots with shaded area for Type I error rate 
# two sample z-test example
#
# And produces two plots:
# Plot 1: Normal HalfNormal
# Plot 2: Permutation
#------------------------------------------
library(fdrtool)
#---------------------------------------------------
# Part 1
# Valid Conditional Permutation Test. 
# All properties of the test obtained from the data
# True model generating the data is Cauchy
# No user defined criterion
# A one ssample/simulation version of this
# is used in DataPeeking.R with user defined criterion
# for illustration
#----------------------------------------
nbins = 50 # number of histogram bins
load("~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/DataPeekingPart1Simulation_TrueModel_Normal_Distribution.RData")
hpermsnormal.uncond = hist(pvalue.perms.uncond,nbins, plot=FALSE)
hpermsnormal.cond = hist(pvalue.perms.cond,nbins, plot=FALSE)
hpermsnormal.uncond.incorrect = hist(pvalue.perms.uncond.incorrect,nbins, plot=FALSE)
load("~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/DataPeekingPart1Simulation_TrueModel_T_Distribution.RData")
hpermst.uncond = hist(pvalue.perms.uncond,nbins, plot=FALSE)
hpermst.cond = hist(pvalue.perms.cond,nbins, plot=FALSE)
hpermst.uncond.incorrect = hist(pvalue.perms.uncond.incorrect, nbins, plot=FALSE)
hpvaluet.model.misspec = hist(pvalue.model.misspec,nbins,plot=FALSE)
load("~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/DataPeekingPart1Simulation_TrueModel_Cauchy_Distribution.RData")
hpermscauchy.uncond = hist(pvalue.perms.uncond,nbins, plot=FALSE)
hpermscauchy.cond = hist(pvalue.perms.cond,nbins, plot=FALSE)
hpermscauchy.uncond.incorrect = hist(pvalue.perms.uncond.incorrect,nbins, plot=FALSE)
hpvaluecauchy.model.misspec = hist(pvalue.model.misspec,nbins,plot=FALSE)
load("~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/DataPeekingPart1Simulation_TrueModel_Gumbel_Distribution.RData")
hpermsgumbel.uncond = hist(pvalue.perms.uncond,nbins, plot=FALSE)
hpermsgumbel.cond = hist(pvalue.perms.cond,nbins, plot=FALSE)
hpermsgumbel.uncond.incorrect = hist(pvalue.perms.uncond.incorrect,nbins, plot=FALSE)
hpvaluegumbel.model.misspec = hist(pvalue.model.misspec,nbins,plot=FALSE)
# 
# Part 1 end
#-----------------------------------------------------
# Part 2
# Normal, HalfNormal plots with shaded area for 
# Type I error rate  two sample z-test example
#----------------------------------------
x = seq(-3,3,0.01)
#--------------------------------------------------------
# Plot NormalHalfNormal
# plot size is 6 x 4, portrait
#--------------------------------------------------------
plot(x,dhalfnorm(x),'l', 
     main='Distribution of the test statistic under the null hypothesis 
     in a conditional and unconditional two sample z-test', 
     xlab = 'test statistic', ylab='density', xaxt='n', col=rgb(0.9,0.38,0), cex.main=0.9)
lines(x, dnorm(x),'l')
axis(side = 1, at = c(0), labels="0")
axis(side = 1, at = c(1.64), labels="z")
axis(side = 1, at = c(1.96), labels="z*", col=rgb(0.9,0.38,0))

# Add shaded areas
xx = seq(1.64,3,0.01)
xxhn=seq(1.96,3,0.01)
xxnhn = seq(1.64,1.96,0.01)

cord.x <- c(1.64,xx,3) 
cord.yn <- c(0,dnorm(xx),0) 

cord.xhn <- c(1.96,xxhn,3) 
cord.yhn <- c(0,dhalfnorm(xxhn),0) 

cord.xnhn <- c(1.64,xxnhn,1.96) 
cord.ynhn <- c(0,dhalfnorm(xxnhn),0) 

polygon(cord.x,cord.yn,col=rgb(0, 0, 0, 0.9), border=NA) 
# for critical value z and normal (common z test valid)
polygon(cord.xhn,cord.yhn,col=rgb(0.9,0.38,0,0.5), border=NA) 
# for critical value z* and halfnormal (conditional analysis valid)
polygon(cord.xnhn,cord.ynhn,col=rgb(0,0,0, 0.5), border=NA) # for 
# for critical value z and half normal 
# (unconditional analysis which was meant to be conditional, invalid)
#----------------------------------------------------------

# Permutation plot

plot(hpermsnormal.cond$mids,hpermsnormal.cond$density, 'l', col='darkviolet',
     xlim=c(0,1), ylim=c(0,2.3),
     main="Distribution of p-value for test of location parameter under valid and invalid tests",
     xlab = "p-value",
     ylab = "density",
     cex.main=0.9)
lines(hpermsnormal.uncond.incorrect$mids,hpermsnormal.uncond.incorrect$density, col='darkviolet', lty=2)

lines(hpermst.cond$mids,hpermst.cond$density, 'l', col= 'blue')
lines(hpermst.uncond.incorrect$mids,hpermst.uncond.incorrect$density, 
      lty=2, col= 'blue')
lines(hpvaluet.model.misspec$mids,hpvaluet.model.misspec$density, 
      lty=3, col= 'blue')

lines(hpermscauchy.cond$mids,hpermscauchy.cond$density, 'l', col='darkviolet')
lines(hpermscauchy.uncond.incorrect$mids,hpermscauchy.uncond.incorrect$density, 
      lty=2, col='darkviolet')
lines(hpvaluecauchy.model.misspec$mids,hpvaluecauchy.model.misspec$density, 
      lty=3, col='darkviolet')

lines(hpermsgumbel.cond$mids,hpermsgumbel.cond$density, 'l', col='darkolivegreen')
lines(hpermsgumbel.uncond.incorrect$mids,hpermsgumbel.uncond.incorrect$density, 
      lty=2, col='darkolivegreen')
lines(hpvaluegumbel.model.misspec$mids,hpvaluegumbel.model.misspec$density, 
      lty=3, col='darkolivegreen')

legend(0.65, 2.4, legend=c("Normal", "T", "Cauchy", "Gumbel",
                          "Group 1","Group 2", "Group 3"),
       col=c("darkviolet","blue", "darkviolet", "darkolivegreen",
             "black", "black","black"), lty= c(NA,NA,NA,NA,1,2,3), cex=0.8,
       box.lty = 0, bg='transparent', 
       y.intersp = 0.45, pch=c(19,19,19,19,NA,NA,NA))

#------------------------------------------------

# end
#------------------------------------------------

