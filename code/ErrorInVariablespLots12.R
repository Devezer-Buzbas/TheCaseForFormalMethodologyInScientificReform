# count_all matrices:
# column 1: beta_1=2
# column 1: beta_1=5
# column 1: beta_1=10
# column 1: beta_1=20
#---------------------------------------------------------

# load("~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/ErrorInVars.n100.RData")
# countall_n100 = countall[1:100,]

load("~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/ErrorInVars.n50.RData")
countall_n50 = countall[1:100,]

load("~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/ErrorInVars.n500.RData")
countall_n500 =countall[1:100,]

#load("~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/ErrorInVars.n10.RData")
#countall_n10 =countall[1:100,]

sigma_eta=sigma_eta[1:100]


plot(sigma_eta,countall_n500[,1] ,'l', col="black",
       main="Reproducibility rate of a true result under measurement error model",
       ylab="Estimated reproducibility rate  of true result", 
       xlab="Ratio of measurement error variance  to sampling error variance",
     ylim=c(0,1))
legend(0.65,0.98,cex=0.8, 
       legend=c("small effect, small sample size",
                "large effect, small sample size",
                "small effect, large sample size", 
                "large effect, large sample size"), 
       lty=c(2,2,1,1), lwd=1, col=c("black","magenta","black", "magenta"), box.lty=0)
#lines(sigma_eta,countall_n500[,2],lty=1,  col="orangered") 
#lines(sigma_eta,countall_n500[,3], lty=1,col="cyan")
lines(sigma_eta,countall_n500[,4], lty=1,col="magenta")
#
# lines(sigma_eta,countall_n100[,1],lty=2, col="black")
#lines(sigma_eta,countall_n100[,2],lty=2, col="orangered")
#lines(sigma_eta,countall_n100[,3], lty=2, col="cyan")
#lines(sigma_eta,countall_n100[,4], lty=2, col="magenta")
#
lines(sigma_eta,countall_n50[,1],lty=3, col="black")
#lines(sigma_eta,countall_n10[,2],lty=3, col="orangered")
#lines(sigma_eta,countall_n10[,3], lty=3, col="cyan")
lines(sigma_eta,countall_n50[,4], lty=3, col="magenta")

#--------------------------------------------------------------
#--------------------------------------------------------------
#--------------------------------------------------------------

# Example plot in 1.2 

n = 50  # sample size
xtrue = runif(n, 0, 10) # some random values for the predictor

beta0 = 2 # true intercept
beta1 = 20 # true regression coefficient of the predictor variable:

sigma_eps = 1 # error variance in classical linear regression
sigma_eta = 1 # error variance of predictor in error-in-variables model

eps = rnorm(n, 0, sigma_eps)  # generate random values
eta = rnorm(n, 0, sigma_eta)  # generate random values
x = xtrue+eta  # incorporate error-in-variables
y = beta0 + beta1*xtrue + eps # generate response under correct (true) model
reg.wrong = summary(lm(y ~ x)) # fit under wrong model assuming no error-in-variables
reg.right = summary(lm(y ~ xtrue)) # fit under correct model assuming error-in-variables

# Plots
plot(x,y,pch =20,
     main="An example of error-in-variables model",
     ylab="Response variable", 
     xlab="Predictor variable") # plot the data
# points(xtrue,y,col='red')

abline(reg.wrong$coefficients[[1]],reg.wrong$coefficients[[2]], 'l', col="magenta") # plot the fitted wrong model
abline(reg.right$coefficients[[1]],reg.right$coefficients[[2]], 'l', col="blue") # plot the fitted right model

