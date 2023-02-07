# Plots of Figure 1 and Figure 2
#----------------------------------------------------------
# Figure 1
#----------------------------------------------------------
#----------------------------------------------------------
# Load and rearrange data for Figure 1.A
#--------------------------------------#----------------------------------------------------------
# count_all matrices:
# column 1: beta_1=2
# column 2: beta_1=5
# column 3: beta_1=10
# column 4: beta_1=20
# rows: sigma_eps/sigma_eta 0.01 to 1.5 by 0.01
#---------------------------------------------------------
load("~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/ErrorInVars.n50.RData")
countall.n50 = countall[1:100,]

load("~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/ErrorInVars.n500.RData")
countall.n500 =countall[1:100,]

sigma.eta=sigma_eta[1:100] 
# take only the first 100 
# up to sigma_eps/sigma_eta=1

#---------------------------------------
# Generate example in Figure 1.B
#--------------------------------------
n = 100  # sample size
xtrue = runif(n, 0, 10) # some random values for the predictor

beta0 = 2 # true intercept
beta1 = 20 # true regression coefficient of the predictor variable:

sigma_eps = 3 # error variance in classical linear regression
sigma_eta = 3 # error variance of predictor in error-in-variables model

eps = rnorm(n, 0, sigma_eps)  # generate random values
eta = rnorm(n, 0, sigma_eta)  # generate random values
x = xtrue+eta  # incorporate error-in-variables
y = beta0 + beta1*xtrue + eps # generate response under correct (true) model
reg.wrong = summary(lm(y ~ x)) # fit under wrong model assuming no error-in-variables
reg.right = summary(lm(y ~ xtrue)) # fit under correct model assuming error-in-variables

#----------------------------------------------------------
# Figure 1A and 1B create
#----------------------------------------------------------
# Create device window for plot
pdf('/home/erkan/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/ErrorInVariables12.pdf',width=13, height=8)
#----------------------------------------
# Figure 1A

par(mfrow=c(1,2))
plot(sigma.eta,countall.n500[,1] ,'l', col="black",
     main="A. Reproducibility rate of a true result \n under measurement error model",
     ylab="Estimated reproducibility rate  of true result", 
     xlab="Measurement error variability / Sampling error variability",
     ylim=c(0,1))
legend(0.5,1,cex=0.8,y.intersp=1,
       legend=c("small effect, small sample size",
                "large effect, small sample size",
                "small effect, large sample size", 
                "large effect, large sample size"), 
       lty=c(2,2,1,1), lwd=1, col=c("black","magenta","black", "magenta"), box.lty=0)
lines(sigma.eta,countall.n500[,4], lty=1,col="magenta")
lines(sigma.eta,countall.n50[,1],lty=3, col="black")
lines(sigma.eta,countall.n50[,4], lty=3, col="magenta")
#--------------------------------------------------------------
# Figure 1B
plot(x,y,pch =20,
     main="B. An example of measurement error model",
     ylab="Response variable", 
     xlab="Predictor variable") # plot the data

abline(reg.wrong$coefficients[[1]],reg.wrong$coefficients[[2]], 'l', col="magenta") # plot the fitted wrong model
abline(reg.right$coefficients[[1]],reg.right$coefficients[[2]], 'l', col="blue") # plot the fitted right model
# warnings in int_abline are ok, just complaning about boundaries
legend(-3,200,cex=0.8,y.intersp=1.3,
       legend=c("estimated line under \n measurement error model",
                "estimated line under \n correct model"), 
       lty=c(1,1), lwd=1, col=c("magenta","blue"), box.lty=0)
dev.off()

#---------------------------------------------------
#--------------------------------------------------
# Figure 2
#--------------------------------------------------
load("~/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/ErrorInVars.1.3.n50.RData")
betafalse = seq(from=0,to=19,length.out=100)
betafalse <- round(betafalse,1)
beta1=20
betadiff=beta1-betafalse
library(corrplot)

colnames(countall)  <- c(sigma_eta[1],rep("",8), sigma_eta[10],rep("",9), sigma_eta[20], rep("",9),
                         sigma_eta[30], rep("",9), sigma_eta[40], rep("",9), sigma_eta[50], rep("",9),
                         sigma_eta[60], rep("",9), sigma_eta[70], rep("",9), sigma_eta[80], rep("",9),
                         sigma_eta[90], rep("",9), sigma_eta[100])


rownames(countall) <- c(betadiff[1],rep("",8), betadiff[10],rep("",9), betadiff[20],rep("",9),
                        betadiff[30], rep("",9), betadiff[40], rep("",9), betadiff[50], rep("",9),
                        betadiff[60], rep("",9), betadiff[70], rep("",9), betadiff[80], rep("",9),
                        betadiff[90], rep("",9), betadiff[100])

# Create device window for plot
pdf('/home/erkan/Desktop/Research/WorkingPapers/Reproducibility/DataDependentAnalysis/ErrorInVariables13.pdf')
#----------------------------------------
plot.new()
pWidth = 105
pHeight =99
plot.window(c(0,pWidth),c(0,pHeight))
title(main="Reproducibility rate of false results under measurement error model",
      xlab="", ylab="", cex=0.8)

corrplot(countall,
         diag = FALSE,
         mar = c(0, 0, 0, 0),
         tl.cex = 1,
         add=TRUE,
         is.cor=FALSE,
         tl.col="black")

text(x=-11, y=50, srt=90, labels = "Distance of false result from true result", cex=1.2)
text(x=50, y=-8, labels = "Measurement error variability /  Sampling error variability", cex=1.2)
text(x=104.5, y=-3, labels = "RR", cex=1.0)

dev.off()

# 


