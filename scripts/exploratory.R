##Required packages----
library(lattice)
library(R2jags)
library(lme4)
##Setting WD----
setwd("~/Dropbox/IVSDoctorado/segundoArticulo")

##Loading required functions----

#Online functions
HighsstatLibV6.R <- source(file = "http://www.highstat.com/BGS/GLMGLMM/RCode/HighstatLibV6.R")
MCMCSupportHighstat.R <- source(file = "http://www.highstat.com/BGS/GLMGLMM/RCode/MCMCSupportHighstat.R")


##Loading data----

groups.prop <- read.table("datos/txt/groupsProportions.txt", header = T, dec = ".")
str(groups.prop)
head(groups.prop)

#Data exploration----

groups.prop$lateCoral <- groups.prop$lateSpsProp + groups.prop$lateSpsRecProp
groups.prop$earlierCoral <- groups.prop$earlierSpsProp + groups.prop$earlierSpsRecProp

#Outliers

MyVars <- c("earlierCoral",
            "lateCoral",
            "semester",
            "condition",
            "nonStbAlgaeProp",
            "stblAlgaeProp",
            "otherGroupsProp")

Mydotplot(groups.prop[,MyVars])



#Converting categorical variables to factors
groups.prop$fsemester <- factor(groups.prop$semester)
groups.prop$fcondition <- factor(groups.prop$condition)

#Reviewing collinearity
#otherGroupsProp removed due to high collinearity and the undefined effect on the recruitment stablishment.

corvif(groups.prop[,c("fsemester", "fcondition","stblAlgaeProp","nonStbAlgaeProp", "otherGroupsProp")])


#Determining singletons
table(groups.prop$square)

singletons <- c(3,12,13,14,18,24,30,31,33,37,39,42,50,51,54,55,58,60,66,72,75,80)

#Removing singletons
bGroups <- groups.prop
for(i in singletons){
  bGroups <- bGroups[which(bGroups$square != i),]
}

table(bGroups$square)
bGroups$fsquare <- factor(bGroups$square)

#Standardizing covariates
MyNorm <- function(x){(x - mean(x, na.rm = T)) / sd(x, na.rm = T)}


bGroups$cNonStbAlgaeProp<- MyNorm(bGroups$nonStbAlgaeProp)
bGroups$cNonStbAlgaeProp<- MyNorm(bGroups$nonStbAlgaeProp)
bGroups$cStbAlgaeProp<- MyNorm(bGroups$stblAlgaeProp)
bGroups$cOtherGroupsProp <- MyNorm(bGroups$otherGroupsProp)

#Beta GLMM in JAGS----
#Preraring data
N <- nrow(bGroups)
bGroups$earlierCoralTrans <- (bGroups$earlierCoral * (N - 1) + 0.5) / N
bGroups$lateCoralTrans <- (bGroups$lateCoral * (N - 1) + 0.5) / N

#The covariate matrix (X)
X <- model.matrix(~ fsemester + fcondition + cNonStbAlgaeProp + cStbAlgaeProp + cOtherGroupsProp ,data = bGroups)

#Number of columns in X, regression parameters in the model
K <- ncol(X)

#Numbering the different levels of the mixed factor at each observation
re <- as.numeric(bGroups$fsquare)

#Levels contained in the mixed factor
Nre <- length(unique(bGroups$fsquare))

#Puttint together the response variable, sample size, covariate matrix X and the information of random intercepts
win.data <- list(Y = bGroups$lateCoralTrans,
                 N = N,
                 X = X,
                 K = K,
                 re = re,
                 Nre = Nre)



#JAGS model code for Beta GLMM for lateCoralsTrans

sink("models/lateCoralsBetaGLMM.txt")
cat("
    model{
    #1A. Diffuse normal priors for betas
    for(i in 1:K){beta[i] ~ dnorm(0, 0.0001)}
    
    #1B. Normal priors for random effects
    for(i in 1:Nre) {a[i] ~ dnorm(0, tau)}
    
    #1C. half-Cauchy(25) prior for sigma zoo
    num ~ dnorm(0, 0.0016)
    denom ~ dnorm(0, 1)
    sigma <- abs(num/denom)
    tau <- 1/(sigma * sigma)
    
    #1D. half-Cauchy(25) prior for tau
    numtheta ~ dnorm(0, 0.0016)
    denomtheta ~ dnorm(0, 1)
    theta <- abs(numtheta/denomtheta)
    
    #2. Likelihood
    for(i in 1:N){
    Y[i] ~ dbeta(shape1[i], shape2[i])
    shape1[i] <- theta * pi[i]
    shape2[i] <- theta * (1 - pi[i])
    logit(pi[i]) <- eta[i]
    eta[i] <- inprod(beta[], X[i,]) + a[re[i]]
    
    #Pearson residuals
    ExpY[i] <- pi[i]
    VarY[i] <- pi[i] * (1 - pi[i]) / (theta + 1)
    PRes[i] <- (Y[i] - ExpY[i]) / sqrt(VarY[i])
    
    #3. Discrepancy measures
    YNew[i] ~ dbeta(shape1[i], shape2[i])
    PResNew[i] <- (YNew[i] - ExpY[i]) / sqrt(VarY[i])
    D[i] <- pow(PRes[i], 2)
    DNew[i] <- pow(PResNew[i], 2)
    }
    Fit <- sum(D[1:N])
    FitNew <- sum(DNew[1:N])
    }", fill = T)
sink()

#Init Values

inits <- function(){
  list(
    beta = rnorm(K, 0, 0.1),
    a = rnorm(Nre, 0, 0.1),
    num = rnorm(1, 0, 25),
    denom = rnorm(1, 0, 1),
    numtheta = rnorm(1, 0, 25),
    denomtheta = rnorm(1, 0, 1)
    
  )
}

params <- c("beta", "a", "sigma", "theta", "PRes", "Fit","FitNew")

load.module("glm")

J0 <- jags.parallel(
  data = win.data,
  inits = inits,
  parameters = params,
  model.file = "models/lateCoralsBetaGLMM.txt",
  n.thin = 10,
  n.chains = 3,
  n.cluster = 3,
  n.burnin = 400000,
  n.iter = 500000
)

out <- J0$BUGSoutput
OUT1 <- MyBUGSOutput(out, c(uNames("beta", K), "sigma", "theta"))
print(OUT1, digits = 3)

#Assesing mixing of chains
vars <- c("beta[1]",
          "beta[2]",
          "beta[3]",
          "beta[4]",
          "beta[5]",
          "beta[6]",
          "beta[7]",
          "beta[8]"
          )

MyBUGSChains(J0$BUGSoutput, vars)

options(max.print = 100000)

print(J0, intervals = c(0.025, 0.975), digits = 3)


#Deviance----
out$mean$deviance



####Beta regression----
##Transformation function since beta has open interval (0,1). From betareg vignette

y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y*(n.obs -1) + 0.5)/n.obs
}


#Model description
library(betareg)
late.b.reg <- betareg(y.transf.betareg(recLateSps) ~ lateSps + condition, data = groups.prop)
earlier.b.reg <- betareg(y.transf.betareg(recEarlierSps) ~ earlierSps + condition, data = groups.prop)

summary(late.b.reg)
summary(earlier.b.reg)




##########################Plotting################
##################################################
library(RColorBrewer)

cols <- brewer.pal(n=2, name = "Set1")
cols1 <- cols[groups.prop$condition]

#Untransformed data
plot(x = groups.prop$lateSps, y = groups.prop$recLateSps, col = cols1, pch = 6, xlab = "Cover proportion of adults", ylab = "Cover proportion of recruits", xlim = c(0, 1))

points(x = groups.prop$earlierSps, y = groups.prop$recEarlierSps, pch = 19, col = cols1)

legend("topright", title = "Grupo funcional", legend = c("Tardias", "Pioneras"), pch = c(6,19))

legend("bottomright", legend = c("Impactado","No impactado"), text.col = rep(cols), bg = "transparent")

lines(groups.prop$earlierSps, predict(earlier.b.reg, type = "response"))
lines(groups.prop$lateSps, predict(late.b.reg))

#Transformed data
plot(y.transf.betareg(groups.prop$recLateSps) ~ groups.prop$lateSps, , col = cols1, pch = 6, xlab = "Cover proportion of adults", ylab = "Cover proportion of recruits", xlim = c(0, 1))

points(y.transf.betareg(groups.prop$recEarlierSps) ~ groups.prop$earlierSps, pch = 19, col = cols1)

legend("topright", title = "Grupo funcional", legend = c("Tardias", "Pioneras"), pch = c(6,19))

legend("bottomright", legend = c("Impactado","No impactado"), text.col = rep(cols), bg = "transparent")

lines(groups.prop$earlierSps, predict(earlier.b.reg, type = "response"))
lines(groups.prop$lateSps, predict(late.b.reg))

