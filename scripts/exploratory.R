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
groups.prop$fsquare <- factor(groups.prop$square)

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

#Standardizing covariates
MyNorm <- function(x){(x - mean(x, na.rm = T)) / sd(x, na.rm = T)}


bGroups$cNonStbAlgaeProp<- MyNorm(bGroups$nonStbAlgaeProp)
bGroups$cNonStbAlgaeProp<- MyNorm(bGroups$nonStbAlgaeProp)
bGroups$cStbAlgaeProp<- MyNorm(bGroups$stblAlgaeProp)

#Beta GLMM in JAGS----
#Preraring data
N <- nrow(bGroups)
bGroups$earlierCoralTrans <- (bGroups$earlierCoral * (N - 1) + 0.5) / N
bGroups$lateCoralTrans <- (bGroups$lateCoral * (N - 1) + 0.5) / N

#The covariate matrix (X)
X <- model.matrix(~ fsemester + fcondition + cNonStbAlgaeProp + cStbAlgaeProp ,data = bGroups)

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

