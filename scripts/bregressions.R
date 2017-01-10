##Setting WD
setwd("~/Dropbox/IVSDoctorado/segundoArticulo")

##Loading data
groups.prop <- read.table("datos/txt/groupsProportions.txt", header = T, dec = ".")

##Required libraries

library(betareg)

##**************************
#****Coral vs nonCoral (m1)
#***************************

##Summing corals vs nonCorals
m1Data <- data.frame(groups.prop[,1:3],
                     coralProp = (groups.prop$lateSps + 
                                    groups.prop$recLateSps +
                                    groups.prop$earlierSps + 
                                    groups.prop$recEarlierSps),
                     nonCoralProp = (groups.prop$nonStbAlgae + 
                                       groups.prop$stblAlgae + 
                                       groups.prop$otherGroups)
                     )
##Checking sums
m1Data$total <- (m1Data$coralProp + m1Data$nonCoralProp)

head(m1Data)


##Model
#Transforming data
y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y*(n.obs -1) + 0.5)/n.obs
}

coralProp1 <- y.transf.betareg(m1Data$coralProp)
nonCoralProp1 <- y.transf.betareg(m1Data$nonCoralProp)

m1 <- betareg(c(coralProp1,nonCoralProp1) ~ condition + square, data = m1Data)


