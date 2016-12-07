##Required packages
library(lattice)

##Setting WD
setwd("~/Dropbox/IVSDoctorado/segundoArticulo")

##Loading data

groups.prop <- read.table("datos/txt/groupsProportions.txt", header = T, dec = ".")
head(groups.prop)
pairs(groups.prop)

####Beta regression
##Transformation function since beta has open interval (0,1). From betareg vignette

y.transf.betareg <- function(y){
  n.obs <- sum(!is.na(y))
  (y*(n.obs -1) + 0.5)/n.obs
}

late.b.reg <- betareg(y.transf.betareg(recLateSps) ~ lateSps + condition, data = groups.prop)
earlier.b.reg <- betareg(y.transf.betareg(recEarlierSps) ~ earlierSps + condition, data = groups.prop)
summary(late.b.reg)
summary(earlier.b.reg)

##Plotting

plot(recLateSps ~ lateSps, data = groups.prop, col = groups.prop$condition, pch = 3)
points(recEarlierSps ~ earlierSps, data = groups.prop, pch = 2, col = groups.prop$condition)


