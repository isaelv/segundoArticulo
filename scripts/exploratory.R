##Required packages
library(lattice)

##Setting WD
setwd("~/Dropbox/IVSDoctorado/segundoArticulo")

##Loading data

groups.prop <- read.table("datos/txt/groupsProportions.txt", header = T, dec = ".")
head(groups.prop)
pairs(groups.prop)

par(mfrow = c(2,2))
coplot(lateSps ~ stblAlgae | factor(semester) + condition, data = groups.prop)
coplot(recLateSps ~ stblAlgae |factor(semester) + condition, data = groups.prop)
coplot(earlierSps ~ factor(semester) | condition, data = groups.prop)
coplot(recEarlierSps ~ factor(semester) | condition, data = groups.prop)

