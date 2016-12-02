##Required packages
library(lattice)

##Setting WD
setwd("~/Dropbox/IVSDoctorado/segundoArticulo")

##Loading data

groups.prop <- read.table("datos/txt/groupsProportions.txt", header = T, dec = ".")
head(groups.prop)
xyplot(lateSpsProp ~ factor(semester), groups.prop)
