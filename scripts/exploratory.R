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
library(RColorBrewer)

cols <- brewer.pal(n=2, name = "Set1")
cols1 <- cols[groups.prop$condition]


plot(x = groups.prop$lateSps, y = groups.prop$recLateSps, col = cols1, pch = 6, xlab = "Cover proportion of adults", ylab = "Cover proportion of recruits", xlim = c(0, 1))

points(x = groups.prop$earlierSps, y = groups.prop$recEarlierSps, pch = 19, col = cols1)

legend("topright", title = "Grupo funcional", legend = c("Tardias", "Pioneras"), pch = c(6,19))

legend("bottomright", legend = c("Impactado","No impactado"), text.col = rep(cols), bg = "transparent")

lines(groups.prop$earlierSps, predict(earlier.b.reg))
lines(groups.prop$lateSps, predict(late.b.reg))
