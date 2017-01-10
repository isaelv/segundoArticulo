myDotPlot = function(DataSelected){

P <- dotplot(as.matrix(as.matrix(DataSelected)),
          groups=FALSE,
          strip = strip.custom(bg = 'white',
                               par.strip.text = list(cex = 1.2)),
          scales = list(x = list(relation = "free", draw = TRUE),
                        y = list(relation = "free", draw = FALSE)),
          col=1, cex  = 0.5, pch = 16,
          xlab = list(label = "Value of the variable", cex = 1.5),
          ylab = list(label = "Order of the data from text file", cex = 1.5))
  
print(P)  
}

corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  #correlation part
  #cat("Correlations of the variables\n\n")
  #tmp_cor <- cor(dataz,use="complete.obs")
  #print(tmp_cor)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}
