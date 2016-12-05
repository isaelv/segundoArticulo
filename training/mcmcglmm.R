install.packages("MCMCglmm")
library(MCMCglmm)
##Data collection
data("BTdata")

##Relation matrix A
data("BTped")


prior <- list(R = list(V = diag(2)/3, n = 2),
              G = list(G1 = list(V = diag(2)/3, n = 2),
                       G2 = list(V = diag(2)/3, n = 2)))

m1 <- MCMCglmm(cbind(tarsus, back)~trait:sex + trait:hatchdate - 1,
               random = ~ us(trait):animal + us(trait):fosternest, rcov = ~ us(trait):units,
               prior = prior, family = rep("gaussian", 2), nitt = 60000, burnin = 10000, thin = 25, 
               data = BTdata, pedigree = BTped)


##Entendiendo la verosimilitud
Ndata <- data.frame(y = rnorm(5, mean = 0, sd = sqrt(1)))
Ndata$y

possible.y <- seq(-3,3,0.1)
Probability <-  dnorm(possible.y, mean = 0, sd = sqrt(1))
plot(Probability~possible.y, type = "l")
Probability.y <-  dnorm(Ndata$y, mean = 0, sd = sqrt(1))
points(Probability.y~Ndata$y)
