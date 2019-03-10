### practice

getwd()

setwd("C:/Users/beomc/working")

rm(list=ls())

## anova

fit <- aov(Petal.Length~Species, data=iris)

str(fit)

fit$coefficients

# check the result

summary(fit)

# post-hoc 

# install.packages("agricolae")

 library(agricolae)

 duncan.test(fit, "Species", alpha = 0.05, console = TRUE)
 a <- duncan.test(fit, "Species", alpha = 0.05)
a
 
 #
 
 comparison <- LSD.test(fit, "Species", p.adj="bonferroni", group=F)
 comparison
 
 
 
 ##
 
 
 ##
 
 
 
 
 
 
 