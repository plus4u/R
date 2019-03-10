## # factor analysis
 
library(psych)
library(GPArotation)
library(readxl)

pcaFa <- read_excel("work_r/source/pcaFa_data.xls", col_names = TRUE)

str(pcaFa)


## factanal + oblimin  

# factanal(x, factors, data = NULL, covmat = NULL, n.obs = NA, subset, na.action, start = NULL, scores = c("none", "regression", "Bartlett"), rotation = "varimax", control = NULL, ...)



pcaFa.fac <- factanal(pcaFa, factors = 5, scores = c("none", "regression", "Bartlett"), rotation = "oblimin")

str(pcaFa.fac)

# plotting
# install.packages("psy")
library(psy)

scree.plot(pcaFa.fac$correlation)

scree.plot(pcaFa.fac$loadings)

# below error #

plot(pcaFa.fac,type="lines") # scree plot 

fit$scores # the principal components

biplot(fit)

# ~ # 



## another method

# Determine Number of Factors to Extract

library(nFactors)

ev <- eigen(cor(pcaFa)) # get eigenvalues
ap <- parallel(subject=nrow(pcaFa),var=ncol(pcaFa),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

str(nS)


 
    