## pca analysis

# install.packages("GPArotation")

install.packages("psych")

library(psych)
library(GPArotation)
library(readxl)

rm(xx.factor)

principal(r, nfactors = 1, residuals = FALSE,rotate="varimax",n.obs=NA, covar=FALSE,
          scores=TRUE,missing=FALSE,impute="median",oblique.scores=TRUE,method="regression",...)


# pcaFa <- read_excel("work_r/source/pcaFa_data.xls")

pcaFa <- read_excel("work_r/source/pcaFa_data.xls", col_names = TRUE)

str(pcaFa)


# principal(r, nfactors = 1, residuals = FALSE,rotate="varimax",n.obs=NA, covar=FALSE, scores=TRUE,missing=FALSE,impute="median",oblique.scores=TRUE,method="regression",...)


## 1. principal   2. plot # scree chart

pcaFa.pca <- principal(pcaFa, nfactors = 5, residuals = FALSE,rotate="varimax",n.obs=NA, covar=FALSE, scores=TRUE, missing=FALSE,method="regression")

pcaFa.pca2 <- principal(pcaFa, nfactors = 5, residuals = FALSE,rotate="oblimin",n.obs=NA, covar=FALSE, scores=TRUE, missing=FALSE,method="regression")

pcaFa.pca

pcaFa.pca2


# plotting

plot(pcaFa.pca$values, type = "b")
plot(pcaFa.pca2$values, type = "b")


# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 


## check

fit <- princomp(pcaFa.pca, cor=TRUE)

summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)

 


## princomp  vs  prcomp 

# 1. 다중 공선성 확인  multicollinearity check

cor(iris[1:4]) # some are 

# data is biased to a side


log.ir <- log(iris[,1:4])

ir.species <- iris[,5]

# principal component analysis
ir.pca <- princomp(log.ir, center = T, scale. = T) 

print(ir.pca) 



# different when both using covariance matrix. When scaling (normalizing) the training data, prcomp uses n−1 as denominator but princomp uses n as its denominator. Difference of these two denominators is explained in this tutorial on principal component analysis.

pc.cr<-prcomp(log.ir, scale=TRUE, cor=TRUE, scores=F)
pc.cr1<-princomp(log.ir, cor=F, scores=TRUE)

plot(pc.cr,type="lines") 
