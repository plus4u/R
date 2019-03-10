## factor analysis - find number of factor > 1 eigenvalue 
 

library(psych)
library(GPArotation)
library(readxl)
 

# pcaFa <- read_excel("work_r/source/pcaFa_data.xls", col_names = TRUE))
 

pcaFa.pca2 <- principal(pcaFa, nfactors = 5, residuals = FALSE,rotate="oblimin",n.obs=NA, covar=FALSE, scores=TRUE, missing=FALSE,method="regression")


pcaFa.pca2


# eigenvalue

str(pcaFa)
head(pcaFa)

temp <- cor(pcaFa[,2:16])
temp

temp.eig <- eigen(temp)

scree.plot(temp.eig$vectors)

# 

temp.cov  <- cov(pcaFa[,2:16])
temp.cov.eig <- eigen(temp.cov)
temp.cov.eig


head(temp.cov.eig,10)

str(temp.cov.eig)

scree.plot(temp.cov.eig$vectors)

## Where p is the number of variables. 
## The quick and dirty loop below finds the proportion of the total for each eigenvalue and the cumulative proportion.


cum.pro <- 0
prop <- c()
cum <- c()

for (i in temp.eig) {
    proportion <- i / dim(pcaFa[,2:16])[2]
    cum.pro <- cum.pro + proportion
    
    prop <- append(prop, proportion)
    cum <- append(cum, cum.pro)
}
data.frame(cbind(prop, cum))
