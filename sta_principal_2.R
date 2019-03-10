# principal {psych}	R Documentation

library(psych)


# Principal components analysis (PCA)
# Does an eigen value decomposition and returns eigen values, loadings, and degree of fit for a specified number of components. 
# Basically it is just doing a principal components analysis (PCA) for n principal components of either a correlation or covariance matrix. 
# Can show the residual correlations as well. The quality of reduction in the squared correlations is reported by comparing residual correlations to original correlations. 
# Unlike princomp, this returns a subset of just the best nfactors. 
# The eigen vectors are rescaled by the sqrt of the eigen values to produce the component loadings more typical in factor analysis.



# Varimax Rotated Principal Components
# retaining 5 components 

pcaFa.principal <- principal(pcaFa, nfactors=5, rotate="varimax")

pcaFa.principal # print results

plot(pcaFa.principal) # not scree plot 

# Determine Number of Factors to Extract
install.packages("nFactors")
library(nFactors)


pcaFa.principal

ev <- eigen(cor(pcaFa.principal)) # get eigenvalues
ap <- parallel(subject=nrow(pcaFa.principal),var=ncol(pcaFa.principal), rep=100,cent=.05)

nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)

plotnScree(nS)




## error

tmp <- principal(mtcars[,c(1:7,10,11)], nfactors = 5, residuals = FALSE,rotate="varimax",n.obs=NA, covar=FALSE, scores=TRUE,missing=FALSE,method="regression")

plot(tmp, type="lines")
