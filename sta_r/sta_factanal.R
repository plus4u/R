## factor analysis : 1. factanal,   2. factor.pa( ) function in the psych package 

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 

## Determining the Number of Factors to Extract : library(psych)- plot   or   plotnScree 

# Determine Number of Factors to Extract
# library(nFactors) 
# ev <- eigen(cor(mydata)) # get eigenvalues
# ap <- parallel(subject=nrow(mydata),var=ncol(mydata), rep=100,cent=.05)
# nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
# plotnScree(nS)

str(pca_xls)

dat <- pca_xls[,2:16]

e <- eigen(cor(dat)) #solving for the eigenvalues and eigenvectors from the correlation matrix
L <- e$values # from manually 

plot(L,main="Scree Plot",ylab="Eigenvalues",xlab="Component number",type='b')
abline(h=1, lty=2)

# 
# rotate can "none", "varimax", "quatimax", "promax", "oblimin", "simplimax", or "cluster"

fit1 <- factanal(dat, 5, rotation="varimax")


# to use oblimin 

install.packages("GPArotation")

library(GPArotation)

library(psych)

## https://www.rdocumentation.org/packages/psych/versions/1.8.10/topics/fa

fit2 <- fa(r = cor(dat), nfactors = 5, rotate = "oblimin", fm = "pa")  # correlation or covariance matrix or

fit2  # spss very similar  
fit1

print(fit2, digits=2, cutoff=.3, sort=TRUE)


## eo 1


# plot factor 1 by factor 2 

load <- fit$loadings[, 1:5] # in case of 2 factor is ok, but 5 ? 
plot(load,type="n") # set up plot 
text(load,labels=names(dat),cex=.7) # add variable names


# Structual Equation Modeling : sem package




# 2.

library(psych)
fit <- factor.pa(mydata, nfactors=3, rotation="varimax")
fit # print results