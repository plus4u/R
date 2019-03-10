### 1. princomp   2. Pricipal    2. prcomp   3. factanal  4. factanal + oblimin : error
###  5. factor.pa( rotation="oblimin") error  6. library(nFactors) 
### 7.  8. fa(r = cor(dat), nfactors = 5, rotate = "oblimin", fm = "pa")

# Pricipal Components Analysis
# entering raw data and extracting PCs 
# from the correlation matrix 


# 1. princomp
# The princomp( ) function produces an unrotated principal component analysis.

dat <- pca_xls[,2:16]

fit <- princomp(dat, cor=TRUE)

summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 

fit$scores # the principal components
biplot(fit)


# 2. The principal( ) function in the psych package can be used to extract and rotate principal components.

# Varimax Rotated Principal Components
# retaining 5 components 

library(psych)

fit <- principal(dat, nfactors=5, rotate="varimax")

fit # print results

# mydata can be a raw data matrix or a covariance matrix. Pairwise deletion of missing data is used. 
# rotate can "none", "varimax", "quatimax", "promax", "oblimin", "simplimax", or "cluster"


# 3. Exploratory Factor Analysis
# The factanal( ) function produces maximum likelihood factor analysis.

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors, 
# with varimax rotation 

fit <- factanal(dat, 3, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)

# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(mydata),cex=.7) # add variable names

# 4. error ?? oblimin

factanal(dat, 5, rotation="oblimin")


# The rotation= options include "varimax", "promax", and "none".
# Add the option scores="regression" or "Bartlett" to produce factor scores. Use the covmat= option to 
# enter a correlation or covariance matrix directly. If entering a covariance matrix, 
# include the option n.obs=.


# The factor.pa( ) function in the psych package offers a number of factor analysis related functions, 
# including principal axis factoring.

# 5. 

# Principal Axis Factor Analysis
library(psych)

fit <- factor.pa(dat, nfactors=5, rotation="oblimin")  # error 
fit # print results


# 6. Determining the Number of Factors to Extract

# Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(dat)) # get eigenvalues
ap <- parallel(subject=nrow(dat),var=ncol(dat),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)


# 7. Package stats has a function factanal() can be used to perform factor analysis:


n.factors <- 5   

fit <- factanal(dat, 
                n.factors,                # number of factors to extract
                scores=c("regression"),
                rotation="oblimin")  # error 

print(fit, digits=2, cutoff=.3, sort=TRUE)



# 8. 

# install.packages("psych")
# install.packages("GPArotation")
library(psych)
library(GPArotation)

fit <- fa(r = cor(dat), nfactors = 5, rotate = "oblimin", fm = "pa")
str(fit)
fit$loadings


## Loading required namespace: GPArotation
## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate =
## rotate, : I am sorry, to do these rotations requires the GPArotation
## package to be installed

plot(fit,labels=names(dat),cex=.7, ylim=c(-.1,1)) 




