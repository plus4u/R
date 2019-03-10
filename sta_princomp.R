## pca analysis : using princomp 

# The function princomp() uses the spectral decomposition approach.
# The functions prcomp() and PCA()[FactoMineR] use the singular value decomposition (SVD).
# According to R help, SVD has slightly better numerical accuracy. Therefore, prcomp() is the preferred function.
# The simplified format of these 2 functions are :   prcomp(x, scale = FALSE)
# princomp(x, cor = FALSE, scores = TRUE)

# Arguments for prcomp() :     x : a numeric matrix or data frame
# scale : a logical value indicating whether the variables should be scaled to have unit variance before the analysis takes place

# Arguments for princomp() :  x : a numeric matrix or data frame
# cor : a logical value. If TRUE, the data will be centered and scaled before the analysis
# scores : a logical value. If TRUE, the coordinates on each principal component are calculated

library(psych)
library(GPArotation)
library(readxl)



x <- 1:10
y <- x + runif(10, min=-.5, max=.5)
z <- x + y + runif(10, min=-10, max=.10)

data <- data.frame(x, y, z)

data

# 1. 


pr <- princomp(data)

pr

summary(pr)


# 2. 

pr$scores[, 1:2]

str(pr)


# 3. check the values of $scores



# 4. screeplot

screeplot(pr)



## another case

## Use cor=FALSE to base the principal components on the covariance matrix. Use the covmat= option to enter a correlation or covariance matrix directly. If entering a covariance matrix, include the option n.obs=.

fit <- princomp(data, cor=TRUE)
summary(fit) # print variance accounted for 
loadings(fit) # pc loadings 
plot(fit,type="lines") # scree plot 
fit$scores # the principal components
biplot(fit)




## compare principal

pcaFa.prin <- princomp(pcaFa, cor = T)

summary(pcaFa.prin)

str(pcaFa.prin)

pcaFa.prin$scores[, 1:6]

plot(pcaFa.prin, type="lines") ## different from spss , the same using cor = T

