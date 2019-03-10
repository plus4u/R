### 1. pca analysis : using prcomp w/ iris  2.   
  
library(psych)
library(GPArotation)

library(readxl)

# The function princomp() uses the spectral decomposition approach.

# The functions prcomp() and PCA()[FactoMineR] use the singular value decomposition (SVD).
#  According to R help, SVD has slightly better numerical accuracy. Therefore, prcomp() is the preferred function.

# prcomp(x, scale = FALSE)  vs princomp(x, cor = FALSE, scores = TRUE)
 
# scale : whether the variables should be scaled to have unit variance before the analysis takes place 
# cor : If TRUE, the data will be centered and scaled before the analysis
# scores : a logical value. If TRUE, the coordinates on each principal component are calculated


# 1. multicollinearity check

cor(iris[1:4])

head(iris)
iris[1:4]

# some of them are big as 0.87

# 2. biased data -> transform log 

iris_log <- log(iris[, 1:4])

iris_species <- iris[,5]

str(iris_log)

# 3. pca analysis

# Unlike princomp, prcomp : variances are computed with the usual divisor N - 1.

iris_prc <- prcomp(iris_log, center = T, scale. = T)

# Warning messages:  In doTryCatch(return(expr), name, parentenv, handler) :  플랏 타입 'lines'은 첫번째 문자에서 잘려질 것입니다

print(iris_prc)

# 4. plot scree graph

plot(iris_prc,type="lines")

# 5. summary : check that how much it has a power of explanation Cumulative Proportion 

summary(iris_prc)  ## 

# Cumulative Proportion  0.7331 of PC1,  0.9599 with PC2

# 6. linear regression

prc <- as.matrix(iris_log) %*% iris_prc$rotation

prc

# 7. 

train <- cbind(iris_species,as.data.frame(prc))

head(train)

train[,1] <- as.factor(train[,1])

str(train)

head(train)


colnames(train)[1] <- "label"

str(train)

head(train)

# 8. linear regression with PC1, PC2

fit <- lm(label~PC1+PC2, data=train) 


# 9. performance check 

fit_pred <- predict(fit, newdata = train)

fit_pred

# 10. comparision the both prediction, actual 

b <- round(fit_pred)

str(b)


b[b==1] <- "setosa"

b[b==2] <- "Versicolor"

b[b==3] <- "Virginica"

b

a <- iris_species

table(b,a)


