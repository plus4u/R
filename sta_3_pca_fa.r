### 1. prcomp   2. principal  3. factanal 
# https://www.datacamp.com/community/tutorials/pca-analysis-r 

## 1. prcomp 

str(mtcars)
head(mtcars)

mtcars_pca <- mtcars[, c(1:7,10,11)]

head(mtcars_pca)

mtcars_pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE)

summary(mtcars_pca)
 
##


##


