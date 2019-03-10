### prcomp with iris

cor(iris[1:4]) 

log.ir <- log(iris[,1:4])

log.ir

cor(log.ir[1:4]) 

ir.species <- iris[,5]

ir.pca <-prcomp(log.ir,center = T, scale. = T)

ir.pca$rotation   ## rotation vaue

str(ir.pca)

print(ir.pca)
summary(ir.pca)




plot(ir.pca, type="l")


PRC <- as.matrix(log.ir) %*% ir.pca$rotation

head(PRC) # [ 1:150, 1:4 ]

train <- cbind(ir.species,as.data.frame(PRC))

train[,1] <- as.factor(train[,1])

head(train)

colnames(train)[1] <- "label"
head(train)

fit <-lm(label~PC1+PC2, data=train)

str(fit)

fit_pred <- predict(fit, newdata = train)

fit_pred[106]  # 2.86    # train 106 = virginica pc1=2.199

b <- round(fit_pred)

b[b==0 | b==1] <- "setosa"
b[b==2] <- "Versicolor"
b[b==3] <- "Virginica"

a <- ir.species
table(b,a)



### 
library(ggplot2)

install.packages(ggfortify)
library(ggfortify)

df <- iris[c(1, 2, 3, 4)]

plot(prcomp(df), data = iris, colour = 'Species')



### sample

# From the integers 1:10, draw 5 numbers
sample(x = 1:10, size  = 15, replace = TRUE)

sample(x = c("a", "b", "c"), 
       prob = c(.7, .2, .1),
       size = 10, 
       replace = TRUE)

sample(x = c(1, 2, 3), 
       prob = c(.6, .2, .2),
       size = 10, 
       replace = TRUE)

# 5 samples from a Normal dist with mean = 0, sd = 1
rnorm(n = 5, mean = 0, sd = 1) 

# 5 samples from Uniform dist with bounds at 0 and 1
runif(n = 5, min = 0, max = 1)

##

x1 <- sample(x = 1:50, size  = 50, replace = TRUE)

x2 <- sample(x = 1:20, size  = 50, replace = TRUE)

x3 <- sample(x = 10:70, size  = 50, replace = TRUE)

z1 <- sample(x = 1:50, size  = 50, replace = TRUE)

z2 <- sample(x = 1:20, size  = 50, replace = TRUE)

y1 <- cbind(x1, x2, x3, z1, z2)

y2 <- as.data.frame(c(.7*x1 + .5*x2 + .1*x3))

y2 <- cbind(y1, y2)

names(y2[4]) = "y"   # not work 

colnames(y2) <- c("x1","x2", "x3", "z1", "z2", "y")
y2

# 

fit <- prcomp(y2, scale. = T)

tem1 <- summary(fit) # list of 6

tem1


tem2 <- summary(fit)$importance # num [ 1:3, 1:6 ]

tem2

fit$rotation

screeplot(fit, col = "green", type = "lines", pch = 1)


## Which variables explain which PCA components, and vice versa?

loadings <- eigen(cov(y2))$vectors
explvar <- loadings^2

explvar

getwd()
setwd()




