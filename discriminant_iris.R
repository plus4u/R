### Linear Discriminant Analysis with Jacknifed Prediction 

# Quadratic Discriminant Analysis ( not yet)

# tidyverse for data manipulation and visualization
# MASS for LDA and QDA functions
# klaR for partition plot function

# 1. Data and Required Packages

# install.packages("klaR")
# install.packages("tidyverse")
library(tidyverse)
library(MASS)
library(klaR)
set.seed(101)
sample_n(iris, 10)


# 2. Preparing the Data

training_sample <- sample(c(TRUE, FALSE), nrow(iris), replace = T, prob = c(0.6,0.4))

train <- iris[training_sample, ]
test <- iris[!training_sample, ]

# review
head(training_sample,10)
summary(iris)
str(iris)
summary(training_sample)


# 3. Linear Discriminant Analysis / Applying LDA

lda.iris <- lda(Species ~ ., train)  ## lda
lda.iris #show results

 # 4. plot

plot(lda.iris, col = as.integer(train$Species))


plot(lda.iris, dimen = 1, type = "b")


partimat(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=train, method="lda")



# 5. LDA Predictions

lda.train <- predict(lda.iris) ## prediction with train data
train$lda <- lda.train$class
table(train$lda,train$Species)

str(lda.train)

lda.test <- predict(lda.iris,test) ## prediction with new data (test)
test$lda <- lda.test$class
table(test$lda,test$Species)


## ldahist 
lda.test.values <- predict(lda.iris)
str(lda.test.values) 

ldahist(data = lda.test.values$x[,1], g = lda$lev) ## ldahis ##

lda.test.values$x[,1]

## ldahist(lda.test, g= lda$lev, nbins = 25, h, x0 = - h/1000, type = c("histogram", 
