## principal

## data preprocessing

library(dplyr)


id <- c(1:10)

# class <- rep(c("a", "b"), 5)

# class <- rep("a", 10)
class <- rep(1, 10)

math <- runif(n=10, min = 80, max = 100)
sci <-  runif(n=10, min = 70, max = 90)
eng <- runif(n=10, min = 50, max = 60)
kor <- runif(n=10, min = 60, max = 70)
# eng <-  sample(x=c(90, 80, 50), prob=c(.7, .2, .1), size=10, replace=T)


# tem_a <- cbind.data.frame(id, math, sci, eng)  # factor error

tem_a <- cbind.data.frame(class, math, sci, eng, kor) 

class(tem_a)

colSums(tem_a[3])


# tem_b
class <- rep(2, 10)

eng <- runif(n=10, min = 80, max = 100)
kor <-  runif(n=10, min = 70, max = 90)
math <- runif(n=10, min = 50, max = 65)
sci <- runif(n=10, min = 60, max = 70)

tem_b <- cbind.data.frame(class, math, sci, eng, kor) 

tempo <- rbind(tem_a, tem_b)

## 

fit <- factanal(tempo[2:5], 2) # "xtable" %in% rownames(installed.packages())


fit <- prcomp(tempo[2:5])

library(psych)

fit <- principal(tempo[2:5], rotate = "varimax" )

fit  # variables - PC1 ~ n
summary(fit) # cumulative proportion 

"xtable" %in% rownames(installed.packages())  # check package installed - TRUE

installed.packages()

"stats" %in% rownames(installed.packages())
