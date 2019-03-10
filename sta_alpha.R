### reliability

# 1. data 

Q <- data.frame(
    Q1=c(1,4,2,3,4,2,3,4,3,2), 
    Q2=c(2,4,1,2,4,1,2,5,2,1), 
    Q3=c(2,5,1,3,3,2,3,4,2,2))

pairs(Q, panel=panel.smooth)

# 2. cronbach : install.packages("psy")  /  alpha ()

library(psy)

cronbach(Q)

library(psych)

a <- alpha(Q)
str(a)

a$total

View(a$scores)
