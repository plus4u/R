# with( ) function applys an expression to a dataset

result <- c(60,61,62,71,72,73)
breed <- c(rep("b1",6))
feed <- c(rep("a1",3),rep("a2",3))

x1 <- c(result,breed, feed)
x1 <- data.frame(result,breed, feed)
x1

head(x1)



with(x1, tapply(result, breed, mean))
with(x1, tapply(result, feed, mean))
