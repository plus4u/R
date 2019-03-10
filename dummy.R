# dummy variables

x <- c(2, 2, 5, 3, 6, 5, NA)

xf <- factor(x, levels = 2:6)

model.matrix( ~ xf -1)
 

