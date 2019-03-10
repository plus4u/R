## plot


x <- iris[sample(1:nrow(iris), 10, replace=FALSE), ] ## 

plot( x[, c(1,2)], col = x[,1], pch=21, cex=2)
text( x[, c(1,2)], cex=1)

plot( x[, c(1,2)], col = x[,5], pch=21, cex=2)
text( x$Sepal.Length, x$Sepal.Width, row.names(x), cex = 1, pos=4, col="red")

plot( x[, c(1,2)], col = x[,5], pch=21, cex=2)
text( x$Sepal.Length, x$Sepal.Width, adj = x$Sepal.Width, cex = 1, pos=4, col="red")

# Example of labeling points
attach(mtcars)
plot(wt, mpg, main="Milage vs. Car Weight", 
     xlab="Weight", ylab="Mileage", pch=18, col="blue")
text(wt, mpg, row.names(mtcars), cex=0.6, pos=4, col="red")

mtcars



###


library(ggplot2)

ggplot(data=diamonds, mapping=aes(x=cut)) + geom_bar()

ggplot(data=diamonds, mapping=aes(x=clarity)) + geom_bar()

ggplot(data=diamonds, aes(x=cut, fill=cut)) + geom_bar(width=1) + coord_polar(clarity)

ggplot(data=diamonds, aes(x=cut, fill=cut)) + geom_bar(width=1)
