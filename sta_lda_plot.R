##
install.packages("gridExtra")
install.packages("scales")

library(gridExtra)
library(MASS)
library(ggplot2)
library(scales)

require(MASS)
require(ggplot2)
require(scales)
require(gridExtra)

str(iris)

pca <- prcomp(iris[,-5],
              center = TRUE,
              scale. = TRUE) 

prop.pca = pca$sdev^2/sum(pca$sdev^2)

lda <- lda(Species ~ ., 
           iris, 
           prior = c(1,1,1)/3)


r <- lda(formula = Species ~ ., 
         data = iris, 
         prior = c(1,1,1)/3)


prop.lda = r$svd^2/sum(r$svd^2)

plda <- predict(object = lda,
                newdata = iris)


dataset = data.frame(Species = iris[,"Species"],
                     pca = pca$x, lda = plda$x)


df=data.frame(lda.LD1,lda.LD2)

scatterplot3d(df[, 1:2], pch = 16, grid=FALSE, box=FALSE)

p1 <- ggplot(dataset) + geom_point(aes(lda.LD1, lda.LD2,colour = Y, shape = Y), size = 2.5) + 
    labs(x = paste("LD1 (", percent(prop.lda[1]), ")", sep=""),
         y = paste("LD2 (", percent(prop.pca[2]), ")", sep=""))

p2=ggplot(dataset) + geom_point(aes(pca.PC1, pca.PC2, colour = Y, shape = Y), size = 2.5) +
    labs(x = paste("PC1 (", percent(prop.pca[1]), ")", sep=""),
         y = paste("PC2 (", percent(prop.pca[2]), ")", sep=""))

grid.arrange(p1, p2)



### change tab dataset


library(plotly)
dataset = data.frame(Y = tab[,"Y"],
                     pca = pca$x, lda = plda$x)


p <- plot_ly(dataset, x = ~lda.LD1, y = ~lda.LD2, z = ~lda.LD3, color = ~Y, colors = c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070')) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'LD1'),
                        yaxis = list(title = 'LD2'),
                        zaxis = list(title = 'LD3'))