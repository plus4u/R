## prcomp 
# https://www.datacamp.com/community/tutorials/pca-analysis-r 


mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE)

summary(mtcars.pca)

# summary(prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = F)) # standard deviation is greater



