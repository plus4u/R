## two-way anova
 

gender <- as.factor(c(rep("M", 9), rep("F", 9)))

class <- c("c_1", "c_1", "c_1", "c_2", "c_2", "c_2", "c_3", "c_3", "c_3")

class <- as.factor(c(rep(class, 2)))
 
s1 <- rnorm( 9, mean=5, sd=1) 
s2 <- rnorm( 9, mean=7, sd=1) 
total <- c(s1,s2)

# c(71, 77, 78, 76, 77, 78, 71, 70, 69, 76, 76, 80, 79, 78, 77, 71, 71, 70)
 
# summary statistics
score <- data.frame(gender, class, total)

# install.packages("doBy")

library(doBy)

summaryBy(total ~ gender, data=score, FUN = c(mean, sd, min, max))

summaryBy(total ~ class, data=score, FUN = c(mean, sd, min, max))

summary(total, data=score)


# box plot and interaction plot
par(mfrow = c(2, 2))

plot(total ~ gender, main="box plot by gender")
plot(total ~ class, main="box plot by class")
interaction.plot(gender, class, total, bty='l', main="interaction effect plot")
interaction.plot(class, gender, total, bty='l', main="interaction effect plot")

# two-way ANOVA : aov()

fit = aov( total ~ gender + class + gender:class)
summary(fit)

str(fit)
