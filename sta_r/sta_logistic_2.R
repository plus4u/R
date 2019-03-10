## logistic regression,   xtabs 


library(aod)
library(ggplot2)

my_data <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv") 
 

xtabs(~admit+rank, data=my_data)  # 1

my_data 



#

my_data$rank <- factor(my_data$rank)

my_logit <- glm(admit ~ gre + gpa + rank, data = my_data, family = "binomial")

summary(my_logit)
 

## xtabs

d <- data.frame(x=c("1", "2", "2", "1"),   y=c("A", "B", "A", "B"),    num=c(3, 5, 8, 7))

# append(d, c("1", "A", "10"), after = 5)  # ??

xtabs(num ~ x + y, data=d)
