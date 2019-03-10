## read excel

# Loading
library(readxl)

getwd()

# xls files : 97~2003 worksheet
dat <- read_excel("source/simple_linear_reg.xls", col_names = T)

head(dat)

fit <- lm(revenue ~ advertising, data = dat)

summary(fit)

# advertising = 3, 4, 5  

x1 =0.64115 +  0.81340 * 3
x2 =0.64115 +  0.81340 * 5
x3 =0.64115 +  0.81340 * 6

x1
x2
x3


