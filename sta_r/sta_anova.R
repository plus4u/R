### anova : one-way, two-way, manova  , plot 

## one-way

# 1. 오차의 등분산성 검정  : p > .05

bartlett.test(Sepal.Width~Species, data=iris)
 

# 2

dat <-aov(Sepal.Width~Species, data=iris)

summary(dat)

## two-way

# If .txt tab file, use this
my_data <- read.delim(file.choose())
# Or, if .csv file, use this
my_data <- read.csv(file.choose())

# 
library(readxl)
dat <- read_excel(file.choose())

dat <- read_excel("source/anova_two.xls", col_names = T)

# Show a random sample
set.seed(1234)
dplyr::sample_n(dat, 10)

#
colnames(dat) = c("smo", "loc","rev")
head(dat,10)

fit <- aov(rev~ smo*loc, data = dat)
fit
summary(fit)

# These two calls are equivalent 
fit2 <- aov(rev ~ smo + loc + smo:loc, data = dat) 


table(dat$smo, dat$loc)

par(mfrow=c(1,1)) 

# Box plot with two factor variables
x <- boxplot(rev ~ smo * loc, data=dat, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), ylab="revenue")

x # combination of smo, loc

# Two-way interaction plot
interaction.plot(x.factor = dat$smo, trace.factor = dat$loc, 
                 response = dat$rev, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Smoke", ylab="Revenue",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"))
 
# 



# Convert smoking as a factor and recode the levels 

dat$smo <- factor(dat$smo, levels = c(1, 2, 3), labels = c("sy", "sn", "st"))
dat$loc <- factor(dat$loc, levels = c(1, 2, 3), labels = c("hd", "jr", "gn"))

head(dat)


## manova
 

fit <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
summary(fit)




## plot

## http://www.sthda.com/english/wiki/two-way-anova-test-in-r 

install.packages("ggpubr")



# Box plot with multiple groups
# +++++++++++++++++++++
# Plot tooth length ("len") by groups ("dose")
# Color box plot by a second group: "supp"
library("ggpubr")
ggboxplot(my_data, x = "dose", y = "len", color = "supp",
          palette = c("#00AFBB", "#E7B800"))



# Line plots with multiple groups
# +++++++++++++++++++++++
# Plot tooth length ("len") by groups ("dose")
# Color box plot by a second group: "supp"
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
library("ggpubr")
ggline(my_data, x = "dose", y = "len", color = "supp",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))


