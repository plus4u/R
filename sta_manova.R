
sepl <- iris$Sepal.Length
petl <- iris$Petal.Length
sepl
str(iris) # Species Factor w/ 3 levels / 'data.frame':	150 obs. of  5 variables:

# MANOVA test
res.man <- manova(cbind(Sepal.Length, Petal.Length) ~ Species, data = iris)
summary(res.man)

res.man <- manova(cbind(Sepal.Width, Petal.Width) ~ Species, data = iris)
summary(res.man)

res.man[3]

str(res.man)

res.man['model']

str(res.man['model'])

# using manova_data from excel

manova_data <- read_excel("work_r/source/manova_data.xls")

manova_data

str(manova_data)


# 1
res.man <- manova(cbind(revenue, revisitRate) ~ cbind(smokingCheck, location), data = manova_data)

summary(res.man)

summary(aov(res.man))


# 2 : with interaction

res.man2 <- manova(cbind(revenue, revisitRate) ~ smokingCheck + location + smokingCheck: location, data = manova_data)

res.man2

summary(aov(res.man2))

summary(res.man2, test = "Wilks")


# 3 

attach(manova_data)

str(manova_data)

y1 <- revenue
y2 <- revisitRate
y1

Y <- cbind(y1,y2)
Y

a <- smokingCheck
b <- location

fit <- manova(Y ~ a:b)
fit <- manova(Y ~ a+b+a*b) # a:b

summary(fit, test="Pillai")

fit
summary(fit)
summary(manova(fit))

summary(aov(res.man))

options('code')

coef(res.man)

anova(res.man)

res.man

