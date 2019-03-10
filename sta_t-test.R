## t-test


a = c(175, 168, 168, 190, 156, 181, 182, 175, 174, 179)
b = c(185, 169, 173, 173, 188, 186, 175, 174, 179, 180)

c <- a-10

var.test(a,b)

t.test(a, mu=170)


## independent 2-group t-test

 # t.test(y~x) # where y is numeric and x is a binary factor

 # t.test(y1,y2) # where y1 and y2 are numeric

 # F test to compare two variances


t.test(a,b,var.equal=T)

t.test(a,b, var.equal=T, alt='two.sided')

t.test(a,b, var.equal=T, alt='less')

t.test(a,b, var.equal=T, alt='greater')

t.test(a,b, var.equal=T, paired=FALSE)

t.test(c,b, var.equal=T)

t.test(b, c, var.equal=T, alt='greater')


# paired t-test
# t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric

t.test(a,b, var.equal=T, paired=T)


# one sample t-test
# t.test(y,mu=3) # Ho: mu=3


## 

require(graphics)

t.test(1:10, y = c(7:20))      # P = .00001855
t.test(1:10, y = c(7:20, 200)) # P = .1245    -- NOT significant anymore

## Classical example: Student's sleep data
plot(extra ~ group, data = sleep)
## Traditional interface
with(sleep, t.test(extra[group == 1], extra[group == 2]))
## Formula interface
t.test(extra ~ group, data = sleep)


## datasets :: pressure 
plot(pressure)

plot(pressure ~ temperature, data = pressure)
text(150, 600, "Pressure (mm Hg)\nversus\nTemperature (Celsius)")

str(pressure)



## independent two population proportions test : prop.test() 

prop <- c(0.33, 0.41) # proportion of events
n <- c(500, 600) # number of trials
x <- prop*n # number of events
x  

prop.test(x = x, # number of events
                n = n, # number of trials
                alternative = c("two.sided"), # alternative = c("two.sided", "less", "greater")
                conf.level = 0.95) # confidence level (= 1- significance level 'alpha')



## independent two population proportions test : prop.test()
