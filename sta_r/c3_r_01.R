### 1. random  2. descriptive statistics  3. plot  4, random log-normal distribution

## 1. random 


df = data.frame(matrix(rnorm(20), nrow=10))  # rnorm

v1 <- round(runif(12, 5.0, 10), 0)  # Generate a random number between 5.0 and 10
runif(3, min=5, max=10)    # define the range between 5 and 10


ceiling(3.1)

floor(3.1) # floor(x)

round(30.62345, 0)


# 2

set.seed(123)
 
v1 <- round(rnorm(1000, mean=10, sd=2), 0) # provide own mean and standard deviation

summary(v1)

sd(v1)
var(v1)



# 3. plot # https://www.harding.edu/fmccown/r/ 

# Define 2 vectors
cars <- c(1, 3, 5, 4, 9, 12)
trucks <- c(2, 4, 4, 7, 13, 15)

# Graph cars using a y axis that ranges from 0 to 12
plot(cars, type="o", col="blue", ylim=c(0,15))

# Graph trucks with red dashed line and square points
lines(trucks, type="o", pch=22, lty=2, col="red")

# Create a title with a red, bold/italic font
title(main="Autos lines", col.main="red", font.main=4)


## dat 

cars = read.csv("cars.csv", header = T)
getwd()
cars

# Graph cars with specified labels for axes.  Use blue 
# borders and diagnal lines in bars.
barplot(cars$sedan, main="Cars", xlab="Days",  
        ylab="Total", names.arg=c("Mon","Tue","Wed","Thu","Fri"), 
        border="blue", density=c(10,20,30,40,50))


# 

# Graph autos with adjacent bars using rainbow colors
barplot(as.matrix(cars), main="Autos", ylab= "Total",
        beside=TRUE, col=rainbow(5))

# Place the legend at the top-left corner with no frame  
# using rainbow colors
legend("topleft", c("Mon","Tue","Wed","Thu","Fri"), cex=0.6, bty="n", fill=rainbow(5))


##

# Expand right side of clipping rect to make room for the legend
par(xpd=T, mar=par()$mar+c(0,0,0,4))

# Graph autos (transposing the matrix) using heat colors,  
# put 10% of the space between each bar, and make labels  
# smaller with horizontal y-axis labels
barplot(t(cars), main="Autos", ylab="Total", 
        col=heat.colors(3), space=0.1, cex.axis=0.8, las=1,
        names.arg=c("Mon","Tue","Wed","Thu","Fri"), cex=0.8) 

# Place the legend at (6,30) using heat colors
legend(6, 30, names(autos_data), cex=0.8, fill=heat.colors(3));

# Restore default clipping rect
par(mar=c(5, 4, 4, 2) + 0.1)


##
# Create a histogram for autos in light blue with the y axis
# ranging from 0-10
hist(cars$sedan, col="lightblue", ylim=c(0,10))


## 
# Get a random log-normal distribution
r <- rlnorm(1000)

# Get the distribution without plotting it using tighter breaks
h <- hist(r, plot=F, breaks=c(seq(0,max(r)+1, .1)))

# Plot the distribution using log scale on both axes, and use
# blue points
plot(h$counts, log="xy", pch=20, col="blue",
     main="Log-normal distribution",
     xlab="Value", ylab="Frequency")



##

# Make an empty chart
plot(1, 1, xlim=c(1,5.5), ylim=c(0,7), type="n", ann=FALSE)

# Plot digits 0-4 with increasing size and color
text(1:5, rep(6,5), labels=c(0:4), cex=1:5, col=1:5)

# Plot symbols 0-4 with increasing size and color
points(1:5, rep(5,5), cex=1:5, col=1:5, pch=0:4)
text((1:5)+0.4, rep(5,5), cex=0.6, (0:4))

# Plot symbols 5-9 with labels
points(1:5, rep(4,5), cex=2, pch=(5:9))
text((1:5)+0.4, rep(4,5), cex=0.6, (5:9))



## http://www.federalreserve.gov/releases/h10/Summary/
## 
d = read.table('dollar_vs_major_currencies_index.txt', header=F, sep="t", col.names=c("month", "index"))



### ggplot2

# Libraries
library(tidyverse)
# 필요한 패키지를 로딩중입니다: ggplot2
library(plotly) # 패키지 ‘plotly’는 R 버전 3.5.2에서 작성

# Scatterplot
p=ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species, shape=Species)) + 
  geom_point(size=6, alpha=0.6)
p    

###

#Library
install.packages("leaflect")
library(leaflet)

# Background 1: NASA
leaflet() %>% 
  addTiles() %>% 
  setView( lng = 2.34, lat = 48.85, zoom = 5 ) %>% 
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")


# Background 2: World Imagery
leaflet() %>% 
  addTiles() %>% 
  setView( lng = 2.34, lat = 48.85, zoom = 3 ) %>% 
  addProviderTiles("Esri.WorldImagery") 


### 

x <- seq(-4, 4, length=100)
hx <- dnorm(x)
hx

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)

##

# Children's IQ scores are normally distributed with a
# mean of 100 and a standard deviation of 15. What
# proportion of children are expected to have an IQ between
# 80 and 120?

mean=100; sd=15
lb=80; ub=120

x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab="IQ Values", ylab="",
     main="Normal Distribution", axes=FALSE)

i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red") 

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)
result <- paste("P(",lb,"< IQ <",ub,") =",
                signif(area, digits=3))
mtext(result,3)
axis(1, at=seq(40, 160, 20), pos=0)

### Q-Q plots
par(mfrow=c(1,2))

# create sample data 
x <- rt(100, df=3)

# normal fit 
qqnorm(x); qqline(x)

# t(3Df) fit 
qqplot(rt(1000,df=3), x, main="t(3) Q-Q Plot", 
       ylab="Sample Quantiles")
abline(0,1)


##
qplot(carat, price, data = diamonds, geom="point" ,colour=clarity) # ??????(1)


# 

p <- ggplot(data=diamonds, aes(x=carat,y=price))

p + geom_smooth() # ??? (1)

p + geom_smooth(aes(group=clarity)) # ??? (2)


###

p <- ggplot(mtcars, aes(wt, mpg))

# Add aesthetic mappings
p + geom_point(aes(colour = factor(cyl)))

##

par(mfrow=c(1,1))

# Boxplot of MPG by Car Cylinders 
boxplot(mpg~cyl,data=mtcars, main="Car Milage Data", 
        xlab="Number of Cylinders", ylab="Miles Per Gallon")



# Better example

install.packages("maps")
library(maps)

crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimesm <- reshape2::melt(crimes, id = 1)
if (require(maps)) {
  states_map <- map_data("state")
  ggplot(crimes, aes(map_id = state)) +
    geom_map(aes(fill = Murder), map = states_map) +
    expand_limits(x = states_map$long, y = states_map$lat)
  
  last_plot() + coord_map()
  ggplot(crimesm, aes(map_id = state)) +
    geom_map(aes(fill = value), map = states_map) +
    expand_limits(x = states_map$long, y = states_map$lat) +
    facet_wrap( ~ variable)
}



