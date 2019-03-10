## extract data / https://www.statmethods.net/management/subset.html 

# exclude variables v1, v2, v3
myvars <- names(mydata) %in% c("v1", "v2", "v3") 
newdata <- mydata[!myvars]

# exclude 3rd and 5th variable 
newdata <- mydata[c(-3,-5)]

# delete variables v3 and v5
mydata$v3 <- mydata$v5 <- NULL

# using subset function 
newdata <- subset(mydata, age >= 20 | age < 10, 
                  select=c(ID, Weight))

# take a random sample of size 50 from a dataset mydata 
# sample without replacement
mysample <- mydata[sample(1:nrow(mydata), 50,
                          replace=FALSE),]


###

##

library(ggplot2)

data(diamonds)

diamonds[ , grep("^c", colnames(diamonds))]

diamonds[ grep("^e", diamonds$cut), c(1:3) ]

head(diamonds, n=5)

diamonds[(diamonds$cut == "Fair") & (diamonds$price >= 18000), ]

diamonds[1:6, ]

diamonds[grep ("oo", diamonds$cut ), ]  ## 
 

