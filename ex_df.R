### data.frame


a <- c(a=1:10)
b <- c(b=5:15)
ab <- cbind(a,b)
ab
plot()



# 1. enequal lenth data.frame

library(plyr)

df1 <- data.frame(x = 1:5, y = 5:1)
df2 <- data.frame(x = 6:15)

x <- cbind(df1, df2 )
x


xdf_combined <- rbind.fill(df1, df2)
print(df_combined)


##

attach(mtcars)
plot(wt, mpg, main="Scatterplot Example", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)

str(mtcars)

head(mtcars,20)


## help 

m <- cbind(1:5, 1:7)
m

m <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
m
m <- cbind(m, 8:14)[, c(1, 3, 2)] # insert a column
m
cbind(1:7, diag(3)) # vector is subset -> warning

cbind(0, rbind(1, 1:3))
cbind(I = 0, X = rbind(a = 1, b = 1:3))  # use some names
xx <- data.frame(I = rep(0,2))
cbind(xx, X = rbind(a = 1, b = 1:3))   # named differently

cbind(0, matrix(1, nrow = 0, ncol = 4)) #> Warning (making sense)
dim(cbind(0, matrix(1, nrow = 2, ncol = 0))) #-> 2 x 1

## deparse.level

dd <- 10
rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 0) # middle 2 rownames
rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 1) # 3 rownames (default)
rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 3) # 4 rownames

## cheap row names:
b0 <- gl(3,4, labels=letters[1:3])
bf <- setNames(b0, paste0("o", seq_along(b0)))
df  <- data.frame(a = 1, B = b0, f = gl(4,3))
df. <- data.frame(a = 1, B = bf, f = gl(4,3))
new <- data.frame(a = 8, B ="B", f = "1")
(df1  <- rbind(df , new))
(df.1 <- rbind(df., new))
stopifnot(identical(df1, rbind(df,  new, make.row.names=FALSE)),
          identical(df1, rbind(df., new, make.row.names=FALSE)))



## error

df1 <- data.frame(Intercept = .4, x1=.4, x2=.2, x3=.7)
df2 <- data.frame(Interceptlego = .5,        x2=.8       )
df1
df2


myList <- list(df1, df2)

dat <- data.frame()

for(i in seq(along=myList)) for(j in names(myList[[i]]))
    dat[i,j] <- myList[[i]][j]
dat


myList <- list(df2, df1)

myList




##

Died.At <- c(22,40,72,41)
Writer.At <- c(16, 18, 36, 36)
First.Name <- c("John", "Edgar", "Walt", "Jane")
Second.Name <- c("Doe", "Poe", "Whitman", "Austen")
Sex <- c("MALE", "MALE", "MALE", "FEMALE")
Date.Of.Death <- c("2015-05-10", "1849-10-07", "1892-03-26","1817-07-18")




# Assign different names to the columns of `writers_df`
names(writers_df) <- .("Age.At.Death", "Age.As.Writer", "Name", "Surname", "Gender", "Death")

# See the result
.....(writers_df)



# Assign different column names to `writers_df`
........(writers_df) <- c("Age.At.Death", "Age.As.Writer", "Name", "Surname", "Gender", "Death")

# Assign row names to `writers_df`
........(writers_df) <- c("ID1", "ID2", "ID3", "ID4")

# Return `writers_df`
..........




# Return the number of rows and columns
dim(writers_df)

# Retrieve the number of rows
dim(..........)[1]

# Retrieve the number of columns of `writers_df`
dim(..........)[2]


