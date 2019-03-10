## function pipe

library(MASS)

d <- data(package = "MASS")
## names of data sets in the package
str(d)
d$results[, "Item"]
library(dplyr)

head(iris)

iris %>% head()

## 
a1 <- group_by(hflights, Year, Month, DayofMonth)

a2 <- select(a1, Year:DayofMonth, ArrDelay, DepDelay)
a3 <- summarise(a2, arr = mean(ArrDelay, na.rm = TRUE), 
                  +                 dep = mean(DepDelay, na.rm = TRUE))
a4 <- filter(a3, arr > 30 | dep > 30)

## 
hflights_df %>% 
    +   group_by(Year, Month, DayofMonth) %>% 
    +   summarise(arr = mean(ArrDelay, na.rm = TRUE), 
                  +             dep = mean(DepDelay, na.rm = TRUE)) %>% 
    +   filter(arr > 30 |  dep > 30)




