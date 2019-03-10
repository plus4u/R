### 1. time series data   2. matrix data   3. chain operation  4. time series 
### 5. xts 


## 1. 

## 1.2. 데이터셋에서 불러오기----------------------------------------------------
# 맥주생산량 데이터
library(readxl)

beer_df <- read.csv("https://raw.githubusercontent.com/jamesrobertlloyd/gp-structure-search/master/data/raw/TSDL/monthly-beer-production-in-austr.csv") 

names(beer_df) <- c("dtime", "beer")

beer_df <- beer_df %>% dplyr::filter(!is.na(beer)) %>% mutate(dtime=ymd(paste0(dtime,"-01")))

head(beer_df)

beer_df[,-1]


## ts 변환 

beer_ts <- ts(beer_df[,-1], start=c(1956,01), frequency=12)

head(beer_ts)

par(mfrow=c(1,2))

plot(beer_df, main="시계열 정보 활용 못함", xlab="", ylab="맥주생산량")
plot(beer_ts, main="시계열 정보 활용 직선으로 연결함", xlab="", ylab="맥주생산량")


beer_ts

# 

# xts 변환
beer_xts <- as.xts(beer_df[,-1], order.by=beer_df$dtime)

par(mfrow=c(1,1))
plot(beer_xts)



## 2.

starting_df <- data.frame(row.names= c(LETTERS[1:4]), a = c(1:4), 
                          b = seq(0.02,0.08,by=0.02),
                          c = c("Aaaa","Bbbb","Cccc","Dddd"))


starting_df



## 

matrix(data=c(1:6), nrow=2, ncol=3)

matrix(data=c(1:6), nrow=2, ncol=3, byrow=T)

x <- matrix(data=c(1:6), nrow=2, ncol=3, dimnames=list(c("1행", "2행"),c("1열","2","3")))

x

t(x)



## 3. 

library(dplyr) 
library(MASS) # dataframe Cars93

 

# why chaining?  when to use chain operation?

# : (1) step-by-step operation, saving the intermediate results


select <- dplyr::select # assigning select() functon of dplyr package explicitly



a1 <- group_by(Cars93, Origin, Type, Cylinders)
a2 <- select(a1, Price, MPG.highway)

a3 <- summarise(a2, Price_m = mean(Price, na.rm = TRUE), MPG.highway_m = mean(MPG.highway, na.rm = TRUE))

a4 <- filter(a3, Price_m > 10 | MPG.highway_m > 25)

a4 


## chain(pipe) operator 는 %>% 이며, 단축키는 shift+ctrl+M  


Cars93 %>%  # dataframe name
  group_by(Origin, Type, Cylinders) %>%  # group_by()
  select(Price, MPG.highway) %>%  # select() columns
  summarise(
    Price_m = mean(Price, na.rm = TRUE),
    MPG.highway_m = mean(MPG.highway, na.rm = TRUE)  # summarise()
  ) %>%
  filter(Price_m > 10 | MPG.highway_m > 25)  # filter() condition

 

## 4 
# 1. 시계열 데이터 --------------------------------------------------------------
## 1.1. 처음부터 생성------------------------------------------------------------
# Time Series:   Start = 1  End = 7 

dat <- c(7,5,3,1,3,5,7)
dat_ts <- as.ts(dat)
dat_ts

# 
 
dat_quarter_ts <- ts(dat, start=c(1987,3), frequency=4)
dat_quarter_ts
      
# 특정기간 뽑아내기

window(dat_quarter_ts, start=c(1987,3), end=c(1988,4))


## 5. 

library(xts)
## Loading required package: zoo
## 
## Attaching package: 'zoo'
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric

# xts 예제 데이터 탐색
data(sample_matrix)
head(sample_matrix)

##                Open     High      Low    Close
## 2007-01-02 50.03978 50.11778 49.95041 50.11778
## 2007-01-03 50.23050 50.42188 50.23050 50.39767
## 2007-01-04 50.42096 50.42096 50.26414 50.33236
## 2007-01-05 50.37347 50.37347 50.22103 50.33459
## 2007-01-06 50.24433 50.24433 50.11121 50.18112
## 2007-01-07 50.13211 50.21561 49.99185 49.99185

class(sample_matrix)

## [1] "matrix"
str(sample_matrix)

##  num [1:180, 1:4] 50 50.2 50.4 50.4 50.2 ...
##  - attr(*, "dimnames")=List of 2
##   ..$ : chr [1:180] "2007-01-02" "2007-01-03" "2007-01-04" "2007-01-05" ...
##   ..$ : chr [1:4] "Open" "High" "Low" "Close"
xts_matrix <- as.xts(sample_matrix, descr = 'my new xts object')
str(xts_matrix)
## An 'xts' object on 2007-01-02/2007-06-30 containing:
##   Data: num [1:180, 1:4] 50 50.2 50.4 50.4 50.2 ...
##  - attr(*, "dimnames")=List of 2
##   ..$ : NULL
##   ..$ : chr [1:4] "Open" "High" "Low" "Close"
##   Indexed by objects of class: [POSIXct,POSIXt] TZ: 
##   xts Attributes:  
## List of 1
##  $ descr: chr "my new xts object"
# Simple plot


plot(xts_matrix[,1], main = "Our first xts plot", ex.main = 0.8)


#  
# Candle plot

plot(xts_matrix, main = "Candle plot on xts object", cex.main = 0.8, type = "c")

range <- "2007-03-15::2007-06-15"
plot(xts_matrix[range])

