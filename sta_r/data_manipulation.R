## sort, sample 

v1 <- sample(1:3, 9, replace = T)

v2 <- rep(c(10,30,20), each=3)

set.seed(123)
v3 <- round( rnorm(9, 30, 10), 1)

v4 <- c("a", "b", "a", "a", "b", "b", "a","a","b")

v5 <- sample(letters[1:3], 9, replace = T)
v5 <- toupper(v5)
v5

v15 <- data.frame(v1, v2, v3, v4, v5)

v15
 
sort(v1, decreasing = TRUE) # 내림차순 정렬
 
v15 [ order(v1, v2, v5, -v3), ]
 
## sum of certain columns

apply(v15[1:3], 2, sum )

# sum(x[1,1:2], x[1,2], x[1,3])

v15

rowSums(v15[, 1:3])
rowSums(v15[, c(1:2, 1:3)])


##

A <- data.frame(a=LETTERS[1:5], b=1:5, c=rnorm(5))
A$d <- NULL     # to delete column "d"
A$e <- 1:5      # add in a new column "e"

A


l <- list(first=A[1, ], second=A[2, ], rest=A[-c(1:2), ])
do.call(rbind, l)

l



## # iris 데이터 필터링
iris_num <- iris[1:10, 1:4]

# 랜덤으로 NA를 넣을 행/열 번호 뽑기
set.seed(123)
idx_r <- sample(1:10, 2)
idx_c <- sample(1:4, 2)

# NA 넣기
for(i in 1:2){
  iris_num[idx_r[i], idx_c[i]] <- NA
}

iris_num

idx_r
idx_c


## 
# 행(row) 단위로 mean 연산
apply(iris_num, 1, mean)

# 열(column) 단위로 mean 연산
apply(iris_num, 2, mean)



# 

# 열(column) 단위로 사용자 정의 함수(function)2 연산
apply(iris_num, 2, function(x) { median(x * 2 + 1) })



# 
# iris_num의 열 단위 평균이 vector 형태로 출력됨
apply(iris_num, 2, mean, na.rm = T)

# iris_num의 열 단위 평균이 list 형태로 출력됨
lapply(iris_num, mean, na.rm = T)



# 기본적으로 sapply 함수는 연산 결과를 벡터 형태로 출력한다.
sapply(iris_num, mean, na.rm = T)

# simplify = F이면 lapply와 동일하게 리스트 형태로 결과를 출력한다.
sapply(iris_num, mean, na.rm = T, simplify = F)
lapply(iris_num, mean, na.rm = T)



#
## lapply  # do.call

x <- list(1:3,4:6,7:9)

x

rbind(X)

do.call(rbind,X)




