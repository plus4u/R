## function, r 클래

# 평균, 표준편차, min, max 계산 >  > 

my_fun <- function(x, y, o) { 
  
  if ( o == 1) {
    z <- x + y
  } 
  
  else if ( o == 2) {
    
    z <- x - y
  } else {
    
    print("o should be either 1 or 2")
    
    z <- NA
  }
  
  return(z)
}


my_fun(10, 3, 3)


my_fun1 <- function(x) { 
  
  if ( x > 0 ) {
    a <- 1:x
    
  } 
  
  else if ( o <= 0 ) {
    
    a <- 999 - x
    
  } else {
    
    print("x should be number")
    
    a <- NA
  }
  
  return(mean(a))
}

a <- 1:10

mean(a)


my_fun1(10)





## 생성자 : R에서 클래스를 정의하는 문법


# 클래스이름 <- function(속성값1, 속성값2, 속성값3) {
#  obj <- list(속성이름1=속성값1, 속성이름2=속성값2, 속성이름3=속성값3)
#  class(obj) <- "클래스이름"
#  return(obj)
#  }


## 함수와 객체 비교 

h <- 10
v <- 20

area <- function(h, v) {
  return(h * v)
}

a = area(h, v)
cat(a)



## 객체 





Rectangle <- function(h, v) {
  obj <- list(h=h, v=v)
  class(obj) <- "Rectangle"
  return(obj)
}

area <- function(obj) {
  UseMethod("area", obj)
}

area.Rectangle <- function(obj) {
  return(obj$h * obj$v)
}


r <- Rectangle(10, 20)    
a <- area(r)
cat(a)



## 활용

a <- Rectangle(1, 1)   # 가로 1, 세로 1인 사각형
b <- Rectangle(2, 1)   # 가로 2, 세로 1인 사각형
c <- Rectangle(4, 2)   # 가로 4, 세로 2인 사각형
d <- Rectangle(6, 3)   # 가로 6, 세로 3인 사각형
e <- Rectangle(8, 5)   # 가로 8, 세로 5인 사각형


cat(area(a), "\n")
cat(area(b), "\n")
cat(area(c), "\n")
cat(area(d), "\n")
cat(area(e), "\n")










