## function, r Ŭ��

# ���, ǥ������, min, max ��� >  > 

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





## ������ : R���� Ŭ������ �����ϴ� ����


# Ŭ�����̸� <- function(�Ӽ���1, �Ӽ���2, �Ӽ���3) {
#  obj <- list(�Ӽ��̸�1=�Ӽ���1, �Ӽ��̸�2=�Ӽ���2, �Ӽ��̸�3=�Ӽ���3)
#  class(obj) <- "Ŭ�����̸�"
#  return(obj)
#  }


## �Լ��� ��ü �� 

h <- 10
v <- 20

area <- function(h, v) {
  return(h * v)
}

a = area(h, v)
cat(a)



## ��ü 





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



## Ȱ��

a <- Rectangle(1, 1)   # ���� 1, ���� 1�� �簢��
b <- Rectangle(2, 1)   # ���� 2, ���� 1�� �簢��
c <- Rectangle(4, 2)   # ���� 4, ���� 2�� �簢��
d <- Rectangle(6, 3)   # ���� 6, ���� 3�� �簢��
e <- Rectangle(8, 5)   # ���� 8, ���� 5�� �簢��


cat(area(a), "\n")
cat(area(b), "\n")
cat(area(c), "\n")
cat(area(d), "\n")
cat(area(e), "\n")









