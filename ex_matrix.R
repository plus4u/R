## matrix 


## 행렬 X와 행렬 Y 생성

a <- c(1, 1, 1)
a <- as.matrix(diag(a))

x <- matrix(1:9, nrow=3, ncol=3, byrow=FALSE, dimnames = NULL) # vertical direction  
x
a %*% x

a1 <- matrix(c(1, 0, 0, 0,0,1, 0,1, 0), nrow=3, ncol=3, byrow=F ) # *
a1
a1 %*% x


# diagonal matrix
a <- c(1, 2, 3, 4)
a <- as.matrix(diag(a)) # *
a


 


# zero matrix
a <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0), byrow = TRUE, nrow = 3)
a

# 대칭행렬 (symmetric matrix)
# symmetric matrix
s <- matrix(c(1:25), 5)

lower.tri(s, diag=FALSE)

s[lower.tri(s, diag=FALSE)]

t(s)[lower.tri(s, diag=FALSE)]

s[lower.tri(s, diag=FALSE)] = t(s)[lower.tri(s, diag=FALSE)]

s

### 
X <- matrix(1:4, nrow=2, ncol=2, byrow=FALSE, dimnames = NULL)
X

 
Y <- matrix(5:8, nrow=2, ncol=2, byrow=TRUE, dimnames = NULL)
Y


## 행렬 X와 행렬 Y의 각 숫자끼리의 연산: +, -, *, /, ^ 

X + Y

X - Y

X * Y

X / Y

X ^ Y

X %*% Y

## colMeans(), rowMeans()
colMeans(X)


rowMeans(X)

 

## colSums(), rowSums()
colSums(X)
 
rowSums(X)
 

colSums(Y, na.rm = TRUE)
 
rowSums(Y, na.rm = TRUE)
 

 
## 행렬의 전치 t()
t(X)



A = matrix(c(2,1,0,1,2,1,0,1,2),ncol=3); A
P = matrix(c(1,0,0,0,0,1,0,1,0),ncol=3); P

P%*%A

B=P%*%A; B
solve(B)
t(B)


