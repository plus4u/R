### basic, eigenvalue, shiny

## basic


x1 <- c("a", "b", "c")
x2 <- list (a=list(val=c(1, 2, 3)), b=list(val=c(1:5)))
x <- c(1:9)

# 벡터를 열(column) 기준으로 행렬로 변환
x3 <- matrix(x, byrow = F, ncol = 3) 
dimnames(x3) <- list(c("r1", "r2", "r3"), c("c1", "c2", "c3")) 
x4 <- array(1:12, dim=c(2, 2, 3)) 

##

x <-(1:5)
y <- c(1,1,3,7, 9)

x > y
x >= y

x> 2 & x < 6

x < 3 | x > 4 

2^3

5%%3 

5%/%3 # 정수형 나누기 



##

# making square matrix A
A <- matrix(c(1, 3, 5, 0.3, 1, 3, 0.2, 0.3, 1), nc = 3, byrow = TRUE)

A <- matrix(c(1, 2, 3, 4), nc = 2, byrow = TRUE)

A

# eigenvalue & eigenvector of matrix A
lambda_A <- eigen(A)
lambda_A

lambda_A$vectors[, 3]

round (lambda_A$vectors[, 3], digits = 2)

options("scipen" = 100) 
round (lambda_A$vectors[, 3], digits = 2)



##

library(shiny)

runExample("01_hello")

## 

library(shiny)

ui <- fluidPage()

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
