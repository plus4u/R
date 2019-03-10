### data type

## mean, sd

x <- 1:10

mean(x)
sd(x)
var(x)

## list

x <- c(1:3)

no <- c(1:3)
name <- c("a", "b", "c")
weight <- c(50, 60, 70)

df1 <- data.frame(no, name, weight) 

df1

# 
list_x <- list(x, df1)
list_x

list_y <- list(me=c(1:3), sd =0.5, center=0)
list_y$me[2]
# [1] 2   # 접근방법 ; 리스트 변수[인덱스]  

list_y$me[-c(2,3)] # 제거 

list_y <- list(list_y, list_x)

list_y

# 빈리스트를 생성하고 인덱스를 이름으로 지정하는 방법 ;  

emty_list <- list.files("./var1/")
emty_list

filenames <- sapply(emty_list,list) 
filenames

# Foreach 
# .final

# compare data.frame

tem <- 1:3

names(tem) <-  c("hd1", "hd2", "hd3")

mode(tem)

df2 <- data.frame(x=1:3, y=c("aa","bb","cc")) 
mode(df2)

# List 중간에 값을 삽입 하기

append(list_x, list(x=10, y=20))


append(list_x, list(f=1:5), after=0) # after - position to append


## matrix

dim(2,3)




## 데이터 프레임

row.names(df2)

data.matrix()


# read.table(), read.csv()
# list -> data.frame 

data.frame(list_x)
 
a <- data.frame(1:3, 11:13)

a

str(a)

a$X1.3

grep(2,a[,2])


grep("X1.3",colnames(a))

names(a)

names(a) <- c("hd1", "hd2")

names(a)[names(a)=="hd1"] <- c("newhd")

names(a)

a$newcol <- NA

a$badcol <- NULL

subset(a, select = -newcol)


cbind 
rbind 

do.call(rbind, mylist)
  
df1

df2

merge (df1, df2, by = intersect(names(df1),names(df2)))
       
      