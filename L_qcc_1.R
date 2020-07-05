##

library(qcc)


## x-bar chart

data(pistonrings)

pistonrings

# attach(pistonrings)

# diameter <- qccGroups(diameter, sample, data = pistonrings)  # error 

# diameter <- qcc.groups(diameter, sample, data = pistonrings)



# 40 sample of 5 obs each
qcc.groups(diameter, sample)
diameter <- qcc.groups(diameter, sample)

head(diameter)


# some obs are removed, the result is still a 40x5 matrix but with NAs added

diameter <- qcc.groups(diameter[-c(1,2,50,52, 199)], sample[-c(1,2,50,52, 199)])

q1 <- qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,])

plot(q1, fill = FALSE)


### youtube

aggregate(diameter~sample, data=pistonrings, mean)

qcc(diameter, type="xbar", std.dev = "UWAVE-SD")

qcc(diameter, type = "S")
 
#

p.data = c(1, 3, 10, 15, 7, 25)

p.data = c(1:100)

n=100
qcc(p.data, type = "p", sizes = 100)



### pareto chart

library(qcc)

library(grDevices) #color Palette

defect <- c(2, 5, 2, 15, 3) # 데이터 만들기

names(defect) <- c("C1", "C2", "C3", "C4", "V1") #x axis titles

pareto.chart(
  defect,  #데이터
  ylab="Frecuency",  # 왼쪽 축 제목
  col=heat.colors(length(defect)), # 각 막대의 색깔
  cumperc = seq(0, 100, by = 5),  # 오른쪽 y축 범위 및 간격  
  ylab2 = "Cumulative Percentage", # 오른쪽 축 제목
  main = "Pareto Chart of defect", # 그래프 제목
  xlab = "Defect", #x축 제목
  ) 

### 

set.seed(1)


x1 = sample( 100:200, 100, replace=TRUE)

x2 = sample(3, 100, replace=TRUE, prob=c(0.1, 0.2, 0.7)) 

x3 <- data.frame(x1,x2)

summary(x3)

head(x3)

x <- aggregate(x3, list(x3$x2), sum)
x

pareto.chart( x$x1,  ylab="Frecuency", cumperc = seq(0, 100, by = 5),  # 오른쪽 y축 범위 및 간격  
  ylab2 = "Cumulative Percentage", # 오른쪽 축 제목
  main = "Pareto Chart of defect", # 그래프 제목
  xlab = "category", #x축 제목
) 

