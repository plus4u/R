##

# non-parametric 

cor(c(1, 4, 3, 6, 5), c(2, 0, 7, 8, 9), method="kendall") # 0.4
cor(c(1, 4, 3, 2, 5), c(2, 3, 1, 4, 5), method="kendall") # 0.4 
cor(c(1, 4, 3, 2, 5), c(2, 3, 1, 4, 5), method="spearman")  # 0.5 / excel = 0.5 

cor(c(1, 3, 3, 2, 5), c(2, 3, 1, 4, 5), method="spearman")  # 0.41 tie / excel = 0.55 
cor(c(1, 3, 3, 2, 5), c(2, 3, 1, 4, 5), method="kendall")  # 0.31


cor(c(1, 2, 3, 4, 5), c(1, 0, 3, 4, 5), method="kendall") 

input1 <- ("
x y
1 1
2 0
3 3
4 4
5 5
")
 
input1

D1 = read.table(textConnection(input1),header=TRUE)
D1

## compare two cases if tied

cor(D1, method="kendall") 
cor(D1)


# Correlations/covariances among numeric variables in 
# data frame mtcars. Use listwise deletion of missing data. 
cor(mtcars, use="complete.obs", method="kendall") 
cov(mtcars, use="complete.obs")


# polychoric correlation
# x is a contingency table of counts
library(polycor)
polychor(x) 


# polyserial (numeric-ordinal), 
# and polychoric (ordinal-ordinal)
# x is a data frame with ordered factors 
# and numeric variables
library(polycor)
hetcor(x) 



## Spearman rank correlation

Input = ("
Volume   Pitch 
         1760    529 
         2040    566
         2440    473
         2550    461
         2730    465
         2740    532
         3010    484
         3080    527
         3370    488
         3740    485
         4910    478
         5090    434
         5090    468
         5380    449
         5850    425
         6730    389
         6990    421
         7960    416
         ")

Data = read.table(textConnection(Input),header=TRUE)
Data


cor.test( ~ Pitch + Volume, 
          data=Data,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)

cor(Data, method="kendall") 



# Simple plot of the data


plot(Pitch ~ Volume, 
     data=Data, 
     pch=16)


### --------------------------------------------------------------
### Spearman rank correlation, species diversity example
### p. 214
### --------------------------------------------------------------

Input = ("
         Town                  State  Latitude  Species
         'Bombay Hook'          DE     39.217    128
         'Cape Henlopen'        DE     38.800    137
         'Middletown'           DE     39.467    108
         'Milford'              DE     38.958    118
         'Rehoboth'             DE     38.600    135
         'Seaford-Nanticoke'    DE     38.583     94
         'Wilmington'           DE     39.733    113
         'Crisfield'            MD     38.033    118
         'Denton'               MD     38.900     96
         'Elkton'               MD     39.533     98
         'Lower Kent County'    MD     39.133    121
         'Ocean City'           MD     38.317    152
         'Salisbury'            MD     38.333    108
         'S Dorchester County'  MD     38.367    118
         'Cape Charles'         VA     37.200    157
         'Chincoteague'         VA     37.967    125
         'Wachapreague'         VA     37.667    114
         ")

Data = read.table(textConnection(Input),header=TRUE)

cor.test( ~ Species + Latitude, 
         data=Data,
         method = "spearman",
         continuity = FALSE,
         conf.level = 0.95)


plot(Species ~ Latitude, 
     data=Data, 
     pch=16)