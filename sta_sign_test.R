## sign test

# The following commands will install these packages if they are not already installed:
    
    
if(!require(psych)){install.packages("psych")}
if(!require(BSDA)){install.packages("BSDA")}
if(!require(DescTools)){install.packages("DescTools")}

##

Input =("
 Speaker  Time  Student  Likert
        Pooh      1     a        1       
        Pooh      1     b        4       
        Pooh      1     c        3        
        Pooh      1     d        3       
        Pooh      1     e        3       
        Pooh      1     f        3       
        Pooh      1     g        4       
        Pooh      1     h        3        
        Pooh      1     i        3       
        Pooh      1     j        3       
        Pooh      2     a        4
        Pooh      2     b        5
        Pooh      2     c        4 
        Pooh      2     d        5
        Pooh      2     e        4
        Pooh      2     f        5
        Pooh      2     g        3
        Pooh      2     h        4 
        Pooh      2     i        3
        Pooh      2     j        4      
        ")

Data = read.table(textConnection(Input),header=TRUE)


###  Check the data frame

library(psych)

headTail(Data)

str(Data)

summary(Data)

##

Time.1 = Data$Likert [Data$Time == 1]
Time.2 = Data$Likert [Data$Time == 2]

library(BSDA)

SIGN.test(x = Time.1,
          y = Time.2,
          alternative = "two.sided",
          conf.level = 0.95)

##
Time.1 = Data$Likert [Data$Time == 1]
Time.2 = Data$Likert [Data$Time == 2]

library(DescTools)

SignTest(x = Time.1,
         y = Time.2)