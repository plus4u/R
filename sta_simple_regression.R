# installing and loading readxl package 

# install.packages('readxl')

library(readxl)

getwd()

## # read_excel("work_r/source/manova_data.xls") // notice : path, xls 

# importing excel file by using read_excel() function

# ld <- read_excel("c:/R/work_r/source/simple_regression.xlsx")

# ld <- read_excel("/work_r/source/simple_regression.xlsx", col_names = TRUE)

ld <- read_xls("work_r/source/simple_regression.xls", 

                           sheet = "Sheet1", # sheet name to read from
                               
                           range = "A1:B61", # cell range to read from
                               
                        col_names = TRUE) # TRUE to use the first row as column names
                               
                           #    col_types = "guess", # guess the types of columns
                               
                           #    na = "NA") # Character vector of strings to use for missing values

head(ld)
str(ld)


# RStudio GUI / Environments / import dataset / From excel
# options('encoding')
## library(readxl)
 

## simple regression

attach(ld)

l1 <- lm(revenue~advertising)

summary(l1)


# scatter plot

plot(revenue~advertising)

abline(l1, col='blue')
