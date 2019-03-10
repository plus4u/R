# installing and loading readxl package 


df = read.table("myfile.csv", header = TRUE)

# install.packages('readxl')

library(readxl)

pcaFa <- read_excel("work_r/source/pcaFa_data.xls", col_names = TRUE)

# write 

write.csv(pcaFa, file = "pcaFa.csv" )


getwd()

# importing excel file by using read_excel() function

customer_profile <- read_excel("C:/R/work_r/customer.xlsx", # path
                               
                               sheet = "cust_profile", # sheet name to read from
                               
                               range = "B3:E8", # cell range to read from
                               
                               col_names = TRUE, # TRUE to use the first row as column names
                               
                               col_types = "guess", # guess the types of columns
                               
                               na = "NA") # Character vector of strings to use for missing values

str(customer_profile)

 
# RStudio GUI / Environments / import dataset / From excel

# options('encoding')


## library(readxl)

manova_data <- read_excel("work_r/source/manova_data.xls")

# View(manova_data)



