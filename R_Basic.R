###########################################################################
### getwd(), readxl
###########################################################################

getwd()


setwd("C:/Working/basic")

setwd("C:/Working/work_r")


library(readxl)

readxl_example()

readxl_example("clippy.xls")

xlsx_example <- readxl_example("datasets.xlsx")

td <- read_excel(xlsx_example)
td

excel_sheets(xlsx_example)

read_excel(xlsx_example, sheet = "chickwts")

read_excel(xlsx_example, n_max = 3)


###


getwd()

###

read_excel("cust_profile.xlsx")

td <- read_excel("C:/Working/basic/cust_profile.xlsx", # path
                 sheet = "cust_profile", # sheet name to read from
                 range = "B3:E8", # cell range to read from
                 col_names = TRUE, # TRUE to use the first row as column names
                 col_types = "guess", # guess the types of columns
                 na = "NA") # Character vector of strings to use for missing values

td

###



