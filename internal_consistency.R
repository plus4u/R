## 
library(psych)
tmp <- pcaFa[,2:4]

head(tmp)

alpha(tmp)

## Five ways to calculate internal consistency


temp <- tempfile()
temp

download.file("http://personality-testing.info/_rawdata/BIG5.zip", temp, mode="wb")
d <- read.table(unz(temp, "BIG5/data.csv"), header = TRUE, sep="\t")

unlink(temp); 
rm(temp)

head(d)
str(d)

# extract

d <- d[1:500, paste0("E", 1:10)]
