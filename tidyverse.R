# tidyverse

library(tidyverse)

# make wide data (20 columns)
df <- matrix(rnorm(1000), ncol = 20)
df <- as.data.frame(df)
colnames(df) <- LETTERS[1:20]

# transform to long format (2 columns)
df <- gather(df, key = "name", value = "value")

# plot histigrams per name
ggplot(df) +
    geom_histogram(aes(value)) +
    facet_wrap(~name, ncol = 5)