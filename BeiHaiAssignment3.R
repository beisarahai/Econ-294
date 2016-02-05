<<<<<<< HEAD
#Bei Hai Assignment 3
# 0.
print(paste("Bei Hai","1505072","bhai@ucsc.edu", sep=","))

# 1.
library(foreign)
df.ex <- read.dta(
  "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)
class(df.ex)

# 2.
require(dplyr)
df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & month == 12
  )
print(nrow(df.ex.2))

df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & (month == 7 | month ==8 | month ==9)
  )
print(nrow(df.ex.2))
=======
# Bai Hai Assignment 3



>>>>>>> 395d0fe5b6625c6849f52fc915f9948e5dde9084
