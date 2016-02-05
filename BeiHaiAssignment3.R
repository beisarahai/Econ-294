##Assignment 3
#0
print(paste("Bei Hai","1505072","bhai@ucsc.edu", sep=","))

#1
library(foreign)
df.ex <- read.dta(
  "https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta"
)
class(df.ex)

#2
require(dplyr)
df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & month == 12
  )
print(nrow(df.ex.2))
#13261 observations

df.ex.2 <- df.ex %>%
  dplyr::filter(
    year == 2013 & (month == 7 | month ==8 | month ==9)
  )
print(nrow(df.ex.2))
#39657 observations

#3
df.ex.3a <- arrange(df.ex, year,month)

#4
df.ex.4a <- select(df.ex, year:age)
df.ex.4b <- select(df.ex, year, month, starts_with("i"))
print(distinct(select(df.ex,state)))

#5
stndz <- function(x){
  (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
}

nrmlz <- function(x){
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}
rw.stndz <- stndz(df.ex$rw)
rw_nrmlz <- nrmlz(df.ex$rw)

df.ex.5a <- df.ex %>%
  mutate(
    rw.stndz = stndz(rw),
    rw_nrmlz = nrmlz(rw) 
  ) %>%
  select(rw.stndz,rw_nrmlz)

df.ex.5b <- df.ex %>%
  group_by(year,month) %>%
  mutate(
    rw.stndz = stndz(rw),
    rw_nrmlz = nrmlz(rw),
    count = n()
  ) %>%
  select(year,month,rw.stndz,rw_nrmlz,count)

#6
df.ex.6 <- df.ex %>%
  group_by(year,month,state) %>%
  summarise(
    min_rw = min(rw, na.rm=T),
    qua1_rw = quantile(rw, .25, na.rm=T),
    mean_rw = mean(rw, na.rm=T),
    med_rw = median(rw, na.rm=T),
    qua3_rw = quantile(rw, .75, na.rm=T),
    max_rw = max(rw, na.rm=T),
    count = n()
  )
print(nrow(df.ex.6))
#4284 obseravtions

print(
  df.ex.6 %>%
    ungroup()%>%
    arrange(desc(mean_rw)) %>%
    select(year,month,state)%>%
    head(1)
)
# Highest mean real wage: 2013/12 DC

#7
df.ex.7a<-df.ex %>%
  group_by(year,month,state)%>%
  ungroup()%>%
  arrange (year, month,as.character(state))

