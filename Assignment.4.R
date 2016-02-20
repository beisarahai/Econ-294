#Bei Hai Assignment 4
# 0:
print(paste(
  "Bei",
  "Hai",
  "1505072",
  sep = " "
)
)

# 1
library(foreign)

flights <- read.csv(
  url("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/flights.csv"),
  stringsAsFactors = FALSE
)


planes <- read.csv(
  file = "https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/planes.csv",
  stringsAsFactors = FALSE
)

weather <- read.csv(
  url("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/weather.csv"),
  stringsAsFactors = FALSE
)

airports <- read.csv(
  url("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/airports.csv"),
  stringsAsFactors = FALSE
)

# 2
flights$date <- as.Date(flights$date)
weather$date <- as.Date(weather$date)

# 3
require(dplyr)
flights.2a <- flights %>%
  dplyr::filter(
    dest == "SFO" | dest == "OAK"
  )

print(nrow(flights.2a))
flights.2b <- flights %>%
  dplyr::filter(
    dep_delay >= 60 | arr_delay >= 60
  )
print(nrow(flights.2b))

flights.2c <- flights %>%
  dplyr::filter(
    arr_delay > 2*dep_delay
  )

print(nrow(flights.2c))


#4
flights.4a <- flights %>%
  select(
    ends_with("delay")
  )

flights.4b <- flights %>%
  select(
    contains("delay")
  )

flights.4c <- flights %>%
  select(
    matches("._.")
  )


# 5
flights.5a <- flights %>%
  arrange(desc(dep_delay)) %>%
  head(5)
print(flights.5a$flight)

flights.5b <- flights %>%
  arrange(desc((dep_delay) - (arr_delay))) %>%
  head(5)

print(flights.5b$flight)


#6

flights <- flights %>%
  mutate(speed = dist/(time/60))

flights <- flights %>%
  mutate(delta = dep_delay - arr_delay)

flights.6a <- flights %>%
  arrange(desc(speed)) %>%
  head(5)

print(flights.6a$flight)
flights.6b <- flights %>%
  arrange(desc(delta)) %>%
  head(5)
print(flights.6b$flight)

flights.6c <- flights %>%
  arrange(delta) %>%
  head(5)

print(flights.6c$flight)
# 7
cancelled_flights <- flights %>%
  group_by(carrier) %>%
  summarize(
    min.cancelled = min(cancelled, na.rm=T),
    q1.cancelled = quantile(cancelled, .25, na.rm=T),
    mean.cancelled = mean(cancelled, na.rm=T),
    med.cancelled = median(cancelled, na.rm=T),
    q3.cancelled = quantile(cancelled, .75, na.rm=T),
    quan90.cancelled = quantile(cancelled, .90, na.rm=T),
    max.cancelled = max(cancelled, na.rm=T),
    totalflights = n()
  )


percent<- flights %>%
  group_by(carrier) %>%
  summarize(
    min.percent = min(cancelled/n(), na.rm=T),
    q1.percent = quantile(cancelled/n(), .25, na.rm=T),
    avg.percent = mean(cancelled/n(), na.rm=T),
    med.percent = median(cancelled/n(), na.rm=T),
    q3.percent = quantile(cancelled/n(), .75, na.rm=T),
    quan90.percent = quantile(cancelled/n(), .90, na.rm=T),
    max.percent = max(cancelled/n(), na.rm=T)
  )

delta <- flights %>%
  group_by(carrier) %>%
  summarize(
    min.delta = min(delta, na.rm=T),
    q1.delta = quantile(delta, .25, na.rm=T),
    avg.delta = mean(delta, na.rm=T),
    med.delta = median(delta, na.rm=T),
    q3.delta = quantile(delta, .75, na.rm=T),
    quan90.delta = quantile(delta, .90, na.rm=T),
    max.delta = max(delta, na.rm=T)
  )


join1 <- left_join(cancelled.flights, percent.cancelled, by = "carrier")
flights.7a <- left_join(join1, delta, by = "carrier")

# 9
dest_delay <- flights %>%
  group_by(dest) %>%
  summarize(
    arr_delay = mean(arr_delay),
    n = n()
  )
airports <- airports %>%
  select(dest=iata, name=airport, city, state, lat, long)


df.9a <- left_join(
  dest_delay,
  airports,
  by = "dest"
)


table <- df.9a %>%
  arrange(desc(arr_delay)) %>%
  head(5)

print(paste(
  table$city, 
  table$state
)
)

# 11 
#a
library(tidyr)
df <- data.frame(treatment = c("a", "b"), subject1 = c(3,4), subject2 = c(5,6))

df1 <- df %>%
  mutate(subject = c(1,1)) %>%
  select(subject, treatment, value = subject1)

df2 <- df %>%
  mutate(subject = c(2,2)) %>%
  select(subject, treatment, value = subject2)

df.11a <- full_join(
  df1,
  df2
)

print(df.11a)

# 11 
#b
df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)

df.11b <- df %>%
  spread(subject, value)

colnames(df.11b)[2] <- "subject1"
colnames(df.11b)[3] <- "subject2"

print(df.11b)

# 11 
#c
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)

df.11c <- df %>%
  separate(demo, c("sex", "age", "state"), remove=T)

print(df.11c)

df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)

df.11d <- df %>%
  unite(demo, sex, age, city, sep=".", remove=T)

df.11d$demo[4] <- NA

print(df.11d)
