##Bei Hai Assignment 5
#1
#a
library(foreign)
install.packages("ggplot2")
library(ggplot2)
ggplot(data = diamonds,
       aes(x = x*y*z, y = price,size = carat, colour = clarity),
       scale_x_log10(),
       scale_y_log10()
) +
  geom_point()

#b
ggplot(diamonds, aes(x = carat, y = ..density.., fill = clarity)) +
  geom_histogram() +
  facet_grid("cut~.")

#c
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_violin() +
  geom_jitter(alpha = 0.02)

#3
#a
library(dplyr)
library(foreign)
df<- read.dta("https://github.com/EconomiCurtis/econ294_2015/blob/master/data/org_example.dta")

df<- df %>%
  group_by(year, month) %>%
  mutate(
    median.rw = median(rw),
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  ) %>%
  filter(!is.na(rw)) %>%
  tbl_df()


ggplot(df, aes(date, median.rw)) +
  geom_ribbon(alpha = 0.1, aes(ymin = quantile(rw, 0.10), ymax = quantile(rw, 0.90))) +
  geom_ribbon(aes(ymin = quantile(rw, 0.25), ymax = quantile(rw, 0.75))) +
  geom_line()


#b
df <- df %>%
  filter(!is.na(rw)) %>%
  group_by(educ, year, month) %>%
  mutate(
    median.rw = median(rw),
    date = paste(year, month, "01", sep = "-"),
    date = as.Date(date, format = "%Y-%m-%d")
  ) %>%
  tbl_df()