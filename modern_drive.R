install.packages("nycflights13")
install.packages("dplyr")
install.packages("knitr")
install.packages("ggplot2")

library(nycflights13)
library(dplyr)
library(knitr)
library(ggplot2)

glimpse(flights)
str(flights)
kable(airlines)

alaska_flights <- flights %>% 
  filter(carrier == "AS")

ggplot(data = alaska_flights, mapping = aes(x = dep_delay, y = arr_delay)) + 
  geom_point()

early_january_weather <- weather %>% 
  filter(origin == "EWR" & month == 1 & day <= 15)

ggplot(data = weather, mapping = aes(x = temp)) +
  geom_histogram(color = "white")

ggplot(data = weather, mapping = aes(x = temp)) +
  geom_bar(color = "white")

str(weather)
ggplot(data = weather, mapping = aes(x = factor(month), y = temp)) +
  geom_boxplot()

ggplot(data = weather, mapping = aes(x = factor(month), y = temp)) +
  geom_boxplot()

ggplot(data = weather, mapping = aes(y = temp)) +
  geom_boxplot() +
  facet_wrap(~ month, nrow = 1)

fruits <- tibble(
  fruit = c("apple", "apple", "orange", "apple", "orange")
)
fruits_counted <- tibble(
  fruit = c("apple", "orange"),
  number = c(3, 2)
)

ggplot(data = fruits, mapping = aes(x = fruit)) +
  geom_bar()

ggplot(data = fruits_counted, mapping = aes(x = fruit, y = number)) +
  geom_col()

ggplot(data = flights, mapping = aes(x = carrier)) +
  geom_bar()

ggplot(data = flights, mapping = aes(x = carrier)) + 
  geom_bar()

ggplot(data = flights, mapping = aes(x = carrier, fill = origin)) +
  geom_bar()

ggplot(data = flights, mapping = aes(x = carrier, color = origin)) +
  geom_bar()

ggplot(data = flights, mapping = aes(x = carrier, fill = origin)) +
  geom_bar()

ggplot(data = flights, mapping = aes(x = carrier)) +
  geom_bar() +
  facet_wrap(~origin, ncol = 1)

ggplot(data = flights, mapping = aes(x = carrier, fill = origin)) +
  geom_bar(position = "dodge")

ggplot(data = flights, mapping = aes(x = carrier, fill = origin)) +
  geom_bar(position = "stack")

ggplot(data = flights, mapping = aes(x = carrier, fill = origin)) +
  geom_bar(position = position_dodge(preserve = "single"))

glimpse(flights)
View(flights)

alaska_flights <- flights %>% 
  filter(carrier == "AS")
alaska_flights

flights_time_new <- flights %>% 
  select(dep_time, arr_time) %>% 
  rename(departure_time = dep_time, arrival_time = arr_time)
glimpse(flights_time_new)

flights %>% top_n(n = 10, wt = year)
flights

summary_temp <- weather %>% 
  summarize(mean(temp),sd(temp))
summary_temp

summary_temp <- weather %>% 
  summarize(mean(temp, na.rm = TRUE), 
            sd(temp, na.rm = TRUE))


summary_temp <- weather %>% 
  summarize(mean = mean(temp, na.rm = TRUE))
summary_temp

summary_temp <- weather %>%   
  summarize(mean = mean(temp, na.rm = TRUE)) %>% 
  summarize(std_dev = sd(temp, na.rm = TRUE))

weather

summary_monthly_temp <- weather %>% 
  group_by(month) %>% 
  summarize(mean = mean(temp, na.rm = TRUE), 
            std_dev = sd(temp, na.rm = TRUE)) %>%
summary_monthly_temp

diamonds %>% 
  group_by(cut) %>% 
  summarize(avg_price = mean(price))

diamonds %>% 
  group_by(cut) %>% 
  ungroup()

by_origin_monthly <- flights %>% 
  group_by(origin, month) %>% 
  summarize(count = n())
by_origin_monthly
