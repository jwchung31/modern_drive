install.packages("nycflights13")
install.packages("tidyverse")


library(nycflights13)
library(tidyverse)
# ----------------------------


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


flights <- flights %>% 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours
  )
str(flights)

freq_dest <- flights %>% 
  group_by(dest) %>% 
  summarize(num_flights = n())
freq_dest

freq_dest %>% 
  arrange(num_flights)

# -------------

install.packages("fivethirtyeight")
library(fivethirtyeight)

drinks_smaller <- drinks %>% 
  filter(country %in% c("USA", "China", "Italy", "Saudi Arabia")) %>% 
  select(-total_litres_of_pure_alcohol) %>% 
  rename(beer = beer_servings, spirit = spirit_servings, wine = wine_servings)
drinks_smaller

drinks_smaller_tidy <- drinks_smaller %>%
  pivot_longer(names_to = "type",
               values_to = "servings",
               cols = -country)
drinks_smaller_tidy

# ----- pivot_* exercise

relig_income
str(relig_income)

long_data <- pivot_longer(data = relig_income,
                          cols = -religion,
                          names_to = 'annual_income',
                          values_to = 'freq')
long_data

wide_data <- pivot_wider(data = long_data,
                         names_from = annual_income,
                         values_from = freq)
wide_data

# -----  

library(readr)
kbo <- read.csv("kbo.csv") %>% as_tibble()

temp <- lm(타석당득점 ~ 타율, data = kbo) %>%
  summary() %>% 
  .$r.squared

a <- c(1, 2, 3, 4, 5)
map_dbl(a, ~.x+1)

b <- c(1, 22, 333, 4444, 55555)
map_int(b, str_length)
map_lgl(b, is.numeric)

c <- c('abc', 'def', 'ghi')
map_chr(c, ~paste0(.x, 'z'))

d <- c(5, 4, 3, 2, 1)
map2_dbl(a, d, sum)

pmap_dbl(list(a, b, d), ~..2-..1+..3)
list(a, b, d)


f <- tibble(a=c(17, 23, 4, 10, 11), 
            b=c(24, 5, 6, 12, 18), 
            c=c(1, 7, 13, 19, 25), 
            d=c(8, 14, 20, 21, 2), 
            e=c(15, 16, 22, 3, 9))
f %>% 
  mutate(sum = a + b + c + d)

# 행에서 
f %>% 
  rowwise() %>% 
  mutate(max = max(a, b, c, d, e))

# 열에서 
map_df(f, sum)

map_df(f, ~.x+1)

# --- returning 야구데이터

kbo %>% 
  select(타율, 출루율, 장타력, OPS) %>% 
  map(~lm(kbo$타석당득점 ~ .x)) %>% 
  map(summary) %>% 
  map_df('r.squared')

kbo %>% 
  select(타율, 출루율, 장타력, OPS) %>% 
  map_df(~lm(kbo$타석당득점 ~ .x) %>% 
  summary() %>% 
  .$r.squared)

kbo %>% 
  pivot_longer(cols = 타율:OPS, names_to = '기록', values_to = '값') %>% 
  group_by(기록) %>% 
  nest() -> kbo2


kbo2$data %>% 
  set_names(., kbo2$기록) %>% 
  map_df(~lm(타석당득점 ~ 값, data = .) %>% 
           summary() %>% 
           .$r.squared)

kbo %>% 
  pivot_longer(cols = 타율:OPS, names_to = '기록', values_to = '값') %>% 
  group_by(기록) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(타석당득점 ~ 값, data = .) %>% 
                       summary() %>% 
                       .$r.squared)) %>% 
  unnest(model) %>% 
  select(-data)

# kbo %>% 
#   pivot_longer(cols = 타율:OPS, names_to = '기록', values_to = '값') %>% 
#   group_by(X10년대, 기록) %>% 
#   nest() %>% 
#   mutate(model = map(data, ~lm(타석당득점 ~ 값, data = .) %>% 
#                        summary() %>% 
#                        .$r.squared)) %>% 
#   unnest(model) %>% 
#   select(-data) %>% 
#   pivot_wider(names_from = 기록, values_from = model)

kbo %>% 
  pivot_longer(cols = 타율:OPS, names_to = '기록', values_to = '값') %>% 
  group_by(X10년대, 기록) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(타석당득점 ~ 값, data = .) %>% 
                       summary() %>% 
                       .$r.squared)) %>% 
  unnest(model) %>% 
  select(-data) %>% 
  ggplot(aes(X10년대, model, fill=기록)) +
  geom_bar(stat='identity', position=position_dodge2(reverse=TRUE)) +
  scale_fill_viridis_d()
