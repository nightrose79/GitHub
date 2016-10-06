
library(dplyr)

library(ggplot2)

data()

df <- airquality

str(df)

glimpse(df)

filter(df, Month == 9, Day == 1)

filter(df, Month == 9 & Day == 1)

filter(df, !(Wind > 10 | Temp > 60))

head(filter(df, between(Wind, 10, 11)), 3)



head(df, 3)

# Remember to assign the result, dplyr doesn't overwrite data by default
newyearsday <- filter(df, Month == 1, Day == 1)

head(filter(df, Wind == 2 * 4), 3)

filter(df, Wind == (sqrt(2) ^ 2) * 4)

head(filter(df, near(Wind, (sqrt(2) ^ 2) * 4)), 3)

head(filter(df, is.na(Solar.R)), 3)

head(arrange(df, desc(Month), desc(Day), Temp), 3)

head(select(df, Solar.R, Temp) == select(df, c(Solar.R, Temp)), 5)

head(select(df, -(Month:Day)))

head(rename(df, Solar = Solar.R), 3)



head(
    mutate(df,
           knots = 0.868976 * Wind,
           celsius = (Temp - 32) * (5/9),
           kelvin = celsius + 273.15),
    3)

# transmute() works similarly but returns only the derived columns.
head(
    transmute(filter(df, !is.na(Ozone)),
              pct_max_ozone = Ozone / max(Ozone)),
    3)

filter(mutate(df, new_months = (Month != lag(Month))), new_months == TRUE)

by_month <- group_by(df, Month)
summarise(by_month,
          avg_temp = mean(Temp, na.rm = TRUE),
          avg_hot_temp = mean(Temp[Temp > 75], na.rm = TRUE),
          days = n())
# alternatively, sum(!is.na(x)) to count all non-NA values

# Pipelining operations with "then"
df %>%
group_by(Month) %>%
summarise(avg_temp = mean(Temp, na.rm = TRUE)) %>%
filter(avg_temp > 80)

df %>% summarise(n_distinct(Month))

df %>% group_by(Month, Day) %>% summarise(n()) %>% head(3)

df %>% group_by(Month, Day) %>% summarise(n()) %>% summarise(n()) %>% head(3)

df %>% group_by(Month, Day) %>% ungroup() %>% summarise(n()) %>% head(3)

# What question does this query answer?
df %>% 
  group_by(Month) %>%
  filter(rank(desc(Temp)) < 4)

# What question does this query answer?
df %>%
  group_by(Temp) %>%
  filter(n() > 10) %>%
  arrange(Month, Day)

month_nums <- df %>% distinct(Month) %>% arrange(Month)
print(month_nums)

month_names <- c('May', 'June', 'July', 'August', 'September')
months <- data.frame(month_nums, month_names)
glimpse(months)

joined_df <- inner_join(df, months)

joined_df %>% group_by(Month) %>% summarise(name = first(month_names))

joined_df %>%
  semi_join(joined_df %>% filter(Day == 31) %>% select(Day))

# summarise() only keeps necessary columns...
joined_df %>%
    group_by(Month) %>%
    summarise(mean_temp=mean(Temp)) %>% head

# ... therefore, add them back with a left_join
joined_df %>%
    group_by(Month) %>%
    summarise(mean_temp=mean(Temp)) %>%
    left_join(., months) %>%                     ## add back the month names 
    select(Month, month_names, mean_temp)        ## rearrange columns)

# or, use magrittr::extract2
joined_df %>% .$month_names

june <- df %>% filter(Month == 7) %>% select(Day, Temp, Wind)

ggplot(data = june, mapping = aes(x = Day, y = Temp)) +
  geom_point(aes(size = Wind), alpha = 1/3) +
  geom_smooth(se = FALSE)

temps = df %>% group_by(Temp) %>% summarise(ozone = mean(Ozone), rad = sd(Solar.R))

ggplot(data = temps, mapping = aes(x = Temp, y = ozone)) + 
  geom_point(aes(size = rad), alpha = 1/2)
