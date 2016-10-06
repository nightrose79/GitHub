
library(dplyr)
library(ggplot2)

str(anscombe)

summary(anscombe)

ggplot(data = anscombe) +
  geom_point(aes(x = x1, y = y1), colour = "red") +
  geom_point(aes(x = x2, y = y2), colour = "blue") +
  geom_point(aes(x = x3, y = y3), colour = "green") +
  geom_point(aes(x = x4, y = y4), colour = "black")

library(nycflights13)

str(flights)

ggplot(data = flights) +
  geom_bar(mapping = aes(x = carrier))

ggplot(data = flights) +
  geom_histogram(mapping = aes(x = distance), binwidth = 100)

# Use geom.freqpoly if you need to overlay multiple histograms
# Why do we have to use factor(month)?
ggplot(data = flights) +
  geom_freqpoly(mapping = aes(x = distance, color = factor(month)), binwidth = 100)

ggplot(data = flights) +
  geom_freqpoly(mapping = aes(x = distance, color = factor(month)), binwidth = 100) +
  coord_cartesian(xlim = c(2400, 2600), ylim = c(1000, 2000))

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)



flights %>% 
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>% 
  ggplot(mapping = aes(sched_dep_time)) + 
    geom_freqpoly(mapping = aes(colour = cancelled), binwidth = 1/4)

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy))

ggplot(data = mpg) +
  geom_boxplot(mapping = aes(x = reorder(class, hwy, FUN = median), y = hwy)) +
  coord_flip()



ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))

diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
    geom_tile(mapping = aes(fill = n))

ggplot(data = flights) + 
  geom_point(mapping = aes(x = dep_delay, y = arr_delay))

ggplot(data = flights) +
  geom_bin2d(mapping = aes(x = arr_delay, y = dep_delay))



library(lattice)

str(mtcars)

# create factors with value labels 
gear_f <- factor(mtcars$gear, levels = c(3,4,5), labels = c("3_gears","4_gears","5_gears")) 
cyl_f <- factor(mtcars$cyl, levels = c(4,6,8), labels=c("4_cyl","6_cyl","8_cyl")) 

mtcars <- mtcars %>%
    mutate(gear_f = gear_f,
           cyl_f = cyl_f)

mtcars %>% head(3)

splom(mtcars[c(1,3,4,5,6)], main="MTCARS Data")

xyplot(mpg~wt, data = mtcars)

barchart(mpg~wt,
         data = mtcars)

xyplot(mpg~wt|gear_f, data = mtcars)

xyplot(mpg~wt|cyl_f*gear_f,
    data = mtcars,
    main = "Every combination of conditioning vars",
    ylab = "MPG", xlab = "Weight")

xyplot(mpg~wt, data = mtcars,
       groups = cyl_f,
       auto.key = list(columns = 3),
       type=c("p","g"))

histogram(~mpg|cyl_f,
          data = mtcars)

bwplot(~hp|cyl_f,
       data = mtcars)

densityplot(~mpg,
            data = mtcars)

densityplot(~mpg|cyl_f,
            data = mtcars,
            layout=c(1,3))

help(lattice)

levelplot(mpg~wt*hp,
          data = mtcars)

cloud(mpg~cyl_f*gear_f,
      data = mtcars)




