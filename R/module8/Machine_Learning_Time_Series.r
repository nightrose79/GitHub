
# essentials
library(dplyr)
library(magrittr)
library(ggplot2)

# best package for dates
# https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html
library(lubridate)

google_stock <- readRDS("small_data/google_stock.RDS")

google_stock$Date %<>% ymd

google_stock %>% glimpse

# easy filter with dplyr
# note: jupyter seems to have an issue with `scale_x_date()`
google_stock %>%
    filter(Date > "2015-01-01") %>%
    qplot(Date, Open, data=.) # + scale_x_date()

#qplot(Date, Open, data=google_stock) + geom_line() + scale_x_date()

library(zoo)

# convert to zoo object
google <- google_stock %>%
    select(Date, Open, Close, Volume) %>% as.data.frame %>% read.zoo 

# use as.data.frame only for printing
google %>% as.data.frame %>% head

# automatic paneling with zoo object
plot(google)

plot(google %>% log %>% diff)

google$Open %>% as.data.frame %>% head

# lag 2 values back
lag(google$Open, 2) %>% as.data.frame %>% head

## apply mean over a window of 2 time points
rollapply(google, 2, mean) %>% as.data.frame %>% head

# options: align and fill 
rollapply(google, 2, mean, align="right") %>% as.data.frame %>% head
rollapply(google, 2, mean, fill=NA, align = "right")%>% as.data.frame %>% head

# more efficient implementation of common functions
# rollmean(), rollmedian(), rollmax()
rollmax(google, 2, align="right", fill=NA) %>% as.data.frame %>% head

library(broom)
library(dynlm)

dynlm(Open ~ lag(Open, 1), data=google) %>% summary

# tidy output with broom!
dynlm(Open ~ lag(Open, 1), data=google) %>% tidy

dynlm(Open ~ lag(Open, 1), data=google) %>% summary %>% glance

# add more predictors
dynlm(Open ~ lag(Open, 1) + lag(Volume, 2), data=google) %>% summary
