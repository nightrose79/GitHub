
library(dplyr)
library(ggplot2)

library(stringr)

string1 <- "Hello world"
string2 <- "I can't help but use contractions"
string3 <- 'Don\'t mind me, just making a quick escape'

writeLines(string3)

str_length(string3)

str_c("Prognosis", "Negative", sep = ": ")

str_c(c(string1, string2, string3), collapse = " & ")

str_sort(c(string1, string2, string3))

str_sub(string3, 21, 26)

str_trim(str_c(string1, "    "))

data()

df <- data.frame(VADeaths)
str(df)

head(df)

df <- df %>% mutate(age_range = rownames(df))
df

str(df)

ggplot(df) +
  geom_point(aes(x = age_range, y = Urban.Male))

# Define age_range as a factor with levels, then replot
levels <- df$age_range %>% sort(decreasing = TRUE)
print(levels)

df$age_range <- factor(df$age_range, levels = levels)

library(tidyr)

messy <- data.frame(
  name = c("Wilbur", "Petunia", "Gregory"),
  a = c("50/67", "25/80", "10/64"),
  b = c("50/56", "25/90", "10/50")
)
messy

gathered <- messy %>%
  gather(a:b, key = drug, value = measurements)
gathered

str(messy)
str(gathered)

# The complement of gather() is spread()
gathered %>% spread(drug, measurements)

gathered %>% spread(measurements,drug)

preg <- frame_data(
  ~pregnant, ~male, ~female,
  "yes",     NA,    10,
  "no",      20,    12
)

str(preg)

# If columns contain more than one observation, use separate()
separated <- gathered %>% 
  separate(measurements, into = c('dosage', 'heartrate'))
separated

sales <- data.frame(
    rep = c('Jim', 'Dwight', 'Stanley'),
    order = c('1998 ## 43,961 ## Malaysia', '1999 ## 22,309 ## Scotland', '2001 ## 78,230 ## Chile')
    )
sales

# Your code here
separated_sales <- sales %>% 
  separate(order, into = c('year', 'amount','country'),sep=('##'),convert=TRUE)
separated_sales

?separate

# unite() is the complement of separate()
separated %>% unite(injection, dosage, drug, sep = "mL ")

stocks <- data_frame(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  rev    = runif(7, min=0, max=5)
)
stocks[4, 'rev'] <- NA
stocks

stocks %>% 
  spread(year, rev)

stocks %>%
  spread(year, rev) %>%
  gather(year, rev, `2015`:`2016`, na.rm = TRUE)

stocks %>% complete(year, qtr)

stocks %>% fill(rev)

library(xml2)

# example 
catalog <- read_xml("http://www.xmlfiles.com/examples/cd_catalog.xml")
xml_url(catalog)

catalog

# print entire tree
 xml_structure(catalog) 

# extract parent tag
xml_name(catalog) %>% head

# extract children
xml_children(catalog) %>% head

# easily parse out all the text...
xml_text(catalog) %>% head

# ... or individual nodes (using an XPath expression)...
xml_find_all(catalog, ".//TITLE") %>% head

# ... and possibly just the node text
xml_find_all(catalog, ".//TITLE") %>% xml_text %>% head

# look within nested nodes (again, with an XPath expression)
xml_find_all(catalog, "CD/TITLE") %>% xml_text %>% head

# debug 
xml_find_all(catalog, ".//TITLE") %>% xml_type %>% unique

# another example: extract all countries
xml_find_all(catalog, ".//COUNTRY") %>% xml_text 
