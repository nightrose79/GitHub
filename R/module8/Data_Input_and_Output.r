
#library(readr)
#library(haven)
#library(readxl)

library(dplyr)
library(readxl)

# First, you may open the file in Excel to see what it looks like
# Here, we skip the first 4 rows in order to extract only the data table
summary_excel <- read_excel("small_data/Medicare_Charge_Inpatient_DRGALL_DRG_Summary_Reports_FY2014.xlsx",
                            col_names=TRUE,            # true by default
                            col_types=NULL,            # guess based on data (default) 
                            sheet="Averages_by_DRG",   # could be an integer
                            na="",                     # change as needed
                            skip=4) 

summary_excel %>% names

library(DBI)
