library(readxl)
raw_data <- read_excel("~/Downloads/Data_Level5_BAH_OceanCleanup.xlsx")
library(plotly)
library(dplyr)
library(tidyverse)


plot(x = raw_data$State, y = )



df = raw_data[, -1] # remove the ID column
df = unique(df) # get unique rows
write.csv(df, "~/Downloads/clean1.csv") # export csv

# import cleaned data
df <- read_csv("~/Documents/GitHub/OceanCleanup/Cleaned/clean3.csv", 
                   col_types = cols(Adults = col_integer(), 
                                    Children = col_integer(), `Group Name` = col_character(), 
                                    People = col_integer()))

### convert data type
df[, 11:60] <- sapply(df[, 11:60], as.integer)
sapply(df, class) # view the data type 
write.csv(df, "~/Documents/GitHub/OceanCleanup/Cleaned/clean4.csv") # export csv



df %>% 
  select(FL_DATE, CARRIER, ORIGIN, ORIGIN_CITY_NAME, ORIGIN_STATE_ABR, DEP_DELAY, DEP_TIME, ARR_DELAY, ARR_TIME)



