library(readxl)
raw_data <- read_excel("~/Downloads/Data_Level5_BAH_OceanCleanup.xlsx")
library(plotly)
library(dplyr)
library(tidyverse)
library(ggplot2)

plot(x = raw_data$State, y = )



# df = raw_data[, -1] # remove the ID column
# df = unique(df) # get unique rows
# write.csv(df, "~/Documents/GitHub/OceanCleanup/Cleaned/clean6.csv", row.names=FALSE) # export csv

# import cleaned data
# df <- read_csv("~/Documents/GitHub/OceanCleanup/Cleaned/clean3.csv", 
                  # col_types = cols(Adults = col_integer(), 
                                  #  Children = col_integer(), `Group Name` = col_character(), 
                                   # People = col_integer()))

clean6 <- read_csv("~/Documents/GitHub/OceanCleanup/Cleaned/clean6.csv", 
                   col_types = cols(`Group Name` = col_character()))
df <- clean6



### convert data type
df[, 11:60] <- sapply(df[, 11:60], as.integer)
sapply(df, class) # view the data type 




df %>%
  select(Year<=2014)

# 
df %>% 
  select(Zone, Country)

df %>% 


# Plots
boxplot(df$`Total Items Collected`)
barplot(df$State, df$`Total Items Collected`)

ggplot(data = df) +
  geom_bar(mapping = aes(x = Zone))
