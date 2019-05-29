#______ Package + Data ______ 

install.packages('jsonlite')
library(DT)
library(jsonlite)
library(ggplot2)
library(dplyr)
library(skmeans)
library(gridExtra)
library(readr)
library(DataExplorer)
library(lubridate)
library(data.table)
library(lubridate)

## read the data
dataset <- fread('data_cluster.csv')

work_data <- dataset[,-6]
names(work_data)[c(3,4,5)] <- c('gun_type', 'gun_power', 'target')

glimpse(work_data)
dim(work_data)

# look for missing values 
options(repr.plot.width=8, repr.plot.height=3)
plot_missing(work_data)



datatable(work_data[1:1000, ])


#______ Exploratory Analysis ______ 

unique(work_data$user_id)


work_data %>%
  group_by(user_id) %>%
  summarise(qty=n()) %>%
  arrange(desc(qty))



# 1.1. Date
work_data$date <- as.character(work_data$date)
work_data$date <- as.Date(work_data$date)
work_data$dayOfWeek <- wday(work_data$date)
work_data$dayOfWeek <- as.factor(work_data$dayOfWeek)

options(repr.plot.width=8, repr.plot.height=3)
work_data %>%
  group_by(date) %>%
  summarise(qty = sum(target)/n()) %>%
  ggplot(aes(x = date, y = qty)) + 
         geom_line() + 
         geom_smooth(method = 'auto', se = FALSE) + 
         labs(x = 'Date', y = 'qty', title = 'qty')


options(repr.plot.width=8, repr.plot.height=3)
work_data %>%
  group_by(user_id) %>%
  summarise(qty = sum(target)/n(), date = max(date)) %>%
  ggplot(aes(x = date, y = qty)) + 
  geom_line() + 
  geom_smooth(method = 'auto', se = FALSE) + 
  labs(x = 'Date', y = 'qty', title = 'qty')

options(repr.plot.width=8, repr.plot.height=3)
work_data %>%
  group_by(user_id) %>%
  summarise(qty = n(), date = max(date)) %>%
  ggplot(aes(x = date, y = qty)) + 
  geom_line() + 
  geom_smooth(method = 'auto', se = FALSE) + 
  labs(x = 'Date', y = 'qty', title = 'qty')

options(repr.plot.width=8, repr.plot.height=3)
work_data %>%
  group_by(dayOfWeek) %>%
  summarise(qty = n()) %>%
  ggplot(aes(x = dayOfWeek, y = qty)) + 
  geom_line() + 
  geom_smooth(method = 'auto', se = FALSE) + 
  labs(x = 'Date', y = 'qty', title = 'qty')


work_data %>%
  group_by(dayOfWeek) %>%
  summarise(qty = sum(target)/n()) %>%
  ggplot(aes(x = dayOfWeek, y = qty)) + 
  geom_col() + 
  labs(x = 'Day of Week', 
       y = 'Revenue (£)', 
       title = 'Revenue by Day of Week')
