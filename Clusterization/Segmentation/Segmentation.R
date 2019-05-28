setwd("C:/Users/EASokol/Desktop/loan")

# https://www.kaggle.com/alves9/rfm-customer-segmentation

# https://www.kaggle.com/chrisbow/e-commerce-eda-and-segmentation-with-r/data


install.packages("DataExplorer")
## Importing packages
library(ggplot2)
library(dplyr)
library(skmeans)
library(gridExtra)
library(readr)
library(DataExplorer)
library(lubridate)

## READ THE DATA
custData <- read.csv('data.csv')
custData$InvoiceDate <- as.character(custData$InvoiceDate)

length(unique(custData$CustomerID))



options(repr.plot.width=8, repr.plot.height=3)
# look for missing values using the DataExplorer package
plot_missing(custData)

custData <- na.omit(custData)
dim(custData)

# separate date and time components of invoice date
custData$date <- sapply(custData$InvoiceDate, 
                        FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})

custData$time <- sapply(custData$InvoiceDate, 
                        FUN = function(x) {strsplit(x, split = '[ ]')[[1]][2]})

head(custData)

# create month, year and hour of day variables
custData$month <- sapply(custData$date,
                         FUN = function(x) {strsplit(x, split = '[/]')[[1]][1]})
custData$year <- sapply(custData$date, 
                        FUN = function(x) {strsplit(x, split = '[/]')[[1]][3]})
custData$hourOfDay <- sapply(custData$time, 
                             FUN = function(x) {strsplit(x, split = '[:]')[[1]][1]})


head(custData, n =5)

custData$date <- as.Date(custData$date, "%m/%d/%Y")

custData$dayOfWeek <- wday(custData$date, label=TRUE)



custData <- custData %>% mutate(lineTotal = Quantity * UnitPrice)


custData$Country <- as.factor(custData$Country)
custData$month <- as.factor(custData$month)
custData$year <- as.factor(custData$year)
levels(custData$year) <- c(2010,2011)
custData$hourOfDay <- as.factor(custData$hourOfDay)
custData$dayOfWeek <- as.factor(custData$dayOfWeek)



options(repr.plot.width=8, repr.plot.height=3)
custData %>%
  group_by(date) %>%
  summarise(revenue = sum(lineTotal)) %>%
  ggplot(aes(x = date, y = revenue)) + 
  geom_line() + 
  geom_smooth(method = 'auto', se = FALSE) + 
  labs(x = 'Date', y = 'Revenue (£)', title = 'Revenue by Date')


# Day of week analysis


custData %>%
  group_by(dayOfWeek) %>%
  summarise(revenue = sum(lineTotal)) %>%
  ggplot(aes(x = dayOfWeek, y = revenue)) + 
  geom_col() + 
  labs(x = 'Day of Week', y = 'Revenue (£)', title = 'Revenue by Day of Week')


weekdaySummary <- custData %>%
  group_by(date, dayOfWeek) %>%
  summarise(revenue = sum(lineTotal), transactions = n_distinct(InvoiceNo)) %>%
  mutate(aveOrdVal = (round((revenue / transactions),2))) %>%
  ungroup()

head(weekdaySummary, n = 10)


ggplot(weekdaySummary, aes(x = dayOfWeek, y = revenue)) + 
  geom_boxplot() + labs(x = 'Day of the Week', y = 'Revenue',
                        title = 'Revenue by Day of the Week')

ggplot(weekdaySummary, aes(x = dayOfWeek, y = transactions)) + 
  geom_boxplot() + labs(x = 'Day of the Week', y = 'Number of Daily Transactions', 
                        title = 'Number of Transactions by Day of the Week')

ggplot(weekdaySummary, aes(x = dayOfWeek, y = aveOrdVal)) + 
  geom_boxplot() + labs(x = 'Day of the Week', y = 'Average Order Value', 
                        title = 'Average Order Value by Day of the Week')



ggplot(weekdaySummary, aes(transactions, fill = dayOfWeek)) + 
  geom_density(alpha = 0.2)



kruskal.test(transactions ~ dayOfWeek, data = weekdaySummary)
