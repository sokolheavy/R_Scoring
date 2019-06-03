setwd("F:/scoring/R_cases/job_task_1")

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
library(openxlsx)
library(agricolae)

## read the data
dataset <- fread('data_cluster.csv', header=T)

work_data <- dataset[,-6]
names(work_data)[c(3,4,5)] <- c('gun_type', 'gun_power', 'target')

glimpse(work_data)
dim(work_data)

# look for missing values 
options(repr.plot.width=8, repr.plot.height=3)
plot_missing(work_data)



datatable(work_data[1:1000, ])


#______ Exploratory Analysis ______ 

work_data <- dataset[,-6]
names(work_data)[c(3,4,5)] <- c('gun_type', 'gun_power', 'target')

dist_userset <- work_data %>%
      group_by(user_id) %>%
      summarise(qty=n(), 
                proc_win = sum(target)/n(),
                qty_win = sum(target)) %>%
      arrange(desc(qty))



## Analyze date variable
work_data$date <- gsub("\\.", "-",work_data$date)
work_data$date <- as.Date(work_data$date, format="%d-%m-%Y")
work_data$dayOfWeek <- wday(work_data$date)
work_data$dayOfWeek <- as.factor(work_data$dayOfWeek)

# Days activity
work_data %>%
  group_by(date) %>%
  summarise(qty = n()) %>%
  ggplot(aes(x = date, y = qty, group = 1)) + 
  geom_line() + 
  geom_smooth(method = 'loess', se = FALSE) + 
  labs(x = 'Date', y = 'qty', title = 'Activity by days')
# It appears as though trends of win are descending
# so that's a bad sign, but that doesn't really generate any actionable insight, so let's dive into the data a bit farther


# DayOfaWeek activity
work_data %>%
  group_by(dayOfWeek) %>%
  summarise(qty = n()) %>%
  ggplot(aes(x = dayOfWeek, y = qty, group = 1)) + 
  geom_line() + 
  geom_smooth(method = 'loess', se = FALSE) + 
  labs(x = 'dayOfWeek', y = 'qty', title = 'Activity by dayOfWeek')


# So we have some specific differences
# Try to know activity by user_id
day_max_activity_user <- 
  work_data %>%
  group_by(user_id) %>%
  summarise(mostDay = names(which.max(table(dayOfWeek))))


# DayOfaWeek activity by users
day_max_activity_user %>%
  group_by(mostDay) %>%
  summarise(qty = n()) %>%
  ggplot(aes(x = mostDay, y = qty, group = 1)) + 
  geom_line() + 
  geom_smooth(method = 'loess', se = FALSE) + 
  labs(x = 'mostDay', y = 'qty', title = 'DayOfaWeek activity by users')

# Trend of plot is changed. It`s not surprised, becouse we take only one day, that user visited the most.
day_max_activity_user %>%
  group_by(mostDay) %>%
  summarise(qty = n()) 

# Mon and Tue have similar distribution - union in one group
# Thu, Fri, Sat have also similar distribution - union in one group

day_max_activity_user2 <- 
  day_max_activity_user %>%
  mutate(mostDay = ifelse(mostDay%in% c("1", "2"), "1",
                          ifelse(mostDay=="3", "2", 
                                 ifelse(mostDay%in% c("4", "5", "6"), "3", "4"))))


# join variable to final dataset
dist_userset <- left_join(dist_userset, day_max_activity_user2, by = "user_id")



## Analyze gun_power variable
 
  gun_power_summary <- work_data %>%
  group_by(gun_power) %>%
  summarise(qty = n(),
            proc = n()/nrow(work_data),
            proc_win = sum(target)/n()) %>%
  ungroup() %>%
  arrange(desc(proc, proc_win))
  
# ~ 80% data in {0, 1} 
  
# For plot take type of gun_power, that consists enough amount of data(percentage of all data>0.0005)) 
gun_power_summary %>%
  filter(proc>0.0005) %>%
  ggplot(aes(gun_power, proc_win)) +
  geom_line() + 
  geom_smooth(method = 'loess', se = FALSE) + 
  labs(x = 'gun_power', y = 'proc_win', title = 'Connection of gun_power with proc_win')

# We have 3 type of gun_power: {0, 1, else},
# but being seen trend`on data, so better to normalize variable 

gun_power_summary <- work_data %>%
  group_by(gun_power) %>%
  summarise(qty = n(),
            proc = n()/nrow(work_data),
            proc_win = sum(target)/n()) %>%
  mutate(gun_power_sc = scale(gun_power, center = FALSE))  %>%
  ungroup() %>%
  arrange(desc(proc, proc_win))

# join variable to final dataset
dist_userset <- 
            left_join(dist_userset, work_data[,c("user_id", "gun_power")], by = "user_id") %>%
            left_join(gun_power_summary[,c("gun_power", "gun_power_sc")], by = "gun_power") %>%
            select(-gun_power) %>%
            group_by(user_id, qty, proc_win, qty_win, mostDay) %>%
            summarise(avg_gun_power = mean(gun_power_sc))



## Analyze gun_type variable


gun_type_summary <- work_data %>%
  group_by(gun_type) %>%
  summarise(qty = n(),
            proc = n()/nrow(work_data),
            proc_win = sum(target)/n(),
            avg_gun_power = mean(scale(gun_power, center = F)))%>%
  ungroup() %>%
  arrange(desc(proc, proc_win))

unique(work_data[,3])

# so we have a lot of type of gun(2592), try to bin gun_type in some group

gun_type_summary %>%
  filter(proc>0.001) %>%
  ggplot(aes(proc, proc_win)) + 
  geom_line() + 
  geom_smooth(method = 'loess', se = FALSE) + 
  labs(x = 'mostDay', y = 'qty', title = 'DayOfaWeek activity by users')


gun_type_summary %>%
  filter(proc>0.001) %>%
  ggplot(aes(proc, avg_gun_power)) + 
  geom_line() + 
  geom_smooth(method = 'loess', se = FALSE) + 
  labs(x = 'mostDay', y = 'qty', title = 'DayOfaWeek activity by users')


# It seems that there`re relation beetwen proc and avg_gun_power
# Try to know if distribution of gun_power is different in each gun_type
# Gun_power`s distribution have to be skewed, so use non-parametric test to look for statistically significant differences in type of gun

kruskal.test(gun_power ~ gun_type, data = work_data)

# Looks like a significant, p-value<0,05. 
# gun_power can be cuted: {0, 1, else}

gun_type_by_power <- 
  work_data %>%
  group_by(gun_type) %>%
  summarise(mostPower = ifelse(!names(which.max(table(gun_power)))%in% c("0", "1"),
                               "2", names(which.max(table(gun_power)))))

prop.table(table(gun_type_by_power$mostPower))
# We have differences in the type of gun in gun_power

# It certainly seems that also we have differences in the qty of each type of gun

gun_type_summary <- gun_type_summary %>%
                    mutate(gun_class = ifelse(proc>0.003, "Top", 
                            ifelse(proc>0.0005,"Medium", "Small")))

prop.table(table(gun_type_summary$gun_class))


left_join(work_data, gun_type_summary[ ,c("gun_type", "gun_class")], by = "gun_type") %>%
  group_by(gun_class) %>%
  summarise(avg_proc_win = sum(target)/n(),
            avg_gun_power = mean(gun_power))
  

# join variable to final dataset 
dist_userset <- 
left_join(work_data[, c("gun_type", "user_id")],  gun_type_summary[, c("gun_type", "gun_class")], by = "gun_type") %>%
  left_join(dist_userset, by = "user_id") %>%
  group_by(user_id, qty, proc_win, qty_win, mostDay, avg_gun_power) %>%
  summarise(mostGunClass = names(which.max(table(gun_class))))

prop.table(table(dist_userset$mostGunClass))


get_difference <- function(test_data, n_cluster){    
  dist_matrix <- dist(test_data)    
  fit <- hclust(dist_matrix)    
  test_data$cluster <- as.factor(cutree(fit, n_cluster))    
  p_val <- sapply(test_data[,-ncol(test_data)],    
                  function(x) {summary(aov(x~cluster, test_data))[[1]][1,'Pr(>F)']})    
  return(names(p_val)[p_val < 0.05])    
}

get_difference(dist_userset[,-1], 5)

gc()

dist_matrix <- dist(dist_userset)    
fit <- hclust(dist_matrix)    
test_data$cluster <- as.factor(cutree(fit, n_cluster))    
p_val <- sapply(test_data[,-ncol(test_data)],    
                function(x) {summary(aov(x~cluster, test_data))[[1]][1,'Pr(>F)']})    
return(names(p_val)[p_val < 0.05])    
