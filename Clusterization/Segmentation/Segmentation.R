setwd("F:/scoring/R_cases/job_task_1")

#______ Package + Data ______ 
install.packages("PCAmixdata")

# Data manipulation & visualization
library(dplyr)
library(lubridate)
library(data.table)
library(DT)
library(ggplot2)
library(gridExtra)
library(DataExplorer)
library(knitr)
library(data.table)

 #clusterization
library(cluster)
library(StatMatch)
library(doParallel)


## read the data
dataset <- fread('data_cluster.csv', header=T)

work_data <- dataset[,-6]
names(work_data)[c(3,4,5)] <- c('gun_type', 'gun_power', 'target')

glimpse(work_data)
dim(work_data)

# look for missing values 
options(repr.plot.width=8, repr.plot.height=3)
plot_missing(work_data)



#______ Exploratory Analysis ______ 

work_data <- dataset[,-6]
names(work_data)[c(3,4,5)] <- c('gun_type', 'gun_power', 'target')

dist_userset <- work_data %>%
      group_by(user_id) %>%
      summarise(qty=n(), 
                proc_win = sum(target)/n()) %>%
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

png(filename="data_cluster.png", res=150, width = 1000, height = 1000)
plot(data_cluster)
dev.off()




rm(dataset)
rm(day_max_activity_user)
rm(day_max_activity_user2)
rm(gun_power_summary)
rm(gun_type_by_power)
rm(gun_type_summary)
rm(work_data)
gc()


### Clustering 

write.csv2(dist_userset, file = 'dist_userset.csv')
# dist_userset <- read.csv2('dist_userset.csv')
data_cluster <- as.data.frame(dist_userset[,-c(5)])

# All variables should have 'factor' type, so convert variables, that not is a 'factor', to 'factor' 
data_cluster$mostDay <- as.factor(data_cluster$mostDay)
data_cluster$mostGunClass <- as.factor(data_cluster$mostGunClass)

set.seed(123)
data_cluster_subset <- data_cluster[sort(sample(nrow(data_cluster), nrow(data_cluster)*.1)),-1] # 10% here

train <- data_cluster_subset[,-1]

# Data preparation
for(i in 2:length(colnames(train))) {
  if(class(train[,i]) == "numeric" | class(train[,i]) == "integer") {
    train[,i] <- as.vector(scale(train[,i])) }
}


# Function to parallize the computation of distance-matrices
computeDistance <- function(dt1, dt2, nThreads = 4) {
  # Determine chunk-size to be processed by different threads
  s <- floor(nrow(dt1) / nThreads)
  
  # Setup multi-threading
  modelRunner <- makeCluster(nThreads)
  registerDoParallel(modelRunner)
  
  # For numeric variables, build ranges (max-min) to be used in gower-distance.
  # Ensure that the ranges is computed on the overall data and not on
  # the chunks in the parallel threads. Also, note that function 'gower.dist()'
  # seems to be buggy in regards to missing values (NA), which can be fixed by
  # providing ranges for all numeric variables in the function-call
  dt <- rbind(dt1, dt2)
  rngs <- rep(NA, ncol(dt))
  for (i in 1:ncol(dt)) {
    col <- dt[[i]]
    if (is.numeric(col)) {
      rngs[i] <- max(col, na.rm = T) - min(col, na.rm = T)
    }
  }
  
  # Compute distance in parallel threads; note that you have to include packages
  # which must be available in the different threads
  distanceMatrix <-
    foreach(
      i = 1:nThreads, .packages = c("StatMatch"), .combine = "rbind",
      .export = "computeDistance", .inorder = TRUE
    ) %dopar% {
      # Compute chunks
      from <- (i - 1) * s + 1
      to <- i * s
      if (i == nThreads) {
        to <- nrow(dt1)
      }
      
      # Compute distance-matrix for each chunk
      distanceMatrix <- gower.dist(dt1[from:to,], dt2, rngs = rngs)
    }
  
  # Clean-up
  stopCluster(modelRunner)
  return(distanceMatrix)
}



dist_Matrix <- computeDistance(train, train)

gower_mat<-as.matrix(dist_Matrix)


# Output most similar pair
train[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

# Output most dissimilar pair
train[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]


# Clustering Algorithm: Partitioning around medoids (PAM)
# calculating silhoutte width for several k
sil_width<-c(NA)
for (i in 2:10) {
  pam_fit<-pam(dist_Matrix,diss = TRUE, k = i) 
  sil_width[i]<-pam_fit$silinfo$avg.width
}


# Plotting Silhouette width (higher is better)
plot(1:10,sil_width)



# Pick the number of cluster with the highest silhoutte width
pam_fit<-pam(dist_Matrix,diss = TRUE, k = 4)

data_cluster_subset <- mutate(data_cluster_subset, cluster = pam_fit$clustering)

pam_results <- train %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary


# The medoids for the clusters
data_cluster_subset[pam_fit$medoids, ]


train_first <- train
write.csv2(train_first, file = 'train_first.csv')





install.packages("rioja")

library(rioja) 

data(RLGH)
data(SWAP)
result <- compare.datasets(RLGH$spec, SWAP$spec)
result


plot.compare.datasets(RLGH$spec, SWAP$spec)


set.seed(1001)
x<- sample(1:1000, 100)

library(caret)
folds <- createDataPartition(x, times=5, p = 0.1) 

# Установить pbmcapply
install.packages("pbmcapply")
library(pbmcapply)




pbmclapply(1:10, func)








sil_width<-c(NA)
for (i in 2:10) {
  pam_fit<-pam(dist_Matrix,diss = TRUE, k = i) 
  sil_width[i]<-pam_fit$silinfo$avg.width
}
