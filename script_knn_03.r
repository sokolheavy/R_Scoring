
sales <- read.table("DISCRIM_0_new_R.txt", header=T, sep=";")

summary(sales)
sales[1:5, ]

dim(sales)
class(sales[ , 6])


set.seed(14)
test.num <- sample(1:nrow(sales), 50, replace = FALSE)

test <- sales[test.num, 2:5]

train <- sales[-test.num, 2:5]

cl <- sales[-test.num, 6]

library(class)

zzz1 <- knn(train, test, cl, k = 3)

table(zzz1, sales[test.num, 6])


a <-rep(0,15)

for (i in 1:15)
{
  zzz <- knn(train, test, cl, k = i)
  a[i] <- summ(zzz != sales[test.num, 6])
}

a

##testing

set.seed(3476)
test.num <- sample(1:nrow(data), 2000, replace = FALSE)

test_knn <- data[test.num, 1:12]

train_knn <- data[-test.num, 1:12]

cl <- data[-test.num, 1]

library(class)

zzz1 <- knn(train_knn, test_knn, cl, k = 3)

table(zzz1, test$target_for_calc)

##########################
##coutses
install.packages("ISLR"
library(ISLR)
library(dplyr)
library(class)

previous_data <- as.data.frame(scale(Caravan[,-86]))

# Train Test split
# choose random observation
set.seed(3476)
train.num <- sample(1:nrow(previous_data), round(nrow(previous_data)*.7), replace = FALSE)

test <- previous_data[-train.num, -86]
test.target <- previous_data[-train.num, 86]

train <- previous_data[train.num, -86]
train.target <- previous_data[-train.num, 86]


# KNN Model

error <- rep(0,15)
for (i in 1:15) {
  pred <- knn(train, test, train[,86], k=i)
  error[i] <- mean(pred != test[,])
}

error

### VISUALIZE K ELBOW METHOD

k <- 1:15
error.df <- data.frame(error, k)

ggplot(error.df, aes(k, error)) +
  geom_point() +
  geom_line(lty='dotted', color='red') +
  scale_x_continuous(breaks=seq(1, tail(k, n=1),2))
  
