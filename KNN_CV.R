
setwd("F:/scoring/KNN")
install.packages("ISLR")
library(ISLR)
library(dplyr)
library(class)
library(DT)
library(ggplot2)
                 
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


















# testing 
previous_data <- read.table("Wine.txt", header = T)
sc_data <- as.data.frame(scale(previous_data[,-14]))

datatable(previous_data)
str(previous_data)

# Train Test split
# choose random observation
set.seed(3476)
train.num <- sample(1:nrow(previous_data), round(nrow(previous_data)*.7), replace = FALSE)

test <- sc_data[-train.num, -14]
test.target <- previous_data[-train.num, 14]
train <- sc_data[train.num, -14]
train.target <- previous_data[train.num, 14]


# KNN Model

error <- rep(0,25)
i <- 1
for (i in 1:25) {
  pred <- knn(train, test, train.target, k=i)
  error[i] <- mean(pred != test.target)
}

error

### VISUALIZE K ELBOW METHOD

k <- 1:25
error.df <- data.frame(error, k)

ggplot(error.df, aes(k, error)) +
  geom_point() +
  geom_line(lty='dotted', color='red') +
  scale_x_continuous(breaks=seq(1, tail(k, n=1),2))


x.train <- train
y.train <- train.target
nfolds <- 10
# Cross-validation

knn.cv <- function(x.train,y.train,nfolds) {
  # Perform nfolds-cross validation of kNN, for the values of k in klist
  
  # Number of instances
  n.train <- nrow(x.train)
  
  # Matrix to store predictions
  p.cv <- matrix(NA, n.train, seq(n.train))
  
  # Prepare the folds
  s <- split(sample(n.train),rep(1:nfolds,length=n.train))
  
  # Cross-validation
  for (i in seq(nfolds)) {
    p.cv[s[[i]],] <- knn(klist,x.train[-s[[i]],], y.train[-s[[i]]], 9)
  }
  
  # Return matrix of CV predictions
  invisible(p.cv)
}

knn.cv(train, train.target, 10)

# Make predictions by kNN
klist <- seq(n.train) # we test all values of k
nfolds <- 5 # we make 5-fold cross-validation
y.pred.train <- knn(klist,x.train,y.train,x.train)
y.pred.test <- knn(klist,x.train,y.train,x.test)
y.pred.cv <- knn.cv(klist,x.train,y.train,nfolds)

# Compute mean-square error (MSE)
mse.train <- apply((y.pred.train - y.train)^2 , 2, mean)
mse.test <- apply((y.pred.test - y.test)^2 , 2, mean)
mse.cv <- apply((y.pred.cv - y.train)^2 , 2, mean)

# Plot MSE as a function of k
plot(mse.train , ylim=c(0,2) , type='l' , xlab='k' , ylab='MSE', col=1 , lwd=2)
lines(mse.test , col=2 , lwd=2)
lines(mse.cv, col=3 , lwd=2)
legend("bottomright",legend=c('Train','Test','CV'),text.col=seq(3) , lty=1 , col=seq(3))
