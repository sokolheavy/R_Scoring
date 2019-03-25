# Packages
library(xgboost)
library(magrittr)
library(dplyr)
library(Matrix)
library(ggplot2)

# Data
data <- read.csv("https://raw.githubusercontent.com/2lsokol2/R_Scoring/master/GBM/binary.csv",
                  header = T, dec=".")

str(data)
data$rank <- as.factor(data$rank)

# Partition data
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]

# Create matrix - One-Hot Encoding for Factor variables
trainm <- sparse.model.matrix(admit ~ .-1, data = train)
head(trainm)
head(data)
train_label <- train[,"admit"]
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

testm <- sparse.model.matrix(admit~.-1, data = test)
test_label <- test[,"admit"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

# Parameters
nc <- length(unique(train_label))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = nc)
watchlist <- list(train = train_matrix, test = test_matrix)

# eXtreme Gradient Boosting Model
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 1000, 
                       watchlist = watchlist, # can see all rounds of losses
                       eta = 0.001, # learning rate 
                       max.depth = 3, # depth of trees
                       gamma = 0, # larger value - more conservative algoritm  
                       subsample = 1, # proc splits - test/train
                       colsample_bytree = 1, 
                       missing = NA,
                       seed = 333)


# Training & test error plot
e <- data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')

min(e$test_mlogloss)
e[e$test_mlogloss == 0.62571,]

# Feature importance
imp <- xgb.importance(colnames(train_matrix), model = bst_model)
print(imp)
xgb.plot.importance(imp)

# Prediction & confusion matrix - test data
p <- predict(bst_model, newdata = test_matrix)
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
         t() %>%
         data.frame() %>%
         mutate(label = test_label, max_prob = max.col(., "last")-1)
table(Prediction = pred$max_prob, Actual = pred$label)
