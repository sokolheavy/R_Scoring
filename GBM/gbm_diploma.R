# work_data

# Partition data
set.seed(1234)
ind <- sample(2, nrow(work_data), replace = T, prob = c(0.8, 0.2))
train <- work_data[ind==1,]
test <- work_data[ind==2,]

# Create matrix - One-Hot Encoding for Factor variables
trainm <- sparse.model.matrix(target ~ .-1, data = train)

head(trainm)
head(work_data)
train_label <- train[,"target"]
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

testm <- sparse.model.matrix(target~.-1, data = test)
test_label <- test[,"target"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

# Parameters
nc <- length(unique(train_label))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = nc)
watchlist <- list(train = train_matrix, test = test_matrix)



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


imp <- xgb.importance(colnames(train_matrix), model = bst_model)
print(imp)
xgb.plot.importance(imp)

pred_target <- predict(object = model, type = "response")

# add values for perfomance plots(only for calc)
pred <- prediction(pred_target, train[-s[[k]], measurevar])


# Prediction & confusion matrix - test data
p <- predict(bst_model, newdata = test_matrix, type = "response")




# add values for perfomance plots(only for calc)
pred1 <- prediction(p, test_matrix)
Gini  <- round(((slot(performance(pred1, measure = "auc"),"y.values")[[1]])*2 - 1), 4)
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(., "last")-1) %>%
  
auc <- mean(pred$label==pred$max_prob)
gini <- auc*2 -1










