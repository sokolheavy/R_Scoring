# https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
# https://insightr.wordpress.com/2018/05/17/tuning-xgboost-in-r-part-i/
# https://rpubs.com/mharris/multiclass_xgboost


# = train and test data = #

factor_names <- names(data[ ,sapply(data, is.factor)])
data_set <- createDummyFeatures(data, cols = factor_names)

set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]

train_set <- train
test_set <- test

train <- sparse.model.matrix(target ~ .-1, data = train_set)
train_label <- train_set[,"target"]
train_matrix <- xgb.DMatrix(data = as.matrix(train), label = train_label)


test <- sparse.model.matrix(target ~ .-1, data = test_set)
test_label <- test_set[,"target"]
test_matrix <- xgb.DMatrix(data = as.matrix(test), label = test_label)


#tune parameters

seedNum <- 333

cv_folds <- KFold(train_label, nfolds = 5, stratified = FALSE, seed = seedNum)
xgb_cv_bayes <- function(nround,max.depth, min_child_weight, subsample,eta,gamma,colsample_bytree,max_delta_step) {
  param<-list(booster = "gbtree",
              max_depth = max.depth,
              min_child_weight = min_child_weight,
              eta=eta,gamma=gamma,
              subsample = subsample, colsample_bytree = colsample_bytree,
              max_delta_step=max_delta_step,
              lambda = 1, alpha = 0,
              objective = "binary:logistic",
              eval_metric = "auc")
  
  cv <- xgb.cv(params = param, 
               data = train_matrix, 
               folds = cv_folds,
               nrounds = 1000,
               early_stopping_rounds = 10, 
               maximize = TRUE)
  
  list(Score = cv$evaluation_log$test_auc_mean[cv$best_iteration],
       Pred=cv$best_iteration)
  # we don't need cross-validation prediction and we need the number of rounds.
  # a workaround is to pass the number of rounds(best_iteration) to the Pred, which is a default parameter in the rbayesianoptimization library.
}
OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                bounds = list(max.depth =c(3, 10),
                                              min_child_weight = c(1, 40),
                                              subsample = c(0.6, 0.9),
                                              eta=c(0.01,0.3),
                                              gamma = c(0.0, 0.2),
                                              colsample_bytree=c(0.5,0.8),
                                              max_delta_step=c(1,10)),
                                init_grid_dt = NULL,
                                init_points = 10, 
                                n_iter = 10,
                                acq = "ucb", 
                                kappa = 2.576,
                                eps = 0.0)

best_param <- list(
  booster = "gbtree",
  eval.metric = "auc",
  objective = "binary:logistic",
  max_depth = OPT_Res$Best_Par["max.depth"],
  eta = OPT_Res$Best_Par["eta"],
  gamma = OPT_Res$Best_Par["gamma"],
  subsample = OPT_Res$Best_Par["subsample"],
  colsample_bytree = OPT_Res$Best_Par["colsample_bytree"],
  min_child_weight = OPT_Res$Best_Par["min_child_weight"],
  max_delta_step = OPT_Res$Best_Par["max_delta_step"])
# number of rounds should be tuned using CV
# However, nrounds can not be directly derivied from the bayesianoptimization function
# Here, OPT_Res$Pred, which was supposed to be used for cross-validation, is used to record the number of rounds
nrounds=OPT_Res$Pred[[which.max(OPT_Res$History$Value)]]
xgb_model <- xgb.train (params = best_param, data = train_matrix, nrounds = nrounds)

p <- predict(xgb_model, newdata = test_matrix)
pred <- prediction(p, test_label)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

ev <- data.frame(Gini = round(((slot(performance(pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(pred, measure = "auc")@y.values[[1]]*100, 2))

