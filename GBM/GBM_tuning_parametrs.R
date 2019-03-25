library(tidyverse)
library(xgboost)

housing = read.csv('https://raw.githubusercontent.com/ageron/handson-ml/master/datasets/housing/housing.csv')

housing$total_bedrooms[is.na(housing$total_bedrooms)] = median(housing$total_bedrooms , na.rm = TRUE)

housing$mean_bedrooms = housing$total_bedrooms/housing$households
housing$mean_rooms = housing$total_rooms/housing$households

drops = c('total_bedrooms', 'total_rooms')

housing = housing[ , !(names(housing) %in% drops)]

categories = unique(housing$ocean_proximity)

#split the categories off
cat_housing = data.frame(ocean_proximity = housing$ocean_proximity)

for(cat in categories){
  cat_housing[,cat] = rep(0, times= nrow(cat_housing))
}

for(i in 1:length(cat_housing$ocean_proximity)){
  cat = as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] = 1
}

cat_columns = names(cat_housing)
keep_columns = cat_columns[cat_columns != 'ocean_proximity']
cat_housing = select(cat_housing,one_of(keep_columns))
drops = c('ocean_proximity','median_house_value')
housing_num =  housing[ , !(names(housing) %in% drops)]


scaled_housing_num = scale(housing_num)

cleaned_housing = cbind(cat_housing, scaled_housing_num, median_house_value=housing$median_house_value)


set.seed(19) # Set a random seed so that same sample can be reproduced in future runs

sample = sample.int(n = nrow(cleaned_housing), size = floor(.8*nrow(cleaned_housing)), replace = F)
train = cleaned_housing[sample, ] #just the samples
test  = cleaned_housing[-sample, ] #everything but the samples


train_y = train[,'median_house_value']
train_x = train[, names(train) !='median_house_value']

test_y = test[,'median_house_value']
test_x = test[, names(test) !='median_house_value']

head(train)

  

######
# XG Boost
######

# see the docs: http://cran.fhcrc.org/web/packages/xgboost/vignettes/xgboost.pdf
library(xgboost)

#put into the xgb matrix format
dtrain = xgb.DMatrix(data =  as.matrix(train_x), label = train_y )
dtest = xgb.DMatrix(data =  as.matrix(test_x), label = test_y)

# these are the datasets the rmse is evaluated for at each iteration
watchlist = list(train=dtrain, test=dtest)
  
  

####
# Proper use - validation set
####


sample = sample.int(n = nrow(train), size = floor(.8*nrow(train)), replace = F)

train_t = train[sample, ] #just the samples
valid  = train[-sample, ] #everything but the samples

train_y = train_t[,'median_house_value']

#if tidyverse was used, dplyr pull function solves the problem:
#train_y = pull(train_t, median_house_value)
train_x = train_t[, names(train_t) !='median_house_value']

valid_y = valid[,'median_house_value']
valid_x = valid[, names(train_t) !='median_house_value']

dtest = xgb.DMatrix(data =  as.matrix(test_x))

gb_train = xgb.DMatrix(data = as.matrix(train_x), label = train_y )
gb_valid = xgb.DMatrix(data = as.matrix(valid_x), label = valid_y )

# train xgb, evaluating against the validation
watchlist = list(train = gb_train, valid = gb_valid)



###
# Grid search first principles 
###

max.depths = c(6, 7, 9)
etas = c(0.01, 0.001)

best_params = 0
best_score = 0

count = 1
for( depth in max.depths ){
  for( num in etas){
    
    bst_grid = xgb.train(data = gb_train, 
                         max.depth = depth, 
                         eta=num, 
                         nthread = 2, 
                         nround = 10000, 
                         watchlist = watchlist, 
                         objective = "reg:linear", 
                         early_stopping_rounds = 50, 
                         verbose=0)
    
    if(count == 1){
      best_params = bst_grid$params
      best_score = bst_grid$best_score
      count = count + 1
    }
    else if( bst_grid$best_score < best_score){
      best_params = bst_grid$params
      best_score = bst_grid$best_score
    }
  }
}

best_params
best_score
max_depth

bst_tuned = xgb.train(data = gb_train, 
                     max.depth = 7, 
                     eta=0.01, 
                     nthread = 2, 
                     nround = 10000, 
                     watchlist = watchlist, 
                     objective = "reg:linear", 
                     early_stopping_rounds = 50, 
                     verbose=0)


# Training & test error plot
e <- data.frame(bst_tuned$evaluation_log)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')


imp <- xgb.importance(colnames(dtest), model = bst_tuned)
print(imp)
xgb.plot.importance(imp)


# Prediction & confusion matrix - test data
p <- predict(bst_tuned, dtest)
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(., "last")-1) 

  
auc <- mean(pred$label==pred$max_prob)
gini <- auc*2 -1

auc
gini

#####
# XGBOOM
#####


# create hyperparameter grid
hyper_grid <- expand.grid(
  eta = c(.001, .005, .01, .125),
  max_depth = c(1, 3, 5, 7),
  min_child_weight = c(1, 3, 5, 7),
  subsample = c(.4, .5, .6), 
  colsample_bytree = c(.8, .9, 1),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

nrow(hyper_grid)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # create parameter list
  params <- list(
    eta = hyper_grid$eta[i],
    max_depth = hyper_grid$max_depth[i],
    min_child_weight = hyper_grid$min_child_weight[i],
    subsample = hyper_grid$subsample[i],
    colsample_bytree = hyper_grid$colsample_bytree[i]
  )
  
  # reproducibility
  set.seed(123)
  
  # train model
  xgb.tune <- xgb.cv(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 5000,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
  hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

hyper_grid %>%
  dplyr::arrange(min_RMSE) %>%
  head(10)



################ SET OPTIMAL MODEL

# parameter list
params <- list(
  eta = 0.01,
  max_depth = 5,
  min_child_weight = 5,
  subsample = 0.65,
  colsample_bytree = 1
)

# train final model
xgb.fit.final <- xgboost(
  params = params,
  data = features_train,
  label = response_train,
  nrounds = 1576,
  objective = "reg:linear",
  verbose = 0
)


# create importance matrix
importance_matrix <- xgb.importance(model = xgb.fit.final)

# variable importance plot
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")


###PLOT ONLY FOR REGRESION 
pdp <- xgb.fit.final %>%
  partial(pred.var = "Gr_Liv_Area_clean", n.trees = 1576, grid.resolution = 100, train = features_train) %>%
  autoplot(rug = TRUE, train = features_train) +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("PDP")

ice <- xgb.fit.final %>%
  partial(pred.var = "Gr_Liv_Area_clean", n.trees = 1576, grid.resolution = 100, train = features_train, ice = TRUE) %>%
  autoplot(rug = TRUE, train = features_train, alpha = .1, center = TRUE) +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("ICE")

gridExtra::grid.arrange(pdp, ice, nrow = 1)
###


# one-hot encode the local observations to be assessed.
local_obs_onehot <- vtreat::prepare(treatplan, local_obs, varRestriction = new_vars)

# apply LIME
explainer <- lime(data.frame(features_train), xgb.fit.final)
explanation <- explain(local_obs_onehot, explainer, n_features = 5)
plot_features(explanation)





