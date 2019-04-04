# install.packages("outliers")

library(dplyr)
library(caret)
library(caretEnsemble)
library(mice)
library(doParallel)
library(car)
library(outliers)

################Read data from csv#################################
train_data  <- read.csv('https://raw.githubusercontent.com/eodaGmbH/predictive_maintenance_trucks/master/aps_failure_training_set.csv',
                       na = "na", skip = 20)

test_data  <- read.csv('https://raw.githubusercontent.com/Arpit-Sharma-USC/Homework-1/master/aps_failure_test_set.csv',
                                       na = "na", skip = 20)
names(test_data) <- names(train_data)
summary(train_data)
dim(train_data) #dimentions of the data
dim(train_data)
####################### data cleaning ##############################################
### 'view' on the data
summary(train_data$class)
summary(test_data$neg)

# persentage of values in variable
options(digits = 2)
prop.table(table(train_data$class))
prop.table(table(test_data$class))

# how many missing value
options(scipen = 999)
summary_df <- do.call(cbind, lapply(train_data[, 
                                                  2:ncol(train_data)], summary))
summary_df_t <- as.data.frame(round(t(summary_df),0))
names(summary_df_t)[7] <- paste("Missing_values")
summary_df_t_2 <- summary_df_t %>% 
  mutate(obs = nrow(train_data),
         Missing_prop = Missing_values / obs)
print(summary_df_t_2)

summary_df_t_2 %>% summarise(Min = mean(Min.),
                             first_Q = mean(`1st Qu.`),
                             Median = median(Median),
                             Mean = mean(Mean),
                             third_Q = mean(`3rd Qu.`),
                             Max = max(Max.),
                             mean_MV = mean(Missing_values),
                             obs = mean(obs),
                             mean_MV_perc = mean_MV / obs)

##### union all data to impute value 
#replicate our sets
training_data_bind <- train_data
test_data_bind <- test_data
#create a new column "set" to label the observations
training_data_bind$set <- "TRAIN"
test_data_bind$set <- "TEST"
#merge them into 1 single set
full_dataset <- rbind(training_data_bind, test_data_bind)
dim(full_dataset)

# impute value to missing value
set.seed(123)
imputed_full <- mice(full_dataset, 
                     m=1, 
                     maxit = 5, 
                     method = "mean", 
                     seed = 500)

full_imputed <- complete(imputed_full, 1)

# check that we don’t have missing values













set.seed(123)
f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}
ss = data.frame(apply(df,2,f))
ts <- data.frame(apply(test_set,2,f))
for (data in ss[,2:171]) {
  # data <- aps_failure_training_set$x
  qnt <- quantile(data, probs=c(.25, .75), na.rm = T)
  caps <- quantile(data, probs=c(.05, .95), na.rm = T)
  H <- 3.5 * IQR(data, na.rm = T)
  data[data < (qnt[1] - H)] <- caps[1]
  data[data > (qnt[2] + H)] <- caps[2]
}
# Outlier handle
for (data in ts[,2:171]) {
  # data <- aps_failure_training_set$x
  qnt <- quantile(data, probs=c(.25, .75), na.rm = T)
  caps <- quantile(data, probs=c(.05, .95), na.rm = T)
  H <- 3.5 * IQR(data, na.rm = T)
  data[data < (qnt[1] - H)] <- caps[1]
  data[data > (qnt[2] + H)] <- caps[2]
}
ss$class <- as.factor(ifelse(df$class == 'neg', 0, 1)) #neg for 0, pos for 1
ts$class <- as.factor(ifelse(test_set$class == 'neg', 0, 1)) #neg for 0, pos for 1

summary(ss)

###################### feature engineering ##################################
ss$ag_sum <- ss$ag_000 + ss$ag_001 + ss$ag_002 + ss$ag_003 + ss$ag_004 + ss$ag_005 + ss$ag_006 + ss$ag_007 + ss$ag_008 + ss$ag_009
ss$ay_sum <- ss$ay_000 + ss$ay_001 + ss$ay_002 + ss$ay_003 + ss$ay_004 + ss$ay_005 + ss$ay_006 + ss$ay_007 + ss$ay_008 + ss$ay_009
ss$az_sum <- ss$az_000 + ss$az_001 + ss$az_002 + ss$az_003 + ss$az_004 + ss$az_005 + ss$az_006 + ss$az_007 + ss$az_008 + ss$az_009
ss$ba_sum <- ss$ba_000 + ss$ba_001 + ss$ba_002 + ss$ba_003 + ss$ba_004 + ss$ba_005 + ss$ba_006 + ss$ba_007 + ss$ba_008 + ss$ba_009
ss$cn_sum <- ss$cn_000 + ss$cn_001 + ss$cn_002 + ss$cn_003 + ss$cn_004 + ss$cn_005 + ss$cn_006 + ss$cn_007 + ss$cn_008 + ss$cn_009
ss$cs_sum <- ss$cs_000 + ss$cs_001 + ss$cs_002 + ss$cs_003 + ss$cs_004 + ss$cs_005 + ss$cs_006 + ss$cs_007 + ss$cs_008 + ss$cs_009
ss$ee_sum <- ss$ee_000 + ss$ee_001 + ss$ee_002 + ss$ee_003 + ss$ee_004 + ss$ee_005 + ss$ee_006 + ss$ee_007 + ss$ee_008 + ss$ee_009

ts$ag_sum <- ts$ag_000 + ts$ag_001 + ts$ag_002 + ts$ag_003 + ts$ag_004 + ts$ag_005 + ts$ag_006 + ts$ag_007 + ts$ag_008 + ts$ag_009
ts$ay_sum <- ts$ay_000 + ts$ay_001 + ts$ay_002 + ts$ay_003 + ts$ay_004 + ts$ay_005 + ts$ay_006 + ts$ay_007 + ts$ay_008 + ts$ay_009
ts$az_sum <- ts$az_000 + ts$az_001 + ts$az_002 + ts$az_003 + ts$az_004 + ts$az_005 + ts$az_006 + ts$az_007 + ts$az_008 + ts$az_009
ts$ba_sum <- ts$ba_000 + ts$ba_001 + ts$ba_002 + ts$ba_003 + ts$ba_004 + ts$ba_005 + ts$ba_006 + ts$ba_007 + ts$ba_008 + ts$ba_009
ts$cn_sum <- ts$cn_000 + ts$cn_001 + ts$cn_002 + ts$cn_003 + ts$cn_004 + ts$cn_005 + ts$cn_006 + ts$cn_007 + ts$cn_008 + ts$cn_009
ts$cs_sum <- ts$cs_000 + ts$cs_001 + ts$cs_002 + ts$cs_003 + ts$cs_004 + ts$cs_005 + ts$cs_006 + ts$cs_007 + ts$cs_008 + ts$cs_009
ts$ee_sum <- ts$ee_000 + ts$ee_001 + ts$ee_002 + ts$ee_003 + ts$ee_004 + ts$ee_005 + ts$ee_006 + ts$ee_007 + ts$ee_008 + ts$ee_009

split <- nrow(ss)*2/3
train <- ss[1:split,]
test <- ss[(split+1):nrow(ss),]

train$class <- as.factor(train$class)
# test <- ss[5000:8000,]
test$class <- as.factor(test$class)
####################### random forest ######################################
library(caret)
library(randomForest)

ss_rf <- randomForest(train$class~.,data=train,importance = TRUE, proximity = FALSE, ntree = 400)
table(predict(ss_rf),train$class)

# n <- length(names(train)))
for(i in 10:15) {
  model <- randomForest(train$class~., data=train,importance = TRUE, proximity = FALSE, ntree = 400, mtry=i)
  err <- mean(model$err.rate)
  print(err)
}



ss_pred <- predict(ss_rf, newdata = test)
table(ss_pred,test$class)

plot(margin(ss_rf, test$class))

plot(ss_rf)
plot(importance(ss_rf), lty=2, pch=16)
lines(importance(ss_rf))
imp = importance(ss_rf)
impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op = par(mfrow=c(1, 3))
# visualization for important variables
# for (i in seq_along(impvar)) {
#   partialPlot(ss_rf, train, impvar[i], xlab=impvar[i],
#               main=paste("Partial Dependence on", impvar[i]),
#               ylim=c(0, 1))
# }

t_rf <- randomForest(ss$class~.,data=ss,importance = TRUE, proximity = FALSE, ntree = 400) # apply for the whole training set
table(predict(t_rf,newdata = ts),ts$class) # predict test dataset
print(t_rf)
plot(t_rf)
#################### Classification and Regression Trees #############################
library(rpart)
library(rpart.plot)
tree <- rpart(train$class~., method = "class", data = train, control = rpart.control(xval = 10, cp = 0.0001))
printcp(tree)
bestcp <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(tree, cp = bestcp)

# confusion matrix (training data)
conf.matrix <- table(train$class, predict(tree.pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

tree_pred <- predict(tree, newdata = test, type="class")
table(tree_pred,test$class)

# visualizations
plot(tree.pruned)
text(tree.pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)
prp(tree.pruned, faclen = 0, cex = 0.8, extra = 1)
tot_count <- function(x, labs, digits, varlen)
{
  paste(labs, "\n\nn =", x$frame$n)
}

prp(tree.pruned, faclen = 0, cex = 0.8, node.fun=tot_count)


#################### Bagging CART ################################
library(ipred)
bcart <-  bagging(train$class~., data=train, type = "class", coob = TRUE)

predictions <- predict(bcart, newdata = test, type="class")
table(predictions, test$class)

t_bagging <- bagging(ss$class~., data=ss, type = "class", coob = TRUE)

# apply to the whole train set
bagging_pre <- predict(t_bagging, newdata = ts, type="class")
table(bagging_pre, ts$class)