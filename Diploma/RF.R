install.packages("kernlab")
library(randomForest)
library(dplyr)
library(ROCR)
library(ggplot2)
library(MLmetrics)
library(caret)

registerDoMC(4) #number of cores on the machine
darkAndScaryForest <- foreach(y=seq(10), .combine=combine ) %dopar% {
  set.seed(y) # not really needed
  rf <- randomForest(Species ~ ., iris, ntree=50, norm.votes=FALSE)
}

data <- work_data2

#create test and dev samples

#  random number of observation(1/4 of all sample)
set.seed(1234)
test.num <- sample(1:nrow(data), 200, replace = FALSE)

# create 2 samples, at first
test <- data[test.num, ]
train <- data[-test.num, ]

x <- train[, 2:21]

y <- train[, 1]
# y go to the factor("say" RF that we wish classification)
y.1 <- as.factor(y) 
train$target <- as.factor(train$target)


results_df <- data.frame(matrix(ncol = 8))
colnames(results_df)[1]="No. of trees"
colnames(results_df)[2]="No. of variables"
colnames(results_df)[3]="Dev_AUC"
colnames(results_df)[4]="Dev_Hit_rate"
colnames(results_df)[5]="Dev_Coverage_rate"
colnames(results_df)[6]="Val_AUC"
colnames(results_df)[7]="Val_Hit_rate"
colnames(results_df)[8]="Val_Coverage_rate"

trees = c(50, 100, 400, 800, 1000, 2000)
variables = c(3,5,7,9,10)

for(i in 1:length(trees))
{
  ntree = trees[i]
  for(j in 1:length(variables))
  {
    mtry = variables[j]
    set.seed(3217)
    rf<-randomForest(x,y=y.1,ntree=ntree,mtry=mtry, replace=T,
                     importance=TRUE, localImp=FALSE,
                     proximity=F, norm.votes=TRUE,
                     corr.bias=FALSE)
    pred<-as.data.frame(predict(rf,type="class"))
    class_rf<-cbind(train$target,pred)
    
    colnames(class_rf)[1]<-"actual_values"
    colnames(class_rf)[2]<-"predicted_values"
    dev_hit_rate = nrow(subset(class_rf, actual_values ==1&predicted_values==1))/nrow(subset(class_rf, predicted_values ==1))
    dev_coverage_rate = nrow(subset(class_rf, actual_values ==1&predicted_values==1))/nrow(subset(class_rf, actual_values ==1))
    
    pred_prob<-as.data.frame(predict(rf,type="prob"))
    prob_rf<-cbind(train$target,pred_prob)
    colnames(prob_rf)[1]<-"target"
    colnames(prob_rf)[2]<-"prob_0"
    colnames(prob_rf)[3]<-"prob_1"
    pred<-prediction(prob_rf$prob_1,train$target)
    auc <- performance(pred,"auc")
    dev_auc<-as.numeric(auc@y.values)
    
    pred<-as.data.frame(predict(rf,test[,-1], type="class"))
    class_rf<-cbind(test$target,pred)
    
    pred_prob<-as.data.frame(predict(rf,test[,-1],type="prob"))
    prob_rf<-cbind(test$target,pred_prob)
   colnames(class_rf)[1]<-"actual_values"
    colnames(class_rf)[2]<-"predicted_values"
    val_hit_rate = nrow(subset(class_rf, actual_values ==1&predicted_values==1))/nrow(subset(class_rf, predicted_values ==1))
    val_coverage_rate = nrow(subset(class_rf, actual_values ==1&predicted_values==1))/nrow(subset(class_rf, actual_values ==1))
    
     colnames(prob_rf)[1]<-"target"
    colnames(prob_rf)[2]<-"prob_0"
    colnames(prob_rf)[3]<-"prob_1"
    pred<-prediction(prob_rf$prob_1,prob_rf$target)
    auc <- performance(pred,"auc")
    val_auc<-as.numeric(auc@y.values)
    results_df = rbind(results_df,c(ntree,mtry,dev_auc,dev_hit_rate,dev_coverage_rate,val_auc,val_hit_rate,val_coverage_rate))
  }
} 

ntree_value <- 50
mtry_value <- 3
rf<-randomForest(x,y=y.1,ntree= ntree_value,mtry=mtry_value, replace=T,
                 importance=TRUE, localImp=FALSE,
                 proximity=F, norm.votes=TRUE,
                 corr.bias=FALSE)

pred1=predict(rf,type = "prob")
perf = prediction(pred1[,2], train$target)

#score test data set
m4_fitForest <- predict(rf, newdata = test, type="prob")[,2]
m4_pred <- prediction( m4_fitForest, test$target)
m4_perf <- performance(m4_pred, "tpr", "fpr")

#plot variable importance
png(filename="RF.png", res=150, width = 2000, height = 1000)
varImpPlot(rf, main="Random Forest: Variable Importance")
dev.off()

ev_df_m4 <- data.frame(Gini = round(((slot(performance(m4_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(m4_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m4 <- ggtexttable(ev_df_m4, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

gini_plot <- ggplot(setNames(data.frame(m4_perf@x.values, m4_perf@y.values), c('x_val', 'y_val')), 
                    aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  ggtitle(paste("Gini=", round(Gini,4), sep="")) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")





##grid method
#example
data(twoClassData)
twoClass=cbind(classes, as.data.frame(predictors))

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# train model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
tunegrid <- expand.grid(.mtry=c(2:3), .ntree=c(500, 700, 1000, 2000))
set.seed(12)
custom <- train(classes~., 
                twoClass, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)







