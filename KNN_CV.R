# https://www.analyticsvidhya.com/blog/2017/09/understaing-support-vector-machine-example-code/
# https://trainings.analyticsvidhya.com/courses/course-v1:AnalyticsVidhya+LP_DL_2019+2019_T1/about?utm_source=AVbannerblogtop&utm_medium=display&utm_campaign=LPDL2019
# cheetseet - https://www.analyticsvidhya.com/blog/2017/02/top-28-cheat-sheets-for-machine-learning-data-science-probability-sql-big-data/
# https://www.analyticsvidhya.com/blog/2017/09/naive-bayes-explained/
# regresion - https://www.analyticsvidhya.com/blog/2015/08/comprehensive-guide-regression/
# lasso http://www.science.smith.edu/~jcrouser/SDS293/labs/lab10-r.html
# PREPROCESING https://habr.com/ru/post/173049/
# https://www.analyticsvidhya.com/blog/2017/02/introduction-to-ensembling-along-with-implementation-in-r/

setwd("F:/scoring/KNN")
install.packages("ISLR")
library(ISLR)
library(dplyr)
library(class)
library(DT)
library(ggplot2)

previous_data <- read.table("https://raw.githubusercontent.com/2lsokol2/R_Scoring/master/Wine.txt", header = T, )
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

# Cross-validation

knn.cv <- function(x.train,y.train, nfolds) {
  # Perform nfolds-cross validation of kNN, for the values of k in klist
  
  # Number of instances
  n.train <- nrow(x.train)
  
  # Matrix to store predictions
  p.cv <- matrix(NA, n.train, seq(n.train))
  
  # Prepare the folds
  s <- split(sample(n.train),rep(1:nfolds,length=n.train))

  eval_table <- data.frame()
  # Cross-validation
  for (j in seq(10)) {
    for (i in seq(nfolds)) {
    pred <- knn(x.train[-s[[i]],], x.train[s[[i]],], y.train[-s[[i]]], j)
    eval_table <- union_all(eval_table, data.frame(Acc = mean(pred !=  y.train[s[[i]]])
                                                 , Model = paste0("Model_", j)
                                                 , Folds = paste0("Fold_", i)))
    }
  }
  
  # Return matrix of CV predictions
  error
}

knn.cv(train, train.target, 10)

###Cheak and evauate evaluation

Acc_analysis <- function(eval_table, k_fold = 10) {
  eval_table <- group_by(eval_table, Model)
  AccCV <- summarize(eval_table, mAcc = mean(Acc), sdAcc = sd(Acc)) 
  AccCV <- mutate(AccCV, AccCV = round(mAcc,3), 
                  AccCVInf = round((mAcc - 2 * sdAcc/sqrt(k_fold)),3),
                  AccCVPAC = round((mAcc - sdAcc),3))
  Acc_avaluation <- select(AccCV, Model, AccCV, AccCVInf, AccCVPAC)  
  Acc_avaluation
}

ggplot(data = reshape2::melt(Acc_analysis(eval_table), "Model"), 
       aes(x = Model, y = value, color = variable)) + 
  geom_point(size = 5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.margin = grid::unit(c(1, 1, .5, 1.5), "lines"))

#######################################################################################
### KNN with caret packetches

# Validation
valid_pred <- predict(model1,validation, type = “prob”)

#Storing Model Performance Scores
library(ROCR)
pred_val <-prediction(valid_pred[,2],validation$Win.Loss)

# Calculating Area under Curve (AUC)
perf_val <- performance(pred_val,”auc”)
perf_val

# Plot AUC
perf_val <- performance(pred_val, “tpr”, “fpr”)
plot(perf_val, col = “green”, lwd = 1.5)

#Calculating KS statistics
ks <- max(attr(perf_val, “y.values”)[[1]] – (attr(perf_val, “x.values”)[[1]]))
ks






