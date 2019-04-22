
setwd("F:/scoring/KNN")
install.packages("ISLR")
library(ISLR)
library(dplyr)
library(class)
library(DT)
library(ggplot2)
library(caret)
library(e1071)
library(ROCR)

previous_data <- read.table("https://raw.githubusercontent.com/2lsokol2/R_Scoring/master/Wine.txt", header = T, )
sc_data <- as.data.frame(scale(previous_data[,-14]))

datatable(previous_data)
str(previous_data)

# Train Test split
# choose random observation
set.seed(3476)
index <- sample(1:nrow(previous_data), round(nrow(previous_data)*.7), replace = FALSE)

test <- sc_data[-index, -14]
test.target <- previous_data[-index, 14]
train <- sc_data[index, -14]
train.target <- previous_data[index, 14]


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
setwd("C:/Users/EASokol/Desktop/Diploma")
data1 = read.csv('US_Presidential_Data.csv')

# Transforming the dependent variable to a factor
data1$Win.Loss = as.factor(data1$Win.Loss)
datatable(data1[1:100,])


# Train Test split
# choose random observation
set.seed(101)
index <- sample(1:nrow(data1), round(nrow(data1)*.7), replace = FALSE)
train = data1[index,]
test = data1[-index,]

# Setting levels for both training and validation data
levels(train$Win.Loss) <- make.names(levels(factor(train$Win.Loss)))
levels(test$Win.Loss) <- make.names(levels(factor(test$Win.Loss)))

# Setting up train controls
repeats = 3
numbers = 10
tunel = 10

set.seed(1234)
x = trainControl(method = 'repeatedcv',
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE,
                 summaryFunction = twoClassSummary)

model1 <- train(Win.Loss~. , data = train, method = 'knn',
                preProcess = c('center','scale'),
                trControl = x,
                metric = 'ROC',
                tuneLength = tunel)

# Summary of model
model1
plot(model1)

pred <- predict(model1,test, type = 'prob')

#Storing Model Performance Scores
pred_val <-prediction(pred[,2],test$Win.Loss)

# Calculating Area under Curve (AUC)

perf_val <- performance(pred_val, 'tpr', 'fpr')

#Calculating KS statistics
ks <- max(attr(perf_val, 'y.values')[[1]]-attr(perf_val, 'x.values')[[1]])

#### transform categorical variable

t <- function(x) {
    # check if x is numeric
    if(is.numeric(x)) {
        return (x)
    }
    l <- LabelEncoder.fit(x)
    y <- transform(l, x)
    return (y)
}

new_df <- sapply(df, t)

##########################################################################
### kNN with categorical variables

install.packages("FNN")
install.packages("gmodels")
install.packages("psych")
install.packages("mlr")
library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN) # estimate the minimal embedding dimension of a multivariate data set
library(gmodels) 
library(psych)
library(mlr)
data <- read.table("https://quantdev.ssri.psu.edu/sites/qdev/files/student-mat.csv", sep=";", header=TRUE)

#change all variable names to lowercase
colnames(data) <-tolower(colnames(data))

data_class <- data

# select target column
mjob_outcome <- data_class %>% select(mjob)
data_class <- data_class %>% select(-mjob)

# creste dummy variables
fac_var <- names(data_class)[sapply(data_class, is.factor)]

# scale data 
data_class_scale <- scale(data_class[,setdiff(names(data_class),fac_var)])
data_class_dumm <- createDummyFeatures(data_class, cols = fac_var)
str(data_class)

index <- sample(1:nrow(data_class), round(nrow(data_class)*.7), replace = FALSE)
train = cbind(data_class_scale,data_class_dumm)[index,]
train.target <- mjob_outcome[index,]
test = cbind(data_class_scale,data_class_dumm)[-index,]
test.target <- mjob_outcome[-index,]

mjob_pred_knn <- knn(train, test, train.target , k=5)

confusionMatrix(mjob_pred_knn, test.target)

### caret usage

# Setting up train controls
repeats = 3
numbers = 10
tunel = 10

set.seed(1234)
x = trainControl(method = 'repeatedcv',
                 number = numbers,
                 repeats = repeats,
                 classProbs = TRUE)

model1 <- train(train, train.target, method = 'knn',
                preProcess = c('center','scale'),
                trControl = x,
                metric = 'Accuracy',
                tuneLength = tunel)

# Summary of model
model1
plot(model1)

mjob_pred_caret <- train(train, train.target, method = "knn", preProcess = c("center","scale"))
plot(mjob_pred_caret)

### knn regression 
# https://quantdev.ssri.psu.edu/sites/qdev/files/kNN_tutorial.html












