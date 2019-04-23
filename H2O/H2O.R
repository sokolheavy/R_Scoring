library(tibble)
library(purrr)
library(data.table)
library(dplyr)
library(magrittr)
library(ggplot)
library(knitr)

# h2o modeling kit
library(h2o)
library(caret)
library(reshape2)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, skimr, GGally, plotly, viridis, 
               caret, DT, data.table, lightgbm)

library(GGally)

graph <- list()

for (i in 1:2){
  
  graph[[i]] <- train[, sapply(train, is.numeric)] %>% na.omit() %>%
    select(TARGET,((i-1)*5+1):((i-1)*5+5)) %>%
    mutate(TARGET = factor(TARGET)) %>%
    ggpairs(aes(col = TARGET, alpha=.4))
  
  print(graph[[i]])
}

### H20


# filename 
fl1 <- 'application_train.csv'
fl2 <- 'POS_CASH_balance.csv'
fl3 <- 'bureau_balance.csv'
fl4 <- 'previous_application.csv'
fl5 <- 'installments_payments.csv'
fl6 <- 'credit_card_balance.csv'
fl7 <- 'bureau.csv'
fl8 <- 'credit_card_balance.csv'
fl9 <- 'application_test.csv'

setwd("C:/Users/EASokol/Desktop/credit_h2o_eda/input")
# load train.csv
DstTrain <- fread(fl1, stringsAsFactors = FALSE, na.strings = c("NA", ""))
ind <- sample(nrow(DstTrain),5000)
DstTrain <- DstTrain[ind,]
dim(DstTrain)


# Unique values per column
GetUniques <- apply(DstTrain,2,function(x) length(unique(x)))
# vars to drop
ConstantColToDrop <- -which(names(DstTrain) %in% rownames(data.frame(GetUniques[GetUniques == 1])))
# just check how many we drop                    
print(sprintf("Goodbye for %s columns because they have a constante value.", length(ConstantColToDrop)))
# drop them                                        
if (length(ConstantColToDrop) > 0) {
  DstTrain <- DstTrain[,ConstantColToDrop]
}


#??????? check missing data
missDataCols <- as.data.frame(sapply(DstTrain, function(x) sum(is.na(x))))
missDataCols <- rownames_to_column(missDataCols, "cols")
colnames(missDataCols) <- c("cols","miss")                                    
# print(sprintf("Goodbye for %s columns because they have missing values (for this version).", length(missDataCols[missDataCols$miss != 0,]$cols)))                                     
head(missDataCols[missDataCols$miss != 0,])

######
# Remove variables with low var
#####

# diagnoses predictors that are have both of the following characteristics: 
#       they have very few unique values relative to the number of samples and 
#       the ratio of the frequency of the most common value to the frequency of the second most common 
nearZeroVars <- nearZeroVar(DstTrain, freqCut = 95/5, uniqueCut = 10, saveMetrics = TRUE)

print(sprintf("Goodbye for %s columns because they have a nearZeroVar.", length(nearZeroVars)))
# drop cols
colnames(DstTrain[,names(DstTrain)[nearZeroVars])
DstTrain <- select(DstTrain,-nearZeroVars)

# define as factor
DstTrain$TARGET <- as.factor(DstTrain$TARGET)
levels(DstTrain$TARGET) = make.names(unique(DstTrain$TARGET))


for (i in nearZeroVars){
  plot <- ggplot(DstTrain, aes(as.factor(names(DstTrain)[i]), fill = as.factor(names(DstTrain)[i]))) +
    geom_bar() +
    xlab(names(DstTrain)[i]) +
    theme(legend.position = "none")
  print(plot)
}



###
# Remove columns with constante value in all dataset
###

# Unique values per column
GetUniques <- apply(DstTrain,2,function(x) length(unique(x)))
# vars to drop
ConstantColToDrop <- which(names(DstTrain) %in% rownames(data.frame(GetUniques[GetUniques == 1])))
# just check how many we drop                    
print(sprintf("Goodbye for %s columns because they have a constante value.", length(ConstantColToDrop)))
# drop them                                        
if (length(ConstantColToDrop) > 0) {
  DstTrain <- DstTrain[,-ConstantColToDrop]
}



# convert to h2o frame 
h2o.init(nthreads=-1, max_mem_size="4G")
h2o.removeAll()    # clean slate - just in case the cluster was already running
h2o.no_progress()  # Don't show progress bars in RMarkdown output

h2o_TrainAux = as.h2o(DstTrain)

# define the splits
h2o_splits <- h2o.splitFrame(h2o_TrainAux, 0.8, seed=1234)
h2o_DstTrain  <- h2o.assign(h2o_splits[[1]], "train.hex") # 80%
h2o_DstTest  <- h2o.assign(h2o_splits[[2]], "test.hex") # 20%

# Identify predictors and response
response <- "TARGET"
predictors <- setdiff(names(h2o_DstTrain), response)

# Number of CV folds (to generate level-one data for stacking)
cvfolds <- 5

get_auc <- function(mm) h2o.auc(h2o.performance(h2o.getModel(mm), newdata = h2o_DstTest))

# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = predictors,
                  y = response,
                  training_frame = h2o_DstTrain,
                  distribution = "bernoulli",
                  max_depth = 3,
                  min_rows = 2,
                  learn_rate = 0.2,
                  nfolds = cvfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

# measure performance
get_auc(my_gbm@model_id)

# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = predictors,
                          y = response,
                          training_frame = h2o_DstTrain,
                          nfolds = cvfolds,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)

# measure performance
get_auc(my_rf@model_id)

# Train & Cross-validate a DNN
my_dl <- h2o.deeplearning(x = predictors,
                          y = response,
                          training_frame = h2o_DstTrain,
                          l1 = 0.001,
                          l2 = 0.001,
                          hidden = c(200, 200, 200),
                          nfolds = cvfolds,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)

# measure performance
get_auc(my_dl@model_id)


# Train & Cross-validate a (shallow) XGB-GBM
my_xgb1 <- h2o.xgboost(x = predictors,
                       y = response,
                       training_frame = h2o_DstTrain,
                       distribution = "bernoulli",
                       ntrees = 50,
                       max_depth = 3,
                       min_rows = 2,
                       learn_rate = 0.2,
                       nfolds = cvfolds,
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)

# measure performance
get_auc(my_xgb1@model_id)

# Train & Cross-validate another (deeper) XGB-GBM
my_xgb2 <- h2o.xgboost(x = predictors,
                       y = response,
                       training_frame = h2o_DstTrain,
                       distribution = "bernoulli",
                       ntrees = 50,
                       max_depth = 8,
                       min_rows = 1,
                       learn_rate = 0.1,
                       sample_rate = 0.7,
                       col_sample_rate = 0.9,
                       nfolds = cvfolds,
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)

# measure performance
get_auc(my_xgb2@model_id)

# Train a stacked ensemble using the H2O and XGBoost models from above
base_models <- list(my_gbm@model_id, my_rf@model_id, my_dl@model_id)

ensemble <- h2o.stackedEnsemble(x = predictors,
                                y = response,
                                training_frame = h2o_DstTrain,
                                base_models = base_models)

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = h2o_DstTest)


# Compare to base learner performance on the test set
baselearner_aucs <- sapply(base_models, get_auc)
baselearner_best_auc_test <- max(baselearner_aucs)
ensemble_auc_test <- h2o.auc(perf)

print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))

### Final submition

# load test.csv
DstTest  <- read.csv(fl9 , stringsAsFactors = FALSE, na.strings = c("NA", ""))

# to keep
ToKeep <- which(names(DstTest) %in% predictors)

# save the ID
TestIDs <- DstTest$SK_ID_CURR
# keep only the good ones
DstTest <- DstTest[,ToKeep]

# convert to h2o frame 
h2o_FinalTest = as.h2o(DstTest)

# predict with the model
predictFinal <- h2o.predict(ensemble, h2o_FinalTest)

# convert H2O format into data frame and save as csv
predictFinal.df <- as.data.frame(predictFinal)

# create a csv file for submittion
Result <- data.frame(SK_ID_CURR = TestIDs, TARGET = predictFinal.df$X0)
head(Result,n=5L)
# write the submition file
write.csv(Result,file = "Result.csv",row.names = FALSE)



############################# Add another files ############################# 
# The bureau.csv 
# load file
DstBureau <- read.csv(fl7, stringsAsFactors = FALSE, na.strings = c("NA", ""))
dim(DstBureau)

# Create the features for this file
DstBureauSum <- DstBureau %>% 
  select(
    SK_ID_CURR, CREDIT_ACTIVE, DAYS_CREDIT, CREDIT_DAY_OVERDUE, 
    AMT_CREDIT_SUM, AMT_CREDIT_SUM_DEBT, AMT_CREDIT_SUM_LIMIT
  ) %>% 
  group_by(SK_ID_CURR) %>% 
  mutate(
    CREDIT_CLOSED = sum(ifelse(CREDIT_ACTIVE == 'Closed',1,0)),
    CREDIT_ACTIVE = sum(ifelse(CREDIT_ACTIVE == 'Active',1,0)),
    AMT_CREDIT_CLOSED = sum(ifelse(CREDIT_ACTIVE == 'Closed',AMT_CREDIT_SUM,0)),
    AMT_CREDIT_ACTIVE = sum(ifelse(CREDIT_ACTIVE == 'Active',AMT_CREDIT_SUM,0)),
    AMT_CREDIT_DEBT_CLOSED = sum(ifelse(CREDIT_ACTIVE == 'Closed',AMT_CREDIT_SUM_DEBT,0)),
    AMT_CREDIT_DEBT_ACTIVE = sum(ifelse(CREDIT_ACTIVE == 'Active',AMT_CREDIT_SUM_DEBT,0)),
    AMT_CREDIT_LIMIT_CLOSED = sum(ifelse(CREDIT_ACTIVE == 'Closed',AMT_CREDIT_SUM_DEBT,0)),
    AMT_CREDIT_LIMIT_ACTIVE = sum(ifelse(CREDIT_ACTIVE == 'Active',AMT_CREDIT_SUM_DEBT,0))
  ) %>% 
  summarize(
    NCredit = n(),
    MAX_DAYS_CREDIT = max(abs(DAYS_CREDIT)),
    CREDIT_DAY_OVERDUE = sum(CREDIT_DAY_OVERDUE),
    CREDIT_CLOSED = sum(CREDIT_CLOSED),
    CREDIT_ACTIVE = sum(CREDIT_ACTIVE),
    AMT_CREDIT_CLOSED = sum(AMT_CREDIT_CLOSED),
    AMT_CREDIT_ACTIVE = sum(AMT_CREDIT_ACTIVE),
    AMT_CREDIT_DEBT_CLOSED = sum(AMT_CREDIT_DEBT_CLOSED),
    AMT_CREDIT_DEBT_ACTIVE = sum(AMT_CREDIT_DEBT_ACTIVE),
    AMT_CREDIT_LIMIT_CLOSED = sum(AMT_CREDIT_LIMIT_CLOSED),
    AMT_CREDIT_LIMIT_ACTIVE = sum(AMT_CREDIT_LIMIT_ACTIVE)
  )


### The credit_card_balance.csv File¶
# load file
DstCreditCard <- read.csv(fl6, stringsAsFactors = FALSE, na.strings = c("NA", ""))
dim(DstCreditCard)

head(DstCreditCard[DstCreditCard$NAME_CONTRACT_STATUS != 'Active',
                   which(names(DstCreditCard)
                          %in% colnames(DstCreditCard[,grep('^AMT_',colnames(DstCreditCard))]))])


DstCreditCardSum <- DstCreditCard %>%
  select(
    SK_ID_CURR, NAME_CONTRACT_STATUS, CNT_DRAWINGS_ATM_CURRENT, CNT_DRAWINGS_CURRENT,
    CNT_DRAWINGS_OTHER_CURRENT, CNT_DRAWINGS_POS_CURRENT, CNT_INSTALMENT_MATURE_CUM,
    AMT_BALANCE, AMT_CREDIT_LIMIT_ACTUAL, AMT_DRAWINGS_ATM_CURRENT, AMT_DRAWINGS_CURRENT,
    AMT_DRAWINGS_OTHER_CURRENT, AMT_DRAWINGS_POS_CURRENT, AMT_INST_MIN_REGULARITY,
    AMT_PAYMENT_CURRENT
  )%>% 
  group_by(SK_ID_CURR) %>% 
  mutate(
    CREDITCARD_COMPLETED = ifelse(NAME_CONTRACT_STATUS == 'Completed',1,0),
    CREDITCARD_ACTIVE = ifelse(NAME_CONTRACT_STATUS == 'Active',1,0),
    AMT_BALANCE_COMPLETED = ifelse(NAME_CONTRACT_STATUS == 'Completed',AMT_BALANCE,0),
    AMT_BALANCE_ACTIVE = ifelse(NAME_CONTRACT_STATUS == 'Active',AMT_BALANCE,0),
    AMT_PAYMENT_CURRENT_COMPLETED = ifelse(NAME_CONTRACT_STATUS == 'Completed',AMT_PAYMENT_CURRENT,0),
    AMT_PAYMENT_CURRENT_ACTIVE = ifelse(NAME_CONTRACT_STATUS == 'Active',AMT_PAYMENT_CURRENT,0)
  ) %>% 
  summarize(
    NCreditCard = n(),
    CREDITCARD_COMPLETED = sum(CREDITCARD_COMPLETED, na.rm = TRUE),
    CREDITCARD_ACTIVE = sum(CREDITCARD_ACTIVE, na.rm = TRUE),
    CNT_DRAWINGS_ATM_CURRENT = mean(CNT_DRAWINGS_ATM_CURRENT, na.rm = TRUE),
    CNT_DRAWINGS_CURRENT = mean(CNT_DRAWINGS_CURRENT, na.rm = TRUE),
    CNT_DRAWINGS_OTHER_CURRENT = mean(CNT_DRAWINGS_OTHER_CURRENT, na.rm = TRUE),
    CNT_DRAWINGS_POS_CURRENT = mean(CNT_DRAWINGS_POS_CURRENT, na.rm = TRUE),
    CNT_INSTALMENT_MATURE_CUM = mean(CNT_INSTALMENT_MATURE_CUM, na.rm = TRUE),
    AMT_BALANCE_COMPLETED = sum(AMT_BALANCE_COMPLETED, na.rm = TRUE),
    AMT_BALANCE_ACTIVE = sum(AMT_BALANCE_ACTIVE, na.rm = TRUE),
    AMT_CREDIT_LIMIT_ACTUAL = mean(AMT_CREDIT_LIMIT_ACTUAL, na.rm = TRUE),
    AMT_DRAWINGS_ATM_CURRENT = mean(AMT_DRAWINGS_ATM_CURRENT, na.rm = TRUE),
    AMT_DRAWINGS_CURRENT = mean(AMT_DRAWINGS_CURRENT, na.rm = TRUE),
    AMT_DRAWINGS_OTHER_CURRENT = mean(AMT_DRAWINGS_OTHER_CURRENT, na.rm = TRUE),
    AMT_DRAWINGS_POS_CURRENT = mean(AMT_DRAWINGS_POS_CURRENT, na.rm = TRUE),
    AMT_INST_MIN_REGULARITY = mean(AMT_INST_MIN_REGULARITY, na.rm = TRUE),
    AMT_PAYMENT_CURRENT_COMPLETED = sum(AMT_BALANCE_COMPLETED, na.rm = TRUE),
    AMT_PAYMENT_CURRENT_ACTIVE = sum(AMT_BALANCE_ACTIVE, na.rm = TRUE)
  )


### The POS_CASH_balance.csv File
# load file
DstPosCash <- read.csv(fl2, stringsAsFactors = FALSE, na.strings = c("NA", ""))
dim(DstPosCash)

DstContractStatus <- DstPosCash %>%
  group_by(SK_ID_CURR, NAME_CONTRACT_STATUS) %>% 
  summarize(value = n())

DstContractStatus <- dcast(DstContractStatus, SK_ID_CURR ~ NAME_CONTRACT_STATUS, value.var="value", fill = 0 )


DstPosCashSum <- DstPosCash %>%
  group_by(SK_ID_CURR) %>% 
  summarize(
    CNT_INSTALMENT = mean(CNT_INSTALMENT, na.rm = TRUE),
    CNT_INSTALMENT_FUTURE = mean(CNT_INSTALMENT_FUTURE, na.rm = TRUE),
    NPosCash = n()
  ) %>%
  inner_join(DstContractStatus, by="SK_ID_CURR")


### The previous_application.csv File
# load file
DstPrevApp <- read.csv(fl4, stringsAsFactors = FALSE, na.strings = c("NA", ""))
dim(DstPrevApp)

DstContractStatus <- DstPrevApp %>%
  group_by(SK_ID_CURR, NAME_CONTRACT_STATUS) %>% 
  summarize(value = n())

DstContractStatus <- dcast(DstContractStatus, SK_ID_CURR ~ NAME_CONTRACT_STATUS, value.var="value", fill = 0 )

DstClientType <- DstPrevApp %>%
  group_by(SK_ID_CURR, NAME_CLIENT_TYPE) %>% 
  summarize(value = n())

DstClientType <- dcast(DstClientType, SK_ID_CURR ~ NAME_CLIENT_TYPE, value.var="value", fill = 0 )

DstPrevAppSum <- DstPrevApp %>%
  group_by(SK_ID_CURR) %>% 
  summarize(
    AMT_ANNUITY = mean(AMT_ANNUITY, na.rm = TRUE),
    AMT_APPLICATION = mean(AMT_APPLICATION, na.rm = TRUE),
    AMT_CREDIT = mean(AMT_CREDIT, na.rm = TRUE),
    AMT_DOWN_PAYMENT = mean(AMT_DOWN_PAYMENT, na.rm = TRUE),
    AMT_GOODS_PRICE = mean(AMT_GOODS_PRICE, na.rm = TRUE),
    NPrevApp = n()
  ) %>%
  inner_join(DstContractStatus, by="SK_ID_CURR") %>%
  inner_join(DstClientType, by="SK_ID_CURR")


DstTrain <- DstTrain %>%
  left_join(DstBureauSum, by="SK_ID_CURR") %>%
  left_join(DstCreditCardSum, by="SK_ID_CURR") %>%
  left_join(DstPosCashSum, by="SK_ID_CURR") %>%
  left_join(DstPrevAppSum, by="SK_ID_CURR")

####### build model...(all the same as previous dataset)
h2o.init(nthreads=-1, max_mem_size="8G")
h2o.removeAll()    # clean slate - just in case the cluster was already running
h2o.no_progress()  # Don't show progress bars in RMarkdown output



