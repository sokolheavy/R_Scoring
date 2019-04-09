https://www.r-bloggers.com/variable-selection-using-cross-validation-and-other-techniques/


# https://www.kaggle.com/ukkoa1/first-kernel-rf-w-boruta-selection-and-caret

install.packages("Boruta")

library(data.table)
library(caret)
library(Boruta)
library(knitr)

cat("Importing data...\n")

tr <- fread("../input/train.csv")
te <- fread("../input/test.csv")
sub <- fread("../input/sample_submission.csv")
sub[, target := 0]
tr$target <- as.factor(tr$target)
levels(tr$target) <- c("zero", "one")

# load data
work_data<-read.table("https://raw.githubusercontent.com/Srisai85/GermanCredit/master/german.data", h=F, sep="")
# Update column Names
colnames(work_data) <- c("chk_ac_status_1",
                         "duration_month_2", "credit_history_3", "purpose_4",
                         "credit_amount_5","savings_ac_bond_6","p_employment_since_7", 
                         "instalment_pct_8", "personal_status_9","other_debtors_or_grantors_10", 
                         "present_residence_since_11","property_type_12","age_in_yrs_13",
                         "other_instalment_type_14", "housing_type_15", 
                         "number_cards_this_bank_16","job_17","no_people_liable_for_mntnance_18",
                         "telephone_19", "foreign_worker_20", 
                         "good_bad_21")

# review variables
work_data <- data.frame(target = ifelse(work_data$good_bad_21 == 1, 1,0), work_data)
work_data$good_bad_21 <- NULL   


work_data$credit_history_3<-as.factor(ifelse(work_data$credit_history_3 == "A30", "01.A30",
                                             ifelse(work_data$credit_history_3 == "A31","02.A31",
                                                    ifelse(work_data$credit_history_3 == "A32","03.A32.A33",
                                                           ifelse(work_data$credit_history_3 == "A33","03.A32.A33",
                                                                  "04.A34")))))


work_data$duration_month_2 <-as.factor(ifelse(work_data$duration_month_2<=6,'00-06',
                                          ifelse(work_data$duration_month_2<=12,'06-12',
                                                 ifelse(work_data$duration_month_2<=24,'12-24', 
                                                        ifelse(work_data$duration_month_2<=30,'24-30',
                                                               ifelse(work_data$duration_month_2<=36,'30-36',
                                                                      ifelse(work_data$duration_month_2<=42,'36-42','42+')))))))



work_data$credit_amount_5<-as.factor(ifelse(work_data$credit_amount_5<=1400,'0-1400',
                                        ifelse(work_data$credit_amount_5<=2500,'1400-2500',
                                               ifelse(work_data$credit_amount_5<=3500,'2500-3500', 
                                                      ifelse(work_data$credit_amount_5<=4500,'3500-4500',
                                                             ifelse(work_data$credit_amount_5<=5500,'4500-5500','5500+'))))))




work_data$age_in_yrs_13 <- as.factor(ifelse(work_data$age_in_yrs_13<=25, '0-25',
                                        ifelse(work_data$age_in_yrs_13<=30, '25-30',
                                               ifelse(work_data$age_in_yrs_13<=35, '30-35', 
                                                      ifelse(work_data$age_in_yrs_13<=40, '35-40', 
                                                             ifelse(work_data$age_in_yrs_13<=45, '40-45', 
                                                                    ifelse(work_data$age_in_yrs_13<=50, '45-50',
                                                                           ifelse(work_data$age_in_yrs_13<=60, '50-60',
                                                                                  '60+'))))))))


names(work_data)
work_data$instalment_pct_8 <- as.factor(work_data$instalment_pct_8)
work_data$present_residence_since_11 <- as.factor(work_data$present_residence_since_11)
work_data$no_people_liable_for_mntnance_18 <- as.factor(work_data$no_people_liable_for_mntnance_18)
work_data$number_cards_this_bank_16 <- as.factor(work_data$number_cards_this_bank_16)
work_data$target <- as.factor(work_data$target)
unique(work_data$number_cards_this_bank_16)
str(work_data)

# some correction to the data

colnames(initial_data) <- c('target',
                            "chk_ac_status1",
                            "duration_month2", 
                            "credit_history3",
                            "purpose4",
                            "credit_amount5",
                            "savings_ac_bond6",
                            "p_employment_since7", 
                            "instalment_pct8", 
                            "personal_status9",
                            "other_debtors_or_grantors10", 
                            "present_residence_since_11",
                            "property_type12",
                            "age_in_yrs13",
                            "other_instalment_type14",
                            "housing_type15", 
                            "number_cards_this_bank16",
                            "job_17",
                            "no_people_liable_for_mntnance18",
                            "telephone19", 
                            "foreign_worker20")

initial_data$credit_history3 <- as.factor(gsub("\\.", "_", initial_data$credit_history3))
initial_data$credit_amount5 <- as.factor(gsub("-", "_", initial_data$credit_amount5))
initial_data$age_in_yrs13 <- as.factor(gsub("-", "_", initial_data$age_in_yrs13))
initial_data$duration_month2 <- as.factor(gsub("-", "_", initial_data$duration_month2))

str(initial_data)

ind <- sample(c(1,2), nrow(initial_data), replace = T, prob = c(.8, .2))
train <- initial_data[ind==1,]
test <- initial_data[ind==2,]

options(scipen = 99)
kable(data.frame(obs = nrow(train),
                 qty_NA = colSums(sapply(train, is.na)),
                 prop_NA = colSums(sapply(train, is.na))/nrow(train)), digits = 3)


cat("Selecting important features...\n")

bor<-  Boruta(target~.,
              data = train,
              maxRuns = 1000,
              num.trees = 5501)
bor <- TentativeRoughFix(bor)
bor.form <- getConfirmedFormula(bor)



cat("Training model...\n")

tc <- trainControl(method = "repeatedcv",
                   number = 5,
                   repeats = 10,
                   classProbs = TRUE,
                   summaryFunction = twoClassSummary,
                   search = "grid",
                   savePredictions = "final")

rf <- train(bor.form,
            data = train,
            trControl = tc,
            method = "ranger",
            tuneGrid = expand.grid(mtry = c(1,3,5),
                                   min.node.size = 1,
                                   splitrule = c("gini","extratrees")),
            
            num.trees = 5501,
            metric = "ROC")

str(train)

preds <- predict(rf, te, type = "prob")

sub[, target := preds[,2]]
fwrite(sub, "submission.csv")


str(train)



################ 

# second variant 

################ 




################ 

# third variant 

################ 

M <- cor(train)
correlation_threshold <- 0.14
significant_correlation <- abs(M["target", ]) > correlation_threshold
table(significant_correlation)

corr_colnames <- colnames(M)[significant_correlation]

# Plotting the selected variables correlations
corrplot(M[significant_correlation, significant_correlation], 
         method = "color",
         type = "upper")

# Linear regression model with selected features, 10 K-fold cross validation using the "caret" package --------------
set.seed(123)

# Preparing data for model
model_dat <- train_dat[, corr_colnames] %>% select(-id)

model_dat$target <- factor(model_dat$target)
levels(model_dat$target) <- make.names(model_dat$target)

# Training parameters
ctrl <- trainControl(method = "cv", 
                     number = 10, 
                     savePredictions = TRUE,
                     classProbs = TRUE)

# Fitting model
mod_fit <- train(target ~ ., data=model_dat, 
                 method="glm", 
                 family="binomial",
                 trControl = ctrl, 
                 tuneLength = 5)

# Look at features
summary(mod_fit)

# Calculate AUC (competition metric)
auc(model_dat$target, mod_fit$pred$X1)


