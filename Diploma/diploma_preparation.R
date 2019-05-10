https://www.kaggle.com/quinn126/predicting-german-credit-risk-logit-reg-xgboost
https://www.kaggle.com/uciml/german-credit/kernels?sortBy=hotness&group=everyone&pageSize=20&datasetId=531&language=R
https://rpubs.com/akulmahajan/366429
https://rpubs.com/Hgoswami/368878
https://www.kaggle.com/bisaria/titanic-lasso-ridge-implementation#lasso-and-ridge-models

work_data_first <- work_data

work_data$age_in_yrs_13 <- as.factor(ifelse(work_data$age_in_yrs_13<=25, '0-25',
                                            ifelse(work_data$age_in_yrs_13<=30, '25-30',
                                                   ifelse(work_data$age_in_yrs_13<=35, '30-35', 
                                                          ifelse(work_data$age_in_yrs_13<=40, '35-40', 
                                                                 ifelse(work_data$age_in_yrs_13<=45, '40-45', 
                                                                        ifelse(work_data$age_in_yrs_13<=50, '45-50',
                                                                               ifelse(work_data$age_in_yrs_13<=60, '50-60',
                                                                                      '60+'))))))))

work_data$credit_amount_5<-as.factor(ifelse(work_data$credit_amount_5<=1400,'0-1400',
                                            ifelse(work_data$credit_amount_5<=2500,'1400-2500',
                                                   ifelse(work_data$credit_amount_5<=3500,'2500-3500', 
                                                          ifelse(work_data$credit_amount_5<=4500,'3500-4500',
                                                                 ifelse(work_data$credit_amount_5<=5500,'4500-5500','5500+'))))))



# Create some groups from contious variables
work_data$duration_month_2 <-as.factor(ifelse(work_data$duration_month_2<=6,'00-06',
                                              ifelse(work_data$duration_month_2<=12,'06-12',
                                                     ifelse(work_data$duration_month_2<=24,'12-24', 
                                                            ifelse(work_data$duration_month_2<=30,'24-30',
                                                                   ifelse(work_data$duration_month_2<=36,'30-36',
                                                                          ifelse(work_data$duration_month_2<=42,'36-42','42+')))))))



keep<- c(1:8,12,13,21)
cdata_reduced_2 <- work_data[,keep]

div_part_1 <- createDataPartition(y = cdata_reduced_2$target, p = 0.7, list = F)

# Training Sample
train <- cdata_reduced_2[div_part_1,] # 70% here

# Test Sample
test <- cdata_reduced_2[-div_part_1,] # rest of the 30% data goes here
table(test$target)






work_data <- work_data[,c(1:5,13,14)]
set.seed(123)
train<-work_data[sort(sample(nrow(work_data), nrow(work_data)*.7)),] # 70% here
test<-work_data[-sort(sample(nrow(work_data), nrow(work_data)*.7)),] # rest of the 30% data goes here


m2 <- rpart(target~.,data=train,method=	"class")

# score test data
test$m2_score <- predict(m2,test)
m2_pred <- prediction(test$m2_score,test$target)
m2_perf <- performance(m2_pred,"tpr","fpr")


test$m2_score <- predict(m2,type='prob',test)
m2_pred <- prediction(test$m2_score[,2],test$target)
m2_perf <- performance(m2_pred,"tpr","fpr")




ev_df_m2 <- data.frame(Gini = round(((slot(performance(m2_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(m2_pred, measure = "auc")@y.values[[1]]*100, 2))
ev_df_m2

ev_df_m2 <- ggtexttable(ev_df_m2, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))


m1_gini_plot <- ggplot(setNames(data.frame(m2_perf@x.values, m2_perf@y.values), c('x_val', 'y_val')), 
                       aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")









# work_data

# Partition data
set.seed(1234)
ind <- sample(2, nrow(work_data), replace = T, prob = c(0.8, 0.2))
train <- work_data[ind==1,]
test <- work_data[ind==2,]

# Create matrix - One-Hot Encoding for Factor variables
trainm <- sparse.model.matrix(target ~ .-1, data = train)
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

names(train_num)

# add values for perfomance plots(only for calc)
pred <- prediction(pred_target, train[-s[[k]], measurevar])

# Prediction & confusion matrix - test data
p <- predict(bst_model, newdata = test_matrix, type = "response")

pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(., "last")-1)

auc <- mean(pred$label==pred$max_prob)
gini 





### second iteration 
trainm <- sparse.model.matrix(target ~ ., data = train_num)
train_label <- train_num[,"target"]
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

testm <- sparse.model.matrix(target~., data = test_num)
test_label <- test_num[,"target"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

xgb = xgboost(data = train_matrix, 
              eta = 0.1,
              max_depth = 5, 
              gamma = 0,
              objective = "binary:logistic",
              nrounds = 15, 
              subsample = 0.8,
              colsample_bytree = 0.8,
              seed = 1,
              eval_metric = "error")

par(mar=c(1,1,1,1))
# Training & test error plot
e <- data.frame(xgb$evaluation_log)



png(filename="GBM2.png", res=150, width = 1000, height = 1000)
plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')
dev.off()



imp <- xgb.importance(colnames(train_matrix), model = bst_model)
print(imp)

xgb.plot.importance(imp)

pred_target <- predict(object = model, type = "response")

# add values for perfomance plots(only for calc)
pred <- prediction(pred_target, train[-s[[k]], measurevar])

# Prediction & confusion matrix - test data
p <- predict(xgb, newdata = test_matrix, type = "response")

pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(., "last")-1)

auc <- mean(pred$label==pred$max_prob)
gini <- auc*2 -1
gini




























# https://www.edureka.co/blog/clustering-on-bank-data-using-r/
# https://towardsdatascience.com/how-to-cluster-your-customer-data-with-r-code-examples-6c7e4aa6c5b1  
# https://www.kaggle.com/btremaine/titanic-random-forest-using-cforest
# https://www.r-bloggers.com/a-brief-tour-of-the-trees-and-forests/ - about trees
# https://stats.stackexchange.com/questions/12140/conditional-inference-trees-vs-traditional-decision-trees - about trees
# https://stackoverflow.com/questions/31314153/decision-tree-completely-different-between-rpart-and-party-package 
# https://www.r-bloggers.com/five-steps-for-missing-data-with-finalfit/
# https://basegroup.ru/community/articles/missing
# https://statistics.ohlsen-web.de/multiple-imputation-with-mice/
# install.packages("caroline") 


library(plyr)
library(dplyr)
library(tibble)
library(tidyr)
library(tibble)
library(tidyverse)
library(knitr)

library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(gtable)
library(gridExtra)
library(corrplot)
library(grid)

library(caret)
library(caretEnsemble)
library(mice)
library(classInt)
library(RColorBrewer)
library(party)
library(rpart)
library(rpart.utils)
library(rpart.plot)
library(C50)
library(randomForest)
library(lattice)
library(ape)
library(kernlab)
library(lars)
library(glmnet)
library(caroline)
library(finalfit) 
library(ROCR)
library(MLmetrics)
library(VIM)
library(GGally)
library(MissMech)
library(InformationValue)
library(smbinning)
library(xgboost)
library(caroline)


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


work_data$duration_month_2  <- as.numeric(work_data$duration_month_2)
work_data$credit_amount_5   <-  as.numeric(work_data$credit_amount_5 )
work_data$instalment_pct_8 <-  as.factor(work_data$instalment_pct_8)
work_data$present_residence_since_11 <-  as.factor(work_data$present_residence_since_11)
work_data$age_in_yrs_13        <-  as.numeric(work_data$age_in_yrs_13)
work_data$number_cards_this_bank_16    <-  as.factor(work_data$number_cards_this_bank_16)
work_data$no_people_liable_for_mntnance_18 <-  as.factor(work_data$no_people_liable_for_mntnance_18)
work_data$credit_history_3<-as.factor(ifelse(work_data$credit_history_3 == "A30", "01.A30",
                                             ifelse(work_data$credit_history_3 == "A31","02.A31",
                                                    ifelse(work_data$credit_history_3 == "A32","03.A32.A33",
                                                           ifelse(work_data$credit_history_3 == "A33","03.A32.A33", "04.A34")))))
work_data <- data.frame(target = ifelse(work_data$good_bad_21 == 1, 1,0), work_data)
work_data$good_bad_21 <- NULL 

str(work_data)

sapply(work_data[,sapply(work_data, is.numeric)], unique)


set.seed(321)
ind_housing_type <- sample(1:1000, 15)
ind_job <- sample(1:1000, 21)
ind_number_cards_this_bank <- sample(1:1000, 23)

work_data[ind_housing_type, 16] <- NA
work_data[ind_job, 18] <- NA
work_data[ind_number_cards_this_bank, 17] <- NA

set.seed(651)
ind_housing_type2 <- sample(1:1000, 21)
ind_job2 <- sample(1:1000, 21)
ind_number_cards_this_bank2 <- sample(1:1000, 55)

work_data[ind_housing_type2, 16] <- NA
work_data[ind_job2, 18] <- NA
work_data[ind_number_cards_this_bank2, 17] <- NA


# [%,#] missing
options(scipen = 99)
kable(data.frame(obs = nrow(work_data),
                 qty_NA = colSums(sapply(work_data, is.na)),
                 prop_NA = colSums(sapply(work_data, is.na))/nrow(work_data)), digits = 3)

work_data <-
  work_data %>%
  mutate(housing_type_15 = recode(housing_type_15, 
                                  A151 = "RENT",
                                  A152 = "OWN",
                                  A153 = "RELATIVES_PROPERTY"))

work_data <-
  work_data %>%
  mutate(job_17 = recode(job_17, 
                         A171 = "PENSIONER",
                         A172 = "OWN_BUSINESS",
                         A173 = "HIRED_EMPLOYEE",
                         A174 = "NOT_OFFICIAL"))



explanatory = c("housing_type_15", "chk_ac_status_1", "job_17")
dependent = "target"
work_data %>% 
  missing_pairs(dependent, explanatory)


ggplot(work_data, aes(housing_type_15)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
  scale_y_continuous(labels=scales::percent) + 
  geom_text(aes( y = ((..count..)/sum(..count..)),
                 label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
  theme(axis.text.x = element_text(angle=10, vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
  labs( y = "", x = "Type of house")


ggplot(work_data, aes(job_17)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
  scale_y_continuous(labels=scales::percent) + 
  geom_text(aes( y = ((..count..)/sum(..count..)),
                 label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
  theme(axis.text.x = element_text(angle=10, vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
  labs( y = "", x = "Type of job")


ggplot(work_data, aes(number_cards_this_bank_16)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
  scale_y_continuous(labels=scales::percent) + 
  geom_text(aes( y = ((..count..)/sum(..count..)),
                 label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
  theme(axis.text.x = element_text(angle=10, vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
  labs( y = "", x = "Number of cards")




work_data %>%
  missing_plot()

### Type of house
# density of na 
ggplot(work_data[is.na(work_data[,16]),], aes(x=target)) + 
  geom_density(aes(group=target, colour=target, fill=target), alpha=0.3) +
  labs(x = "Type of house") +
  theme(legend.position ="none")

# density w/o na
ggplot(work_data[is.na(work_data[,16])==F,], aes(x=target)) + 
  geom_density(aes(group=target, colour=target, fill=target), alpha=0.3) +
  labs(x = "Type of house") +
  theme(legend.position ="none")  


### Type of job
# density of na 
ggplot(work_data[is.na(work_data[,17]),], aes(x=target)) + 
  geom_density(aes(group=target, colour=target, fill=target), alpha=0.3) +
  labs(x = "Type of house") +
  theme(legend.position ="none")

# density w/o na
ggplot(work_data[is.na(work_data[,17])==F,], aes(x=target)) + 
  geom_density(aes(group=target, colour=target, fill=target), alpha=0.3) +
  labs(x = "Type of house") +
  theme(legend.position ="none")  



### Type of house
# density of na 
ggplot(work_data[is.na(work_data[,18]),], aes(x=target)) + 
  geom_density(aes(group=target, colour=target, fill=target), alpha=0.3) +
  labs(x = "Type of house") +
  theme(legend.position ="none")

# density w/o na
ggplot(work_data[is.na(work_data[,18])==F,], aes(x=target)) + 
  geom_density(aes(group=target, colour=target, fill=target), alpha=0.3) +
  labs(x = "Type of house") +
  theme(legend.position ="none")  


### chisq.test
j <- 17
agg_table <- data.frame(true = table(work_data[is.na(work_data[,16])==F, 1]),
                        na = table(work_data[is.na(work_data[,16]), 1]))


ch_table <- data.frame()
for (j in c(16,17,18)){
  pre_ch_table <- data.frame(true = data.frame(table(work_data[is.na(work_data[,j])==F, 1]))[,2],
                             missing = data.frame(table(work_data[is.na(work_data[,j]), 1]))[,2])
  
  # set chisq.test value and p_value
  chisq <- round(as.data.frame(chisq.test(pre_ch_table)[1])[1,1], 3)
  p_value_chisq <- round(as.data.frame(chisq.test(pre_ch_table)[3])[1,1], 5)
  
  ch_table <- union_all(ch_table, data.frame(variable = names(work_data)[j],
                                             chisq_value = chisq,
                                             p_value = p_value_chisq))
}


#### NA imputation
install.packages("lattice")
require(mice)
require(lattice)

ini <- mice(work_data[, -1], maxit=0, print=F)
meth<- ini$meth
meth

pred <- ini$pred
View(pred)
pas.imp <- mice(work_data[, -1], meth=meth, pred=pred, maxit=10, seed=123, print=F)
completedData <- complete(pas.imp)
work_data <- data.frame(target = work_data[,1], completedData)
work_data_if_smth_go_wrong <- work_data 


densityplot(work_data)
stripplot(work_data, pch = 20, cex = 1.2)

ggplot(work_data, aes(job_17)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
  scale_y_continuous(labels=scales::percent) + 
  geom_text(aes( y = ((..count..)/sum(..count..)),
                 label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
  theme(axis.text.x = element_text(angle=10, vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
  labs( y = "", x = "Type of job")


ggplot(completedData, aes(job_17)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
  scale_y_continuous(labels=scales::percent) + 
  geom_text(aes( y = ((..count..)/sum(..count..)),
                 label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
  theme(axis.text.x = element_text(angle=10, vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
  labs( y = "", x = "Type of job")

#### bining

################ Create binning data 

# initial number of columns
begin_ncol <- ncol(work_data)

#initiate target column and first column with variables
target_ncol <- match("target",names(work_data))

variable_fc <- 2 #put number of first variable column

target_calc <- match("target",names(work_data))


# binning every variable with different type of binning('equal', 'kmeans', 'smbinning', '')
bin_col <- match(names(work_data[,(variable_fc):ncol(work_data)])[(as.vector(sapply(work_data[,(variable_fc):ncol(work_data)], 
                                                                                    function(x) is.numeric(x))))], names(work_data))

digit<- c()
digit[begin_ncol]<-NA

#Enter precision using next loop or directly like digit[97]<-100 digit[c(3:5)]<-1 digit[135]<-0.01 etc


save(digit, file = "digit.rda")
load(file = 'digit.rda')

error_list <- c()  

work_data <- work_data_if_smth_go_wrong
#loop with binning and creating cat vatiables in one
for (i in bin_col) 
{ 
  print(i)
  print(names(work_data[i]), quote = FALSE)
  # equal 
  # Idea: equal number of observation in every class
  print('calculating equal_depth method...', quote = FALSE)
  # create intervals
  eq_d<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], 5, style = 'quantile')
  
  # column name for new binning column, that bins with 'equal' method
  colname <- paste(names(work_data)[i], "cat_Eq_depth ", sep="_")
  
  # set column, that bins with 'equal' method
  work_data[[colname]] <- with(work_data, cut(work_data[,i], 
                                              c(min(work_data[,i])-1,unique(eq_d$brks)),include.lowest = TRUE, 
                                              right = FALSE, ordered = TRUE,dig.lab = 10))
  
  # for saving ordered factors all repleacements must be done on factor levels, not on work_data(!!!)
  levels(work_data[[colname]])<-gsub(",", ";", levels(work_data[[colname]]))
  
  print('calculating equal_width method...', quote = FALSE)
  # create intervals
  eq_w<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], 5, style = 'equal')
  
  # column name for new binning column, that bins with 'equal' method
  colname <- paste(names(work_data)[i], "cat_eq_width", sep="_")
  
  # set column, that bins with 'equal' method
  work_data[[colname]] <- with(work_data, cut(work_data[,i], 
                                              c(min(work_data[,i])-1,unique(eq_w$brks)),include.lowest = TRUE, 
                                              right = FALSE, ordered = TRUE,dig.lab = 10))
  
  # for saving ordered factors all repleacements must be done on factor levels, not on work_data(!!!)
  levels(work_data[[colname]])<-gsub(",", ";", levels(work_data[[colname]]))
  
  print('calculating jenks method...', quote = FALSE)
  # create intervals
  jk<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], 5, style = 'jenks')
  
  # column name for new binning column, that bins with 'equal' method
  colname <- paste(names(work_data)[i], "cat_jenks", sep="_")
  
  # set column, that bins with 'equal' method
  work_data[[colname]] <- with(work_data, cut(work_data[,i], 
                                              c(min(work_data[,i])-1,unique(jk$brks)),include.lowest = TRUE, 
                                              right = FALSE, ordered = TRUE,dig.lab = 10))
  
  # for saving ordered factors all repleacements must be done on factor levels, not on work_data(!!!)
  levels(work_data[[colname]])<-gsub(",", ";", levels(work_data[[colname]]))
  
  # kmeans 
  # Idea: search k 'center' points, that have biggest spreading of points around this 'center'(every interval have his own 'center' point)
  print('calculating kmeans method...', quote = FALSE)
  # k in {6,5,4,3,2}
  interval_count<-6
  repeat{
    km<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], interval_count, style = 'kmeans')
    colname <- paste(names(work_data)[i], "cat_km", interval_count, sep="_")
    work_data[[colname]] <- with(work_data, cut(work_data[,i], km$brks,include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
    interval_count<-interval_count-1
    
    # any class have to be bigger than 3% of all work_data
    #if (min(table(work_data[[colname]])/(nrow(work_data)/100)>3)) {break}
    
    # interval_count - {6,5,4,3,2}, if interval_count<2 - end
    if (interval_count<2) {break}
  }
  
  # hclust 
  # Idea: search k 'center' points, that have biggest spreading of points around this 'center'(every interval have his own 'center' point)
  print('calculating hclust method...', quote = FALSE)
  # k in {6,5,4,3,2}
  interval_count<-6
  repeat{
    hcl<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], interval_count, style = 'hclust')
    colname <- paste(names(work_data)[i], "cat_hcl", interval_count,  sep="_")
    work_data[[colname]] <- with(work_data, cut(work_data[,i], hcl$brks,include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
    interval_count<-interval_count-1
    
    # any class have to be bigger than 3% of all work_data
    #if (min(table(work_data[[colname]])/(nrow(work_data)/100)>3)) {break}
    
    # interval_count - {6,5,4,3,2}, if interval_count<2 - end
    if (interval_count<2) {break}
  }
  
  # smbinning
  print('calculating smbinning method...', quote = FALSE)
  # Idea: 'optimal binning' (maximization IV)
  i <- 3
  sb_data<-work_data[c(target_calc,i)]
  sb_data[2]<-ceiling(sb_data[2]/digit[i])*digit[i]
  sb<-try(smbinning(sb_data,y=names(work_data)[target_calc],x=names(work_data)[i]), FALSE)
  if (length(sb) > 1) ##check this condition !!!!
  {colname <- paste(names(work_data)[i], "cat_sb", sep="_")
  work_data[[colname]] <- with(work_data, cut(work_data[,i], c(min(work_data[,i])-1,unique(sb$bands)),
                                              right = TRUE, left = FALSE, ordered = TRUE,dig.lab = 10))
  levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
  }
  if (length(grep('Error', sb)) > 0 )
  {
    error_list <- c(error_list, i)
  }
  # save binnings intervals into 'rda' files
  save(eq_w, eq_d, jk, km, sb, file = paste(names(work_data[i]),".rda", sep = ""))
  
  rm(eq_w)
  rm(eq_d)
  rm(jk)
  rm(km)
  rm(sb)
  rm(interval_count)
  rm(colname) 
}

### IV

work_data$duration_month_2 <- NULL
work_data$credit_amount_5 <- NULL
work_data$age_in_yrs_13 <- NULL

bin_data <- work_data
variable_fc_bin <- which( colnames(bin_data)=="target" ) + 1
bin_ncol <- ncol(bin_data)
## All variables should have 'factor' type, so convert variables, that not is a 'factor', to 'factor' 
# select variables that shouldn`t conver to the 'fator'
factor_vars <- c(names(bin_data[,-1])[1:(variable_fc_bin-1)]
                 ,names(which(sapply(bin_data[,variable_fc_bin:ncol(bin_data)], is.factor))))


str(work_data)


iv_table <- cbind.data.frame( variables = names(bin_data[variable_fc_bin:bin_ncol])
                              , IV = sapply(bin_data[variable_fc_bin:bin_ncol], function(x) round(IV(X=x, Y=bin_data$target)[1],4))
                              , row.names = NULL) %>%
  arrange(desc(IV)) %>%
  transmute(row_number = row_number(), !!!.)

# Add strength of variables
iv_table$Strength <- ifelse(iv_table$IV>=1, "Suspicious",
                            ifelse(iv_table$IV>=.5, "Very strong",
                                   ifelse(iv_table$IV>=.2, "Strong",
                                          ifelse(iv_table$IV>=.1, "Average",
                                                 ifelse(iv_table$IV>=.02, "Weak", "Wery weak")))))


iv_table[!grepl('hcl',iv_table$variables), -1]














##### BR analysis #####
bin_data <- work_data
variable_fc_bin <- 2
i <- 2
bin_ncol<-ncol(bin_data)
for (i in variable_fc_bin:bin_ncol){
  #create 'br_table'. It consists of 2 column("BR" + name_of_variables, BR_value)
  var_for_group <- names(bin_data)[i]
  column_br <- paste("BR", 
                     names(bin_data)[i]
                     , sep="_")
  
  br_table <- bin_data %>%
    select(c(i,target)) %>%
    group_by_(.dots = var_for_group) %>%
    summarise_all(funs(!!column_br := (n() - sum(.))/n()))
  
  # join 'br_table' to the table with bining variables
  bin_data <- left_join(bin_data, br_table,by=names(bin_data)[i])
}

setwd("F:/Дипломна робота_2/картинки")
target_calc_bin <- 1
variable_fc_bin <- 2
bin_ncol <- 21
k <- 1
i <- 1

Total<-length(bin_data$target_for_calc)
Good<-sum(bin_data$target_for_calc)
Bad<-Total-Good

j <- 18
for (j in variable_fc_bin:bin_ncol) {
  
  plot1_hist <- ggplot(bin_data, aes(bin_data[,j])) + 
    geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
    scale_y_continuous(labels=scales::percent)+ 
    geom_text(aes( y = ((..count..)/sum(..count..)),
                   label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
    theme(axis.text.x = element_text(angle=10, vjust=0.9),
          plot.margin = unit(c(1,1,1,1), "cm") ) + 
    labs( y = "Class", x = "")
  
  plot2_BR_line <- ggplot(bin_data, aes(x=bin_data[,j],y=bin_data[,j-variable_fc_bin+bin_ncol+1],group=1)) + 
    geom_line(color="indianred3",size=1)+
    geom_point(color="indianred3") +
    theme(axis.text.x = element_text(angle=10, vjust=0.9),
          plot.margin = unit(c(1,1,1,1), "cm") ) + 
    scale_y_continuous(limits=c(0, 0.6),breaks= seq(0, .6, .2), 
                       labels = function(x) paste0(x*100, "%"))+
    labs( y = "BR", x = "")
  
  # union 2 graphics(plot1_hist, plot2_BR_line) in 1 
  # extract gtable
  g1 <- ggplot_gtable(ggplot_build(plot1_hist))
  g2 <- ggplot_gtable(ggplot_build(plot2_BR_line))
  
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                       pp$l, pp$b, pp$l)
  
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  #log(x) will produce NaN any time x is less than zero(calculating 'length(x)-sum(x)' we have '-' func 'log' see that and returns error
  options(warn = -1) 
  
  # calc statistic values for every column
  aggregate_table<-aggregate(. ~ bin_data[,j], data = bin_data[c(names(bin_data)[target_calc_bin],names(bin_data)[j])],
                             FUN = function(x) c(good = sum(x),
                                                 bad=length(x)-sum(x),
                                                 total = length(x),
                                                 good2=  round((sum(x)*100)/Good,2),
                                                 bad2=round((length(x)-sum(x))*100/Bad,2),
                                                 total2=round((length(x)*100)/Total,2),
                                                 BR=round((length(x)-sum(x))*100/length(x),2),
                                                 WOE=round(log((sum(x)/Good)/((length(x)-sum(x))/Bad)),4)))[,c(1,2)]
  
  #log(x) will produce NaN any time x is less than zero(calculating 'length(x)-sum(x)' we have '-' func 'log' see that and returns error
  aggregate_table<-cbind(aggregate_table[,1],data.frame(aggregate_table[,2]))
  names(aggregate_table)<-c(names(bin_data)[j],"good, #","bad, #","total, #","good, %","bad, %","total, %","BR, %","WOE")
  
  # chisq.test
  
  var_for_group <- names(bin_data)[j]
  chisq_table <-  bin_data %>%
    select(c(j,target_calc_bin)) %>%
    group_by_(.dots = var_for_group) %>%
    summarise_all(funs(good = sum(.),
                       bad = (n() - sum(.)))) %>%
    select(-1) %>% 
    t() 
  
  # set chisq.test value and p_value
  chisq <- round(as.data.frame(chisq.test(chisq_table)[1])[1,1], 2)
  p_value_chisq <- round(as.data.frame(chisq.test(chisq_table)[3])[1,1], 4)
  
  # Data visualization in pdf
  # value from 'aggregate_table' sets in the ggplot object
  table <- ggtexttable(aggregate_table, rows = NULL, theme = ttheme("lRedWhite"))
  
  # set name of variable and her 'Strength'(dependense of IV: 'Strong', Weak, 'Very weak' and etc)
  print(paste0(k,". ", names(bin_data)[j]))
  k <- k+1
  png(file = paste0(i,".png"),width = 1200, height=1200)
  i <- i+1
  # union 4 object in one file: 
  print(ggarrange( g, table , 
                   ncol = 1, nrow = 2,heights = c(0.1, 0.04, 0.04, 0.3, 0.2)))
  
  dev.off() 
  
}



########################## select variable ##########################################
duration_month <- iv_table[grepl("duration_month", iv_table$variables),c(2,3)]
credit_amount <- iv_table[grepl("credit_amount", iv_table$variables),c(2,3)]
age_in_yrs <- iv_table[grepl("age_in_yrs", iv_table$variables),c(2,3)]

table1 <- ggtexttable(duration_month, rows = NULL, theme = ttheme(base_style ="lRedWhite", base_size = 10))

tab1 <- table_cell_font(table1, row = 2, column = 2, face = "bold")
tab1 <- table_cell_font(tab1, row = 5, column = 2, face = "bold")

table2 <- ggtexttable(credit_amount, rows = NULL, theme = ttheme(base_style ="lRedWhite", base_size = 10))

tab2 <- table_cell_font(table2, row = 2, column = 2, face = "bold")
tab2 <- table_cell_font(tab2, row = 8, column = 2, face = "bold")


table3 <- ggtexttable(age_in_yrs, rows = NULL, theme = ttheme(base_style ="lRedWhite", base_size = 10))

tab3 <- table_cell_font(table3, row = 2, column = 2, face = "bold")
tab3 <- table_cell_font(tab3, row = 3, column = 2, face = "bold")

names(bin_data)



########################## modeling 

work_data2 <- work_data[ ,c(1:18,22,34,48)]
str(work_data2)

setwd("F:/Дипломна робота_2")
save(work_data2, file = 'work_data.rda')


######################### go to model ######################################
work_data3 <- select(work_data2,
                     target,
                     chk_ac_status_1,
                     credit_history_3,
                     purpose_4,
                     savings_ac_bond_6,
                     p_employment_since_7,
                     instalment_pct_8,
                     present_residence_since_11,
                     duration_month_2_cat_km_6,
                     other_debtors_or_grantors_10,
                     credit_amount_5_cat_eq_width)

work_data3 <- select(work_data2,
                     target,
                     chk_ac_status_1,
                     credit_history_3,
                     purpose_4,
                     savings_ac_bond_6,
                     duration_month_2_cat_km_6)
names(work_data2)
work_data3 <- work_data2[,c(1:6,11,19,21)]
#select training sample 
set.seed(123)
train<-work_data3[sort(sample(nrow(work_data3), nrow(work_data3)*.7)),] # 70% here
test<-work_data3[-sort(sample(nrow(work_data3), nrow(work_data3)*.7)),] # rest of the 30% data goes here

names(work_data2)
table(train$target)
table(test$target)


table(train$target)[1]/table(train$target)[2]
table(test$target)[1]/table(test$target)[2]
table(bin_data$target)[1]/table(bin_data$target)[2]

## numeric
work_data_num <- read.csv2("https://raw.githubusercontent.com/Sokolheavy/R_Scoring/master/Diploma/german-numeric.csv")
work_data_num[,25] <- ifelse(work_data_num[,25]==1,1,0)

set.seed(123)
train_num <- work_data_num[sort(sample(nrow(work_data_num), nrow(work_data_num)*.7)),] # 70% here
test_num <- work_data_num[-sort(sample(nrow(work_data_num), nrow(work_data_num)*.7)),] # rest of the 30% data goes here


names(work_data_num) <- c('V1','V2','V3','V4','V5','V6','V7','V8','V9','V10','V11','V12','V13',
                          'V14','V15','V16','V17','V18','V19','V20','V21','V22','V23','V24','target')


##### log regr
m1 <- glm(target~.,data=train,family=binomial())
# m1 <- step(m1)
# summary(m1)

#score test data set
test$m1_score <- predict(m1,type='response',test)
m1_pred <- prediction(test$m1_score, test$target)
m1_perf <- performance(m1_pred,"tpr","fpr")

ev_df_m1 <- data.frame(Gini = round(((slot(performance(m1_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(m1_pred, measure = "auc")@y.values[[1]]*100, 2))
ev_df_m1

ev_df_m1 <- ggtexttable(ev_df_m1, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

Gini1 <- round(((slot(performance(m1_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2)
gini_plot <- ggplot(setNames(data.frame(m1_perf@x.values, m1_perf@y.values), c('x_val', 'y_val')), 
                    aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  ggtitle(paste("Gini=", round(Gini1,4), sep="")) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")


#LASSO
setwd("F:/Дипломна робота_2/картинки/LASSO")
x.m = train_num[,1:24]
y.m = train_num[,25]

set.seed(356)
# 10 fold cross validation
cvfit.m.ridge = cv.glmnet(x.m, y.m, 
                          family = "binomial", 
                          alpha = 0,
                          type.measure = "class")

cvfit.m.lasso = cv.glmnet(x.m, y.m, 
                          family = "binomial", 
                          alpha = 1,
                          type.measure = "class")
par(mfrow=c(1,2))
png(filename="myfile.png", res=150, width = 1000, height = 1000)
plot(cvfit.m.ridge, main = "Ridge")

plot(cvfit.m.lasso, main = "Lasso")
dev.off()

#### second variant 
mat1 <- model.matrix(target ~ . , data = train  ) # convert to numeric matrix
mat2 <- model.matrix(target ~ . , data = test  )  # convert to numeric matrix

m3 <-cv.glmnet(mat1,as.numeric(train$target), alpha=1, family="binomial", type.measure = 'auc')

# Apply model to testing dataset
test$m3_score <- predict(m3,type="response", newx =mat2, s = 'lambda.min')
m3_pred <- prediction(test$lasso.prob,test$target)

# calculate probabilities for TPR/FPR for predictions
m3_perf <- performance(m3_pred,"tpr","fpr")


ev_df_m3 <- data.frame(Gini = round(((slot(performance(m3_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                         AUC = round(performance(m3_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m3 <- ggtexttable(ev_df_m3, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

Gini3 <- round(((slot(performance(m3_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2)
gini_plot <- ggplot(setNames(data.frame(m3_perf@x.values, m3_perf@y.values), c('x_val', 'y_val')), 
                    aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  ggtitle(paste("Gini=", round(Gini3,4), sep="")) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")

png(filename="LASSO.png", res=150, width = 1000, height = 1000)
plot(m3)
dev.off()

log(m3$lambda.min)

coef(m3, s=m3$lambda.min)

### Random Forest















names(work_data2)

##### recursive partition (cart algorithm)#####

set.seed(123)
train<-work_data2[sort(sample(nrow(work_data3), nrow(work_data2)*.7)),] # 70% here
test<-work_data2[-sort(sample(nrow(work_data3), nrow(work_data2)*.7)),] # rest of the 30% data goes here


m2 <- rpart(target~age_in_yrs_13_cat_jenks+credit_amount_5_cat_eq_width+credit_history_3+duration_month_2_cat_km_6+property_type_12+purpose_4,data=train)

# score test data
test$m2_score <- predict(m2,test)
m2_pred <- prediction(test$m2_score,test$target)
m2_perf <- performance(m2_pred,"tpr","fpr")

ev_df_m2 <- data.frame(Gini = round(((slot(performance(m2_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(m2_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m2 <- ggtexttable(ev_df_m2, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))


m1_gini_plot <- ggplot(setNames(data.frame(m2_perf@x.values, m2_perf@y.values), c('x_val', 'y_val')), 
                       aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")


### C4.5

m2_1 <- C5.0(x = train[,-1] , y = as.factor(train[,1]))
# score test data
test$m2_score <- predict(m2_1,type='prob',test)
m2_1_pred <- prediction(test$m2_score,test$target)
m2_perf <- performance(m2_pred,"tpr","fpr")


ev_df_m2_1 <- data.frame(Gini = round(((slot(performance(m2_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                         AUC = round(performance(m2_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m2_1 <- ggtexttable(ev_df_m2_1, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

m2_1_gini_plot <- ggplot(setNames(data.frame(m2_perf@x.values, m2_perf@y.values), c('x_val', 'y_val')), 
                         aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")

##### Bayesian N Recursive partitioning #####

m2_2 <- rpart(target~.,data=train,parms=list(prior=c(.7,.3)),cp=.0002)

# score test data
test$m2_score <- predict(m2_2,type='prob',test)
m2_pred <- prediction(test$m2_score[,2],test$target)
m2_perf <- performance(m2_pred,"tpr","fpr")

ev_df_m2 <- data.frame(Gini = round(((slot(performance(m2_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(m2_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m2 <- ggtexttable(ev_df_m2, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))


m2_2_gini_plot <- ggplot(setNames(data.frame(m2_perf@x.values, m2_perf@y.values), c('x_val', 'y_val')), 
                         aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")

prp(m2_2,type=2,extra=1, main="m2_1-Recursive Partitioning - Using Bayesian N 70%-30%")


### ??????????????????????????Random Forest
m3 <- randomForest(target ~ ., data = train)

m3_fitForest <- predict(m3, newdata = train, type="prob")[,2]
m3_pred <- prediction( m3_fitForest, train$target)
m3_perf <- performance(m3_pred, "tpr", "fpr")

#plot variable importance
varImpPlot(m3, main="Random Forest: Variable Importance")


ev_df_m3 <- data.frame(Gini = round(((slot(performance(m3_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(m3_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m3 <- ggtexttable(ev_df_m3, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

m3_gini_plot <- ggplot(setNames(data.frame(m3_perf@x.values, m3_perf@y.values), c('x_val', 'y_val')), 
                       aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")

### ??????????????????????????Random Forest 2

set.seed(123456742)
m3_1 <- cforest(target~., control = cforest_unbiased(mtry = 2, ntree = 50), data = train)
#plot(m3_1)

# Variable Importance
kable(as.data.frame(varimp(m3_1)))


m3_fitForest <- predict(m3_1, newdata = train, type="prob")[,2]
m3_pred <- prediction( m3_fitForest, train$target)
m3_perf <- performance(m3_pred, "tpr", "fpr")

ev_df_m3_1 <- data.frame(Gini = round(((slot(performance(m3_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                         AUC = round(performance(m3_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m3_1 <- ggtexttable(ev_df_m3_1, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

m3_1_gini_plot <- ggplot(setNames(data.frame(m3_perf@x.values, m3_perf@y.values), c('x_val', 'y_val')), 
                         aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")


### recursive partition (chaid algorithm)
m4 <- ctree(target~.,data=train)
plot(m4, main="m4: Conditional inference Tree",col="blue");

resultdfr <- as.data.frame(do.call("rbind", treeresponse(m4, newdata = test)))
test$m4_score <- resultdfr[,2]
m4_pred <- prediction(test$m4_score,test$target)
m4_perf <- performance(m4_pred,"tpr","fpr")


ev_df_m4 <- data.frame(Gini = round(((slot(performance(m4_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(m4_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m4 <- ggtexttable(ev_df_m4, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

m4_gini_plot <- ggplot(setNames(data.frame(m4_perf@x.values, m4_perf@y.values), c('x_val', 'y_val')), 
                       aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")

### SVM
m5 <- ksvm(target~.,data=train, kernel = "vanilladot")
m5_pred <- predict(m5, test[,1:11], type="response")
m5_accuracy <- data.frame(pred=(m5_pred == test$target))

m5_score <- predict(m5,test, type="decision")
m5_pred <- prediction(m5_score, test$target)
m5_perf <- performance(m5_pred, measure = "tpr", x.measure = "fpr")

ev_df_m5 <- data.frame(Gini = round(((slot(performance(m5_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(m5_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m5 <- ggtexttable(ev_df_m5, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

m5_gini_plot <- ggplot(setNames(data.frame(m5_perf@x.values, m5_perf@y.values), c('x_val', 'y_val')), 
                       aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")

### SVM Kernal

m5_2 <- ksvm(target ~ ., data = train, kernel = "rbfdot")
m5_2_pred <- predict(m5_2, test[,2:11], type="response")
m5_2_score <- predict(m5_2,test, type="decision")
m5_2_pred <- prediction(m5_2_score, test$target)
m5_2_perf <- performance(m5_2_pred, measure = "tpr", x.measure = "fpr")

ev_df_m5_2 <- data.frame(Gini = round(((slot(performance(m5_2_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                         AUC = round(performance(m5_2_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m5_2 <- ggtexttable(ev_df_m5_2, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

m5_2_gini_plot <- ggplot(setNames(data.frame(m5_2_perf@x.values, m5_2_perf@y.values), c('x_val', 'y_val')), 
                         aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")



### GBM
# install.packages("gbm")
# install.packages("xgboost")
# install.packages("h2o")

library(rsample)      # data splitting 
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(caret)        # an aggregator package for performing many machine learning models
library(h2o) 

set.seed(123)
# train GBM model
gbm.fit <- gbm(
  formula = target ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  


m6_pred <- predict(gbm.fit, test[,2:11], type="response")
m6_pred <- prediction(m6_score, test$target)
m6_perf <- performance(m6_pred, measure = "tpr", x.measure = "fpr")

ev_df_m5_2 <- data.frame(Gini = round(((slot(performance(m5_2_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                         AUC = round(performance(m5_2_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m5_2 <- ggtexttable(ev_df_m5_2, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

m5_2_gini_plot <- ggplot(setNames(data.frame(m5_2_perf@x.values, m5_2_perf@y.values), c('x_val', 'y_val')), 
                         aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")


m6 <- predict(gbm.fit, newdata = train, type="prob")[,2]
m6 <- prediction( m3_fitForest, train$target)
m6 <- performance(m3_pred, "tpr", "fpr")

m6_pred <- predict(gbm.fit, test[,2:11], type="response")
m6_score <- predict(gbm.fit,test, type="decision")
m6_pred <- prediction(m6_pred, test$target)
m6_perf <- performance(m6_pred, measure = "tpr", x.measure = "fpr")

ev_df_m6 <- data.frame(Gini = round(((slot(performance(m6_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(m5_2_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m6 <- ggtexttable(ev_df_m6, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))


