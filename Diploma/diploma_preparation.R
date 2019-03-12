# https://www.edureka.co/blog/clustering-on-bank-data-using-r/
# https://towardsdatascience.com/how-to-cluster-your-customer-data-with-r-code-examples-6c7e4aa6c5b1  

setwd('C:/Users/EASokol/Desktop/Diploma')
install.packages("caretEnsemble")
install.packages("mice") # for missing values
install.packages("doParallel")
install.packages("car")
install.packages("tibble")
install.packages("RColorBrewer")

library(dplyr)
library(tibble)
library(caret)
library(caretEnsemble)
library(mice)
library(doParallel)
library(car)
library(classInt)
library(RColorBrewer)
library(smbinning)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(gtable)
library(rpart)
library(rpart.plot)

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


work_data$duration_month_2  <- as.numeric(cdata$duration_month_2)
work_data$credit_amount_5   <-  as.numeric(cdata$credit_amount_5 )
work_data$instalment_pct_8 <-  as.numeric(cdata$instalment_pct_8)
work_data$present_residence_since_11 <-  as.numeric(cdata$present_residence_since_11)
work_data$age_in_yrs_13        <-  as.numeric(cdata$age_in_yrs_13)
work_data$number_cards_this_bank_16    <-  as.numeric(cdata$number_cards_this_bank_16)
work_data$no_people_liable_for_mntnance_18 <-  as.numeric(cdata$no_people_liable_for_mntnance_18)

# review variables
work_data <- data.frame(target_for_calc = ifelse(work_data$good_bad_21 == 1, 1,0), work_data)
work_data$good_bad_21 <- NULL   


work_data$credit_history_3<-as.factor(ifelse(work_data$credit_history_3 == "A30", "01.A30",
                                         ifelse(work_data$credit_history_3 == "A31","02.A31",
                                                ifelse(work_data$credit_history_3 == "A32","03.A32.A33",
                                                       ifelse(work_data$credit_history_3 == "A33","03.A32.A33",
                                                              "04.A34")))))


save(work_data, file='work_data.rds')

################ Create binning data 

# initial number of columns
begin_ncol <- ncol(work_data)

#initiate target column and first column with variables
target_ncol <- match("target_for_calc",names(work_data))

variable_fc <- 2 #put number of first variable column

target_calc <- match("target_for_calc",names(work_data))


# if unique values of variable < 6  => type of variable is factor
work_data <- data.frame(lapply(work_data, function(x) if(length(unique(x)) <=7) { as.factor(x)} else{x}))

# binning every variable with different type of binning('equal', 'kmeans', 'smbinning', '')
bin_col <- match(names(work_data[,(variable_fc):ncol(work_data)])[(as.vector(sapply(work_data[,(variable_fc):ncol(work_data)], 
                                                                                    function(x) is.numeric(x))))], names(work_data))

str(work_data)

load(file = 'digit.rda')
digit <- c()
digit[begin_ncol]<-NA
summary(work_data[,bin_col])
save(digit, file = "digit.rda")

load("digit.rda")

#Enter precision using next loop or directly like digit[97]<-100 digit[c(3:5)]<-1 digit[135]<-0.01 etc
for (i in bin_col)
{ 
  if(is.na(digit[i]))
  {
    print(paste(i, ':', names(work_data[i])), quote = FALSE)
    print(paste('from', min(work_data[i]), 'to', max(work_data[i]), 'with', nrow(unique(work_data[i])), 'unique values in', nrow(work_data[i]), 'records'), quote = FALSE)
    digit[i] <- as.numeric(readline(prompt="Enter precision: "))
  }
}


load('work_data.rda')
work_data <- work_data[complete.cases(work_data),]
nrow(work_data)
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

### plot for 
ggplot(work_data, aes(x = age_in_yrs)) +
  geom_density(adjust = .5)

names(work_data)[match("age_in_yrs",names(work_data))] <- 'age_in_yrs'

#unique(bin_data$credit_amount_5_cat_eq_width)
#credit_amount_5_cat_eq_width

ggplot(bin_data, aes(credit_amount_5_cat_eq_width)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
  scale_y_continuous(labels=scales::percent)+ 
  geom_text(aes( y = ((..count..)/sum(..count..)),
                 label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
  theme(axis.text.x = element_text(angle=10, vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
  labs( y = "Class", x = "")

str(bin_data)
ggplot(work_data, aes(y=age_in_yrs_13, x=target_for_calc, group=1)) + 
  geom_boxplot()

### IV
work_data2 <- work_data[complete.cases(work_data),]
nrow(work_data2)

bin_data2 <- bin_data[complete.cases(bin_data),]
nrow(bin_data2)

work_data$duration_month_2 <- NULL
work_data$credit_amount_5 <- NULL
work_data$age_in_yrs_13 <- NULL

bin_data <- work_data
variable_fc_bin <- which( colnames(bin_data)=="target_for_calc" ) + 1
bin_ncol <- ncol(bin_data)
## All variables should have 'factor' type, so convert variables, that not is a 'factor', to 'factor' 
# select variables that shouldn`t conver to the 'fator'
factor_vars <- c(names(bin_data[,-1])[1:(variable_fc_bin-1)]
                 ,names(which(sapply(bin_data[,variable_fc_bin:ncol(bin_data)], is.factor))))

# convert "unfactor" variables to 'factor'
bin_data[setdiff(names(bin_data[ ,-1]),factor_vars)] <- data.frame(
  sapply(
    select(bin_data, -factor_vars), as.factor))


rm(factor_vars)

## IV - statistic table
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



save(bin_data, file='bin_data.rda')

bin_data <- bin_data[-916,]
bin_data[bin_data$credit_amount_5_cat_eq_width=='[14770;18400]',33]  <- '[11140;14770)'



unique(bin_data$credit_amount_5_cat_eq_width)
summary(bin_data$credit_amount_5_cat_eq_width)
View(bin_data$credit_amount_5_cat_eq_width)

View(subset(bin_data, is.na(bin_data$credit_amount_5_cat_eq_width)))

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

work_data_new <- select(bin_data, target_for_calc,
                        chk_ac_status_1,
                        duration_month_2_cat_km_5,
                        credit_history_3,
                        purpose_4,
                        credit_amount_5_cat_eq_width,
                        savings_ac_bond_6,
                        p_employment_since_7,
                        instalment_pct_8,
                        property_type_12,
                        age_in_yrs_13_cat_km_6)


work_data_old <- select(bin_data, target_for_calc,
                        chk_ac_status_1,
                        duration_month_2_cat_eq_depth,
                        credit_history_3,
                        purpose_4,
                        credit_amount_5_cat_eq_depth,
                        savings_ac_bond_6,
                        p_employment_since_7,
                        instalment_pct_8,
                        property_type_12,
                        age_in_yrs_13_cat_eq_depth)

names(work_data_new)[1] <- "target"
names(work_data_old)[1] <- "target"

save(work_data_new, file = 'work_data_new.rda')
save(work_data_old, file = 'work_data_old.rda')

######################### go to model ######################################
#select training sample 
set.seed(123)
train<-work_data_new[sort(sample(nrow(work_data_new), nrow(work_data_new)*.7)),] # 70% here
test<-work_data_new[-sort(sample(nrow(work_data_new), nrow(work_data_new)*.7)),] # rest of the 30% data goes here

train_old<-work_data_old[sort(sample(nrow(work_data_old), nrow(work_data_old)*.7)),] # 70% here
test_old<-work_data_old[-sort(sample(nrow(work_data_old), nrow(work_data_old)*.7)),] # rest of the 30% data goes here

table(train$target)
table(test$target)
table(bin_data$target)


table(train$target)[1]/table(train$target)[2]
table(test$target)[1]/table(test$target)[2]
table(bin_data$target)[1]/table(bin_data$target)[2]

##### log regr
m1 <- glm(target~.,data=train,family=binomial())
m1 <- step(m1)

#score test data set
test$m1_score <- predict(m1,type='response',test)
m1_pred <- prediction(test$m1_score, test$target)
m1_perf <- performance(m1_pred,"tpr","fpr")


Gini  <- (slot(performance(m1_pred, measure = "auc"),"y.values")[[1]])*2 - 1
gini_plot <- ggplot(setNames(data.frame(m1_perf@x.values, m1_perf@y.values), c('x_val', 'y_val')), 
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


##### recursive partition #####

m2 <- rpart(target~.,data=train)
# Print tree detail
printcp(m2)

# Better version of plot
prp(m2,type=2,extra=1,  main="Tree:Recursive Partitioning")

# score test data
test$m2_score <- predict(m2,type='prob',test)
m2_pred <- prediction(test$m2_score[,2],test$target)
m2_perf <- performance(m2_pred,"tpr","fpr")

Gini  <- (slot(performance(m2_pred, measure = "auc"),"y.values")[[1]])*2 - 1
gini_plot <- ggplot(setNames(data.frame(m2_perf@x.values, m2_perf@y.values), c('x_val', 'y_val')), 
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

### old data

m2 <- rpart(target~.,data=train_old)
# Print tree detail
printcp(m2)

# Better version of plot
prp(m2,type=2,extra=1,  main="Tree:Recursive Partitioning")

# score test data
test$m2_score <- predict(m2,type='prob',test_old)
m2_pred <- prediction(test_old$m2_score[,2],test_old$target)
m2_perf <- performance(m2_pred,"tpr","fpr")

Gini  <- (slot(performance(m2_pred, measure = "auc"),"y.values")[[1]])*2 - 1
gini_plot <- ggplot(setNames(data.frame(m2_perf@x.values, m2_perf@y.values), c('x_val', 'y_val')), 
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


##### Bayesian N Recursive partitioning #####

m2_1 <- rpart(target~.,data=train,parms=list(prior=c(.7,.3)),cp=.0002)

# score test data
test$m2_score <- predict(m2_1,type='prob',test)
m2_pred <- prediction(test$m2_score[,2],test$target)
m2_perf <- performance(m2_pred,"tpr","fpr")

Gini  <- (slot(performance(m2_pred, measure = "auc"),"y.values")[[1]])*2 - 1
gini_plot <- ggplot(setNames(data.frame(m2_perf@x.values, m2_perf@y.values), c('x_val', 'y_val')), 
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

### old data
m2_1 <- rpart(target~.,data=train_old,parms=list(prior=c(.7,.3)),cp=.0002)
# score test data
test_old$m2_score <- predict(m2_1,type='prob',test_old)
m2_pred <- prediction(test_old$m2_score[,2],test_old$target)
m2_perf <- performance(m2_pred,"tpr","fpr")

Gini  <- (slot(performance(m2_pred, measure = "auc"),"y.values")[[1]])*2 - 1
gini_plot <- ggplot(setNames(data.frame(m2_perf@x.values, m2_perf@y.values), c('x_val', 'y_val')), 
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
