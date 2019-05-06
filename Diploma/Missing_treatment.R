# https://www.r-bloggers.com/five-steps-for-missing-data-with-finalfit/
# https://basegroup.ru/community/articles/missing

install.packages("GGally")
install.packages("finalfit") 
install.packages("MissMech") 


library(finalfit) 
library(gridExtra)
library(ggpubr)
library(cowplot)
library(corrplot)
library(gtable)
library(grid)
library(tidyr)
library(tibble)
library(tidyverse)
library(corrplot)
library(ROCR)
library(MLmetrics)
library(VIM)
library(GGally)
library(MissMech)

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
work_data$instalment_pct_8 <-  as.numeric(work_data$instalment_pct_8)
work_data$present_residence_since_11 <-  as.numeric(work_data$present_residence_since_11)
work_data$age_in_yrs_13        <-  as.numeric(work_data$age_in_yrs_13)
work_data$number_cards_this_bank_16    <-  as.factor(work_data$number_cards_this_bank_16)
work_data$no_people_liable_for_mntnance_18 <-  as.numeric(work_data$no_people_liable_for_mntnance_18)


work_data <- data.frame(target = ifelse(work_data$good_bad_21 == 1, 1,0), work_data)
work_data$good_bad_21 <- NULL  

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
ind_number_cards_this_bank2 <- sample(1:1000, 33)

work_data[ind_housing_type2, 16] <- NA
work_data[ind_job2, 18] <- NA
work_data[ind_number_cards_this_bank2, 17] <- NA


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

str(work_data)


options(repr.plot.width=8, repr.plot.height=5)
aggr_plot = aggr(work_data[,-1], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                 labels=names(work_data[,-1]), cex.axis=.7, gap=1,
                 ylab=c("Histogram of missing data","Pattern"))

explanatory = c("housing_type_15", "chk_ac_status_1", "job_17")
dependent = "target"
work_data %>% 
  missing_pairs(dependent, explanatory)


work_data %>%
  missing_plot()


№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№

install.packages("ggpubr")

library(knitr)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(lazyeval)
library(gtable)
library(grid)
library(tidyr)
library(tibble)
library(InformationValue)
library(data.table)
library(ggpubr)

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
work_data$instalment_pct_8 <-  as.numeric(work_data$instalment_pct_8)
work_data$present_residence_since_11 <-  as.numeric(work_data$present_residence_since_11)
work_data$age_in_yrs_13        <-  as.numeric(work_data$age_in_yrs_13)
work_data$number_cards_this_bank_16    <-  as.factor(work_data$number_cards_this_bank_16)
work_data$no_people_liable_for_mntnance_18 <-  as.factor(work_data$no_people_liable_for_mntnance_18)


work_data <- data.frame(target = ifelse(work_data$good_bad_21 == 1, 1,0), work_data)
work_data$good_bad_21 <- NULL  

set.seed(321)
ind_housing_type <- sample(1:1000, 15)
ind_job <- sample(1:1000, 21)
ind_number_cards_this_bank <- sample(1:1000, 23)

work_data[ind_housing_type, 16] <- NA
work_data[ind_job, 18] <- NA
work_data[ind_number_cards_this_bank, 17] <- NA

set.seed(651)
ind_housing_type2 <- sample(1:1000, 21)
ind_job2 <- sample(1:1000, 33)
ind_number_cards_this_bank2 <- sample(1:1000, 33)

work_data[ind_housing_type2, 16] <- NA
work_data[ind_job2, 18] <- NA
work_data[ind_number_cards_this_bank2, 17] <- NA

# All variables
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


ggplot(work_data, aes(housing_type_15)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
  scale_y_continuous(labels=scales::percent) + 
  geom_text(aes( y = ((..count..)/sum(..count..)),
                 label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
  theme(axis.text.x = element_text(angle=10, vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
  labs( y = "", x = "Type of house")

ggplot(work_data, aes(number_cards_this_bank_16)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
  scale_y_continuous(labels=scales::percent) + 
  geom_text(aes( y = ((..count..)/sum(..count..)),
                 label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
  theme(axis.text.x = element_text(angle=10, vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
  labs( y = "", x = "Number of cards")

ggplot(work_data, aes(job_17)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
  scale_y_continuous(labels=scales::percent) + 
  geom_text(aes( y = ((..count..)/sum(..count..)),
                 label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
  theme(axis.text.x = element_text(angle=10, vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
  labs( y = "", x = "Type of job")


str(work_data)


options(repr.plot.width=8, repr.plot.height=5)
aggr_plot = aggr(work_data[,-1], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                 labels=names(work_data[,-1]), cex.axis=.7, gap=1,
                 ylab=c("Histogram of missing data","Pattern"))

explanatory = c("housing_type_15", "chk_ac_status_1", "job_17")
dependent = "target"
work_data %>% 
  missing_pairs(dependent, explanatory)


work_data %>%
  missing_plot()


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
№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№
                         
   



plot(boruta_credit)


boruta_credit <- TentativeRoughFix(boruta_credit)
boruta_credit

model_formula <- getConfirmedFormula(boruta_credit)
model_formula

boruta_variables <- getSelectedAttributes(boruta_credit)

knitr::kable(attStats(boruta_credit))

plotImpHistory(boruta_credit)

boruta_dataset <- select(initial_data, c(target, boruta_variables))

cl <- makeCluster(6)
registerDoParallel(cl) 

fitControl <- trainControl(method = "repeatedcv", 
                           number = 5, 
                           repeats = 5,
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary,
                           sampling = "smote")

set.seed(100)
fit1 <- train(target ~ ., 
              data = boruta_dataset, 
              method = "glmnet", 
              trControl = fitControl,
              metric = "ROC",
              tuneGrid = expand.grid(alpha = c(0.1, 0.5, 1),
                                     lambda = c(0.005, 0.01, 0.02)))


saveRDS(fit1,  "fit1.rds")
fit1 <- readRDS("fit1.rds")
fit1$results[order(-fit1$results$ROC), ]

### 
# train & test
ind <- sample(c(1,2), nrow(boruta_dataset), replace = T, prob = c(.8, .2))
train <- initial_data[ind==1,]
test <- initial_data[ind==2,]


### boruta variable

# boruta_dataset <- train
#  72.29 86.15

# boruta_dataset <- test
#  88.54 94.27

##67.62 83.81
m1 <- glm(target~.,data=boruta_dataset,family=binomial())

#score test data set
boruta_dataset$m1_score <- predict(m1,type='response',boruta_dataset)
m1_pred <- prediction(boruta_dataset$m1_score, boruta_dataset$target)
m1_perf <- performance(m1_pred,"tpr","fpr")

ev_df_m1 <- data.frame(Gini = round(((slot(performance(m1_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(m1_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m1


### initial variable

# train & test
ind <- sample(c(1,2), nrow(work_data), replace = T, prob = c(.8, .2))
train <- initial_data[ind==1,]
test <- initial_data[ind==2,]


### boruta variable

# work_data <- train
# 42.68 71.34

# work_data <- test
#  51.73 75.86



##64.78 82.39
m1 <- glm(target_for_calc~.,data=work_data,family=binomial())
т
#score test data set
work_data$m1_score <- predict(m1,type='response',work_data)
m1_pred <- prediction(work_data$m1_score, work_data$target)
m1_perf <- performance(m1_pred,"tpr","fpr")

ev_df_m1 <- data.frame(Gini = round(((slot(performance(m1_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(m1_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m1




                          

