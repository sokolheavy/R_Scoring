# Data manipulation
library(plyr)
library(dplyr)
library(tibble)
library(tidyr)
library(tibble)
library(tidyverse)
library(knitr)

# Visualization
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(gtable)
library(gridExtra)
library(corrplot)
library(grid)

# Modeling
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
library(glmnet)
library(caroline)
library(ROCR)
library(MLmetrics)
library(VIM)
library(GGally)
library(MissMech)
library(InformationValue)
library(smbinning)
library(xgboost)
library(parallel)



# Load data
work_data <- read.table("https://raw.githubusercontent.com/Sokolheavy/R_Scoring/master/Diploma/work_data_initial.csv", h=T, sep=";")
work_data[,1] <- as.integer(work_data[,1])
var_for_facor <- c(8, 9 ,12 ,17 ,19)
work_data[,var_for_facor] <- data.frame(sapply(work_data[,var_for_facor], 
                                               function(x) as.factor(x)))


head(train)
glimpse(train)
str(work_data)
kable(as.data.frame(colnames(train)))
DT::datatable(train)

## Stratified Random Sampling(test, train)
set.seed(123)
part_1 <- createDataPartition(y = work_data$target, p = 0.7, list = F)

# Training Sample
train <- work_data[part_1,] # 70%

# Test Sample
test <- work_data[-part_1,] # 30% 

# persentage of values in variable
prop.table(table(test$target))
prop.table(table(train$target))

table(test$target)
table(train$target)



### Check missing data

missing_row = train[!complete.cases(train),]
head(missing_row)


sum(is.na(train))/(nrow(train)*ncol(train))

work_data %>%
  missing_plot()

# [%,#] missing
options(scipen = 99)
kable(data.frame(obs = nrow(work_data),
                 qty_NA = colSums(sapply(work_data, is.na)),
                 prop_NA = colSums(sapply(work_data, is.na))/nrow(work_data)), digits = 3)


### how many missing in num variables
# num, int
train_int <- select_if(train, is.integer)
options(scipen = 999)
summary_df <- do.call(cbind, lapply(train_int[, 
                                              1:ncol(train_int)], summary))
summary_df_t <- as.data.frame(round(t(summary_df),0))
names(summary_df_t)[7] <- paste("Missing_values")
summary_df_t_2 <- summary_df_t %>% 
  mutate(obs = nrow(train_int),
         Missing_prop = Missing_values / obs,
         variable = rownames(summary_df_t))

kable(summary_df_t_2, digits = 4)

kable(data.frame(summary_df_t_2 %>% summarise(Min = mean(Min.),
                                              first_Q = mean(`1st Qu.`),
                                              Median = median(Median),
                                              Mean = mean(Mean),
                                              third_Q = mean(`3rd Qu.`),
                                              Max = max(Max.),
                                              obs = mean(obs),
                                              qty_NA = mean(Missing_values),
                                              prop_NA = qty_NA/obs)))

# distribution of variables
train %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(fill = "steelblue4")

train %>%
  keep(is.numeric) %>%                     
  gather() %>%                            
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_density()

train %>%
  #keep(is.factor) %>%                     
  gather() %>%                            
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
  scale_y_continuous(labels=scales::percent)+ 
  geom_text(aes( y = ((..count..)/sum(..count..)),
                 label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
  theme(axis.text.x = element_text(angle=10, vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
  labs( y = "Class", x = "")


## try to nkow reason of missing values

# missing values has the same influece to the target as other values in variable
# use chisq.test

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

kable(ch_table) 


# with NA
train %>%
  filter(is.na(job)) %>%
  select(target) %>%
  gather() %>%                            
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
  scale_y_continuous(labels=scales::percent)+ 
  geom_text(aes( y = ((..count..)/sum(..count..)),
                 label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
  theme(axis.text.x = element_text(angle=10, vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
  labs( y = "Class", x = "")


# without NA
train %>%
  filter(!is.na(job)) %>%
  select(target) %>%
  gather() %>%                            
  ggplot(aes(value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
  scale_y_continuous(labels=scales::percent)+ 
  geom_text(aes( y = ((..count..)/sum(..count..)),
                 label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
  theme(axis.text.x = element_text(angle=10, vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
  labs( y = "Class", x = "")


## imputation NA

init <- mice(work_data[, -1], maxit=0, print=F)
methnew<- init$meth

var_imputation <- init$meth[init$meth!=""]

pred <- init$pred
pas <- mice(work_data[, -1], meth=methnew, pred=pred, maxit=10, seed=123, print=F)

# check imputation
densityplot(pas, ~job)


t.test(work_data$job, completedData$job, paired = TRUE)


completedData <- complete(pas)
work_data <- data.frame(target = work_data[,1], completedData)
work_data_if_smth_go_wrong <- work_data 


#Confirm no NAs
sum(sapply(train_complete, function(x) { sum(is.na(x)) }))


