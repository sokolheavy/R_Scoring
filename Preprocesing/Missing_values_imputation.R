# https://www.kaggle.com/captcalculator/imputing-missing-data-with-the-mice-package-in-r
install.packages("finalfit")

library(ggplot2) 
library(readr)
library(dplyr)
library(knitr)
library(data.table)
library(DT)
library(purrr)
library(tidyr)
library(corrplot)
library(VIM)
library(caret)
library(randomForest)
library(Metrics)
library(mice)
library(finalfit)



#Reading the dataset
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
work_data <- data.frame(target= ifelse(work_data$good_bad_21 == 1, 1,0), work_data)
work_data$good_bad_21 <- NULL   

set.seed(123)
ind_status_na <- sample(c(1,2), nrow(work_data), replace = T, prob = c(.2, .8))
work_data[ind_status_na==1,2] <- NA

set.seed(321)
ind_duration_na <- sample(c(1,2), nrow(work_data), replace = T, prob = c(.2, .8))
work_data[ind_duration_na==1, 3] <- NA

work_data[work_data$personal_status_9 =='A91', 'personal_status_9'] <- NA

str(work_data)


initial_data <- work_data

# train & test
ind <- sample(c(1,2), nrow(initial_data), replace = T, prob = c(.8, .2))
train <- initial_data[ind==1,]
test <- initial_data[ind==2,]


head(train)

glimpse(train)

kable(as.data.frame(colnames(train)))
DT::datatable(train)


# persentage of values in variable
options(digits = 2)
prop.table(table(train$target))

### how many missing value
str(train)
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


# factor, charater
train_f <- select_if(train, is.factor)
options(scipen = 999)
kable(data.frame(obs = nrow(train_f),
                 qty_NA = colSums(sapply(train_f, is.na)),
                 prop_NA = colSums(sapply(train_f, is.na))/nrow(train_f)), digits = 4)

cat_var_names = names(train_f)
cat_var_names

# All variables
options(scipen = 99)
kable(data.frame(obs = nrow(train),
                 qty_NA = colSums(sapply(train, is.na)),
                 prop_NA = colSums(sapply(train, is.na))/nrow(train)), digits = 3)



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

### DATA PREPARATION

## imputation NA

# delete rows
missing_row = train[!complete.cases(train),]
head(missing_row)


sum(is.na(train))/(nrow(train)*ncol(train))


# NA distribution
options(repr.plot.width=8, repr.plot.height=5)
aggr_plot = aggr(train, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                 labels=names(train), cex.axis=.7, gap=1,
                 ylab=c("Histogram of missing data","Pattern"))

marginplot(train[c(1,3)])



# variables with NA
var_na <- names(initial_data %>% select_if(function(x) any(is.na(x))))

# missing value class(MAR, MCAR)


target_personal_status_9 <- ifelse(is.na(train$personal_status_9), 1, 0)
personal_status_9_dataset <- data.frame(target_personal_status_9, select(train, -personal_status_9))
names(personal_status_9_dataset)
model <- glm(as.factor(target_personal_status_9) ~., personal_status_9_dataset, family="binomial")


summary(model)

target_chk_ac_status_1 <- ifelse(is.na(train$chk_ac_status_1), 1, 0)
chk_ac_status_1_dataset <- data.frame(target_chk_ac_status_1, select(train, -chk_ac_status_1))
names(chk_ac_status_1_dataset)
model2 <- glm(as.factor(target_chk_ac_status_1) ~., chk_ac_status_1_dataset, family="binomial")


summary(model2)

explanatory = var_na
dependent = "target"
imp_df <- train %>% 
  finalfit(dependent, explanatory)  # Omit when you run


View(imp_df)

init = mice(train, maxit=0) 
meth = init$method
predM = init$predictorMatrix

set.seed(103)
imputed = mice(train, method=meth, predictorMatrix=predM, m=1)
densityplot(imputed)



## cheack accurancy
# use test data

i <- "CONTENT_LENGTH"
for (i in var_na){
  
  set.seed(103)
  data_var_wo_na <- initial_data[!sapply(initial_data[ ,i], is.na), ]
  ind_var <- sample(c(1,2), nrow(data_var_wo_na), replace = T, prob = c(.2,.8))
  test_var <- data_var_wo_na[ind_var==1, ]
  
  # set NA to the test randomly
  test_na <- test_var
  test_na[sample(1:nrow(test_var), trunc(nrow(test_var)*.2)), i]<- NA
  test_imputed <- complete(test_na)
  if (meth[i]=='pmm') mean(test_imputed[,i] != test_var[,i])
  else (table(test_var[,i]))
  
}








# table for Accurancy
accurancy_df <- data.frame(variable = character(),
                           method = character(),
                           MAE = numeric(),
                           RMSE = numeric(),
                           MAPE = numeric())


## KNN imputation
for (i in seq(var_na)) {
  for (k in c(2,4,6)){
    df_knn<- kNN(train %>% select_if(function(x) any(is.na(x))),
                 variable= var_na[i], k=k)
    predictkNN <- df_knn[is.na(train[ ,var_na[i]]), var_na[i]]
    
    original<-train[is.na(train[ ,var_na[i]]), var_na[i]]
    fn<-ifelse (is.na (df2$Sepal.Length) ==TRUE, df2$Sepal.Length, 0)
    original<-D$Sepal.Length [is.na (fn)]
    
    
    df1 <- iris
    df1$Sepal.Length [20:140] <-NA
    
    
    mae_kNN<-mae(original,predictkNN)
    rmse_kNN<-rmse(original,predictkNN)
    mape_kNN<-mape(original,predictkNN)
    accurancy_df <- union_all(accurancy_df, data.frame(var_na[i]
                                                       , paste0("KNN_", k) 
                                                       , mae_kNN
                                                       , rmse_kNN
                                                       , mape_kNN))
  }
}



df_knn<- kNN(train, variable= var_na[i] ,k=6 )
df23$Sepal.Length_imp<-NULL 



library(mice)
library(lattice)

data(nhanes)

### Check missing data
# [%,#] missing
options(scipen = 99)
kable(data.frame(obs = nrow(work_data),
                 qty_NA = colSums(sapply(work_data, is.na)),
                 prop_NA = colSums(sapply(work_data, is.na))/nrow(work_data)), digits = 3)

work_data %>%
  missing_plot()


nhanes.imp <- mice(nhanes, m = 5, method = 'pmm')

# Show imputation 3
head(complete(nhanes.imp, 3))

# Extract the "tall" matrix which stacks the imputations
nhanes.comp <- complete(nhanes.imp, "long", include = TRUE)


table(nhanes.comp$.imp)


# Let's visualize the distribution of imputed and observed values.
# `cci` returns logical whether its input is complete at each observation. 
nhanes.comp$bmi.NA <- cci(nhanes$bmi)

# Note that the above uses the recylcing properties of matrixes/data.frame:
#  The `cci` call returns length 25; but because values are recylced to the total
#  number of rows in nhanes.comp, it replicates 6 times.

head(nhanes.comp[, c("bmi", "bmi.NA")])


ggplot(nhanes.comp, 
       aes(x = .imp, y = bmi, color = bmi.NA)) + 
  geom_jitter(show.legend = FALSE, 
              width = .1)


with(nhanes.imp, mean(bmi))

with(nhanes.imp, t.test(bmi ~ hyp))

densityplot(nhanes.imp, ~bmi)

##########################################################################################
# Here we can alo use the stripplot function of the mice pkg to check diagnostics visually
# using a jittered stripplot of the data
# Blue = Original Data, Red = Imputed Data
# 0 = Orignial Dataset, 1 = Imputed Dataset
# The distributions of the imputed data look somewhat similar to the original...
#################################################################################

stripplot(data.frame(nhanes.comp), bmi ~.imp, cex=c(2,4), pch=c(1,20),jitter=TRUE,layout=c(1,1))
stripplot(impTrainData, Family_Hist_5 ~.imp, cex=c(2,4), pch=c(1,20),jitter=TRUE,layout=c(1,1))
stripplot(impTrainData, Family_Hist_3 ~.imp, cex=c(2,4), pch=c(1,20),jitter=TRUE,layout=c(1,1))
stripplot(impTrainData, Family_Hist_2 ~.imp, cex=c(2,4), pch=c(1,20),jitter=TRUE,layout=c(1,1))
stripplot(impTrainData, Insurance_History_5 ~.imp, cex=c(2,4), pch=c(1,20),jitter=TRUE,layout=c(1,1))
stripplot(impTrainData, Family_Hist_4 ~.imp, cex=c(2,4), pch=c(1,20),jitter=TRUE,layout=c(1,1))
stripplot(impTrainData, Employment_Info_6 ~.imp, cex=c(2,4), pch=c(1,20),jitter=TRUE,layout=c(1,1))
stripplot(impTrainData, Employment_Info_4 ~.imp, cex=c(2,4), pch=c(1,20),jitter=TRUE,layout=c(1,1))
stripplot(impTrainData, Employment_Info_1 ~.imp, cex=c(2,4), pch=c(1,20),jitter=TRUE,layout=c(1,1))
stripplot(impTrainData, Medical_History_1 ~.imp, cex=c(2,4), pch=c(1,20),jitter=TRUE,layout=c(1,1))


















### Correlation

options(repr.plot.width=7, repr.plot.height=7)
numeric_var1 <- subset(train_int, select = -c(CONTENT_LENGTH, DNS_QUERY_TIMES))
M <- cor(numeric_var1)
corrplot(M, method = "circle")

options(repr.plot.width=8, repr.plot.height=8)
corrplot(M, method = "number")


