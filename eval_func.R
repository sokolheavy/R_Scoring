
#setwd("E:/scoring")

getwd()

library(ROCR)
library(ggplot2)
library(MLmetrics)
library(smbinning)
library(dplyr)
#data_log_regr <- read.csv2("WOE_CC_no_CSF_else.csv")

#recall column with "target",take universall name(target can be 'target_for_calc','target_for_calc_30' and so on) 
names(data_log_regr)[grep("target", names(data_log_regr))]<-"target"


#generate "data_log_regr" with first column "target" and another columns are sorted alphabetically
data_log_regr<-cbind(data_log_regr$target,(data_log_regr[ , sort(names(data_log_regr[,-which( colnames(data_log_regr)=="target")]))]))

#recall first column,cauth automatically programe calls it "data$target"
names(data_log_regr)[1]<-"target"

# if exists rows with Inf value - delete them 
#data_log_regr <- data_log_regr[!is.infinite(rowSums(data_log_regr)),]

# target name:
measurevar <- "target"

train <- data_log_regr
# Kappa = (accuracy - expected accuracy)/(1 - expected accuracy)
# expected accuracy = ( ((FP+TP)*TP)/all) + ((FN+TN)*TN)/all) )/all

############### create cross validation

i <- 3

eval_table <- data.frame()

eval_table <- CV(train, 5, eval_table)

CV <- function(train, k_fold = 5, eval_table = data.frame()) {
  # cv_list - sequence of data rows(1:number_of_rows)
  # train - data for modeling
  # k_fold - number of folds
 
  # Number of instances
  n.train <- nrow(train)
 
  # Prepare the folds
  s <- split(sample(n.train),rep(1:k_fold,length=n.train))
  
  # add initial model to 'eval_table'
  eval_table <- union_all(eval_table, data.frame(Gini, Model = paste0("Model_0_", i), Folds = "None"))

  ## Cross-validation
  
  # close 'warning()'
  options(warn = -1) 
  for (k in seq(k_fold)) {
    
    model <- glm(as.formula(paste(measurevar,paste(groupvars[[i]], collapse=" + "), sep=" ~ "))
                 ,train[-s[[k]],], family = "binomial")
    
    # add column with predicted values
    pred_target <- predict(object = model, type = "response")
    
    # add values for perfomance plots(only for calc)
    pred <- prediction(pred_target, train[-s[[k]], measurevar])
    Gini  <- round(((slot(performance(pred, measure = "auc"),"y.values")[[1]])*2 - 1), 4)
    eval_table <- union_all(eval_table, data.frame(Gini, Model = paste0("Model_", i), Folds = paste0("Fold_", k)))
  }
  
  eval_table 
}




# Accurany analysis
Gini_analysis1 <- function(eval_table) {
  cbind(dplyr::summarize(eval_table, mGini = mean(Gini), sdGini = sd(Gini)))
}

Gini_analysis <- function(eval_table) {
  eval_table <- group_by(eval_table, Model)
  GiniCV <- dplyr::summarize(filter(eval_table, Folds != "None"), # without initial model(model without CV)
                  mGini = mean(Gini), sdGini = sd(Gini)) 
                  
  GiniCV <- mutate(GiniCV, GiniCV = round(mGini,3), 
                           GiniCVInf = round((mGini - 2 * sdGini/sqrt(k_fold)),3),
                           GiniCVPAC = round((mGini - sdGini),3))
  GiniEmp <- round(filter(eval_table, Folds == "None")[,1],3)
  Gini_avaluation <- dplyr::select(cbind(GiniCV, GiniEmp), Model, Gini, GiniCV, GiniCVInf, GiniCVPAC)  
  Gini_avaluation
}



ggplot(data = reshape2::melt(Gini_avaluation, "Model"), 
             aes(x = Model, y = value, color = variable)) + 
        geom_point(size = 5) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), 
              plot.margin = grid::unit(c(1, 1, .5, 1.5), "lines"))








