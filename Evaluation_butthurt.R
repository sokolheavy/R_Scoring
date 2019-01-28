################### Evaluating models(Gini, KS)
# load function 'metrics_pdf'
# install.packages("ROCR")
# install.packages("ggplot2")
# install.packages("MLmetrics")

#setwd("F:/scoring")

library(ROCR)
library(ggplot2)
library(MLmetrics)
library(smbinning)
library(dplyr)
library(ggpubr)
#data_log_regr <- read.csv2("cc_with_pil_bin.csv")

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

# method to transform model: model2 <- update(model1, .~. - Age)
# Choose variables for different models:
groupvars = list()
groupvars[[1]] <- names(data_log_regr[,-1])[-1]
groupvars[[2]] <- names(data_log_regr[,-1])[-2]
groupvars[[3]] <- names(data_log_regr[,-1])[-3]
groupvars[[4]] <- names(data_log_regr[,-1])[-4]

# number of models
nm <- length(groupvars)

## Firstly cheak all model by Cross-validation
# combine evauate parameters(Gini) for all models
# k_fold=5 - default value

eval_table <- CV(train = data_log_regr, k_fold = 6)


ggplot(data = reshape2::melt(Gini_analysis(eval_table), "Model"), 
       aes(x = Model, y = value, color = variable)) + 
  geom_point(size = 5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.margin = grid::unit(c(1, 1, .5, 1.5), "lines"))

# select models, that suit fyi

####Welcome to hell


################## create pdf file 
pdf("Evaluation_of_models.pdf",width = 7, height=14, pointsize = 12, paper='special')
options(warn = -1) 


# loop, that creates 3 graphics(Gini, KS, Statistical graphic) 
for (i in 1:nm) {
  
  # This returns the model:
  model <- glm(as.formula(paste(measurevar,paste(groupvars[[i]], collapse=" + "), sep=" ~ "))
               ,data_log_regr, family = "binomial")
  
  # add column with predicted values
  pred_target <- predict(object = model, type = "response")
  
  # add values for perfomance plots(only for calc)
  pred <- prediction(pred_target, data_log_regr$target)
  perf <- performance(pred,"tpr","fpr") 
  
  ##### Plot with KS
  
  ks = max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])
  
  #find probability, where KC is located
  x_value_ks <- perf@alpha.values[[1]][which((attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]]) == ks)]
  
  #point for plot(Good Rate)
  cdf1 <- ecdf(subset(pred_target, data_log_regr$target == 1))
  #point for plot(Bad Rate)
  cdf2 <- ecdf(subset(pred_target, data_log_regr$target == 0))
  
  y_point <- cdf1(x_value_ks) 
  y2_point <- cdf2(x_value_ks) 
  
  # KS plot 
  KS_plot <- ggplot(data_log_regr, aes(x = pred_target, group = target ,colour = target))+
    stat_ecdf(size=1) +
    theme_bw(base_size = 25) +
    xlab("Probability") +
    ylab("ECDF") +
    geom_segment(aes(x = x_value_ks, y = y_point, xend = x_value_ks, yend = y2_point),
                 linetype = "dashed", color = "red") +
    geom_point(aes(x = x_value_ks , y= y_point), color="red", size=5) +
    geom_point(aes(x = x_value_ks , y= y2_point), color="red", size=5) +
    ggtitle(paste0("K-S Test= ",round(ks*100, 2))) +
    scale_x_continuous(breaks=seq(0,1,0.1)) +
    scale_y_continuous(breaks=seq(0,1,0.2)) +
    theme(legend.position ="none")
  
  ##### ROC Curve
  
  #gini = auc*2 - 1
  gini  <- (slot(performance(pred, measure = "auc"),"y.values")[[1]])*2 - 1
  
  gini_plot <- ggplot(setNames(data.frame(perf@x.values, perf@y.values), c('x_val', 'y_val')), 
                      aes(x = x_val, y = y_val), color=sort_criterion) + 
    geom_line(aes(group=1), colour="#000099", size=1) + 
    geom_abline(color="gray") +
    ggtitle(paste("Gini=", round(gini,4), sep="")) +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    theme_bw(base_size = 20) +
    scale_x_continuous(breaks=seq(0,1,0.2)) +
    scale_y_continuous(breaks=seq(0,1,0.2)) +
    theme(legend.position ="none")
  
  
  ##### Statistical graphics
  ## Calc optimal cuttoff
  
  opt.cut = function(perf, pred){
    cut.ind = mapply(FUN=function(x, y, p){
      d = (x)^2 + (y-1)^2
      ind = which(d == min(d))
      c(sensitivity = y[[ind]], 
        specificity = 1-x[[ind]], 
        Optimal_cutoff = p[[ind]])
    }, perf@x.values, perf@y.values, pred@cutoffs)
  }
  
  opt_cut <- opt.cut(perf, pred)
  #False negative rate = y-1 = TP/P -1 = TP/(TP+FN) - (TP+FN)/(TP+FN) = -TN/(TP+FN) 
  #specificity = 1-x[[ind]]= 1-FP/N=1- FP/(TN+FP)=((TN+FP)/(TN+FP)) - FP/(TN+FP)= TN/(TN+FP) = TN/N - False positive rate(specificity)
  
  # Create plot  
  
  #bad distribution (TN/N)
  perf1  <- performance(pred, x.measure = "cutoff", measure = "spec")
  #good distribution (TP/P)
  perf2  <- performance(pred, x.measure = "cutoff", measure = "sens")
  #acc=(TP+TN)/(N+P)
  perf3  <- performance(pred, x.measure = "cutoff", measure = "acc")
  
  df <- rbind(rbind(setNames(data.frame(perf1@x.values, perf1@y.values, 'spec'), c('x_val', 'value', 'variable')),
                    setNames(data.frame(perf2@x.values, perf2@y.values, 'sens'), c('x_val', 'value', 'variable'))),
              setNames(data.frame(perf3@x.values, perf3@y.values, 'acc'), c('x_val', 'value', 'variable')))
  
  stat_plot <- ggplot(df, aes(x=x_val, y=value, color = as.factor(variable))) +
    geom_line(size=0.75) +
    theme_bw(base_size = 20) +
    ggtitle("Statistical graphics") +
    xlab("") +
    ylab("") +
    scale_color_manual(values = c("red", "steelblue", "gray27")) +
    scale_x_continuous(breaks=seq(0,1,0.1)) +
    scale_y_continuous(breaks=seq(0,1,0.2)) +
    theme(legend.position="bottom",
          legend.title=element_blank()) 
  
  # Create a 'confusion_matrix' 
  cM <- table(truth=data_log_regr$target, prediction=pred_target> opt_cut[3,1]) 
  
  stat_table <- data.frame(Measure = c("Accuracy", "Precision", "Sensitivity","Specificity","Optimal_cutoff"),
                           Formula = c("(TP+TN)/(TP+FP+TN+FN)", "TP/(TP+FP)", "TP/(TP+FN)", "TN/(TN+FP)",""),
                           Value=round(c((cM[1,1]+cM[2,2])/sum(cM), # Accuracy
                                         cM[2,2]/(cM[2,2]+cM[1,2]), # Precision
                                         cM[2,2]/(cM[2,2]+cM[2,1]), # Sensitivity
                                         cM[1,1]/(cM[1,1]+cM[1,2]),
                                         opt_cut[3,1]),4)) # Specificity
  
  table <- ggtexttable(df, rows = NULL, theme = ttheme("lBlueWhite"))
  
  text <- paste0("     
                 ","Model ", i)
  
  
  # set style of 'text1'
  title <- ggparagraph(text = text, face = "italic", size = 25, color = "black")
  
  
  # union 4 object in one file: 
  print(ggarrange(title, gini_plot, KS_plot, stat_plot, table , 
                  ncol = 1, nrow = 5,heights = c(0.08, 0.25, 0.25, 0.25, 0.13)))
  
}


dev.off()



################adjuvant func

###Cross-validation

CV <- function(train, k_fold = 5, eval_table = data.frame()) {
  # cv_list - sequence of data rows(1:number_of_rows)
  # train - data for modeling
  # k_fold - number of folds
  
  # Number of instances
  n.train <- nrow(train)
  
  # Prepare the folds
  s <- split(sample(n.train),rep(1:k_fold,length=n.train))
  
  ### Cheak all model 
  # nm - number of models
  for (i in seq(nm)) {
  # Calc Gini for initial model
  model <- glm(as.formula(paste(measurevar,paste(groupvars[[i]], collapse=" + "), sep=" ~ "))
               ,train, family = "binomial")
  pred <- prediction(predict(object = model, type = "response"), data_log_regr$target)
  Gini  <- (slot(performance(pred, measure = "auc"),"y.values")[[1]])*2 - 1
  
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
}  
  
  
  eval_table 
}



###Cheak and evauate evaluation

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
