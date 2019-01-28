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
eval_table <- data.frame()

i <- 1
# data_log_regr <- data_log_regr[1:500,]
# table(data_log_regr[,1])
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
  
  #Cheak model by Cross-validation
  eval_table <- CV(data_log_regr, 5, eval_table)
  
}


ggplot(data = reshape2::melt(Gini_analysis(eval_table), "Model"), 
       aes(x = Model, y = value, color = variable)) + 
  geom_point(size = 5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.margin = grid::unit(c(1, 1, .5, 1.5), "lines"))


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




df = metrics_pdf(dataset=data_log_regr,prediction="pred_target",actualclass="target",
                 report=1, returndf=0) # Show report


metrics_pdf=function(dataset,prediction,actualclass,cutoff=NA,report=1,plot="none", returndf=0){
  dataset_first <- dataset
  i=which(names(dataset)==actualclass) # Find Column for actualclass
  j=which(names(dataset)==prediction) # Find Column for prediction
  if(!is.data.frame(dataset)){ # Check if data.frame
    return("Data not a data.frame.")
  } else if (!is.na(cutoff) & !is.numeric(cutoff)){
    return("'cutoff' must be numeric.")
  } else if(!is.numeric(dataset[,which(names(dataset)==prediction)])){
    return("'prediction' not found.")
  } else if(max(dataset[,i],na.rm=TRUE)!=1 | min(dataset[,i],na.rm=TRUE)!=0){
    return("'actualclass' must be binary (0/1).")
  } else if(length(unique(na.omit(dataset[,c(actualclass)])))!=2){
    return("'actualclass' must be binary (0/1).")
  } else if(report!=1 & report!=0){ 
    return("'report' must be 0 (Deactivated) or 1 (Activated).")
  } else if(returndf!=1 & returndf!=0){ 
    return("'df' must be 0 (Deactivated) or 1 (Activated).")
  } else if(plot!="auc" & plot!="ks" & plot!="none"){ 
    return("'plot' options are: 'auc', 'ks' or 'none'.")
  } else if(!is.na(cutoff) & (max(dataset[,j],na.rm=TRUE)<cutoff | min(dataset[,j],na.rm=TRUE)>cutoff)){
    return("'cutoff' out of range.")
  }
  else {
    
    # Create table and its basic structure
    dataset=dataset[,c(prediction,actualclass)] # Only Score and class
    nmiss=nrow(dataset)-nrow(na.omit(dataset)) # Missing rows
    dataset=na.omit(dataset) # Complete Cases Only
    df=table(dataset[,c(prediction)],dataset[,c(actualclass)]) # Group by prediction
    df=as.data.frame.matrix(df) # Prediction becomes rowname 
    df$Prediction=rownames(df) # Bring rowname as a column with values
    rownames(df)=NULL
    names(df)[names(df)=="0"]="CntBad"
    names(df)[names(df)=="1"]="CntGood"
    df=df[,c("Prediction","CntGood","CntBad")] #Reorder
    df$CntTotal=df$CntBad+df$CntGood
    
    # Cumulative Ascending
    df$CumAscGood=cumsum(df$CntGood)
    df$CumAscBad=cumsum(df$CntBad)
    df$CumAscTotal=cumsum(df$CntTotal)
    
    # Totals for upcoming calculations
    SumRecords=sum(df$CntTotal)
    SumGoods=sum(df$CntGood)
    SumBads=sum(df$CntBad)
    
    # Cumulative Descending
    df$CumDescGood=NA
    c=which(names(df)=="CumDescGood")
    df[1,c]=sum(df$CntGood)
    for (i in 2:nrow(df)) {
      df[i,c]=df[1,c]-df[i-1,5]
    }
    
    df$CumDescBad=NA
    c=which(names(df)=="CumDescBad")
    df[1,c]=sum(df$CntBad)
    for (i in 2:nrow(df)) {
      df[i,c]=df[1,c]-df[i-1,6]
    }
    
    df$CumDescTotal=NA
    c=which(names(df)=="CumDescTotal")
    df[1,c]=sum(df$CntTotal)
    for (i in 2:nrow(df)) {
      df[i,c]=df[1,c]-df[i-1,7]
    }
    
    # Cumulative Percent (Column/Vertical Calculation)
    df$PctCumDescTotal=df$CumDescTotal/SumRecords
    
    # Good/Bad Rates (Row/Horizontal Calculation)
    df$GoodRateRow=df$CntGood/df$CntTotal
    df$BadRateRow=df$CntBad/df$CntTotal
    df$GoodRateDesc=df$CumDescGood/df$CumDescTotal
    df$BadRateDesc=df$CumDescBad/df$CumDescTotal
    
    # Cumulative Percentage
    df$PctCumAscGood=df$CumAscGood/SumGoods
    df$PctCumAscBad=df$CumAscBad/SumBads
    
    # Remove ascending stats to avoid confusion
    df$CumAscGood=NULL
    df$CumAscBad=NULL
    df$CumAscTotal=NULL
    
    # TN and FN
    df$FN=SumGoods-df$CumDescGood
    df$TN=SumBads-df$CumDescBad
    
    # Accuracy
    df$Accuracy=(df$CumDescGood+df$TN)/(df$TN+df$FN+df$CumDescGood+df$CumDescBad)
    
    # Specificity, Sensitivity
    df$Sensitivity=df$CumDescGood/(df$CumDescGood+df$FN)
    df$Specificity=df$TN/(df$TN+df$CumDescBad)
    
    # False Positive Rate
    df$FalPosRate=df$CumDescBad/(df$CumDescBad+df$TN)
    
    # Precision
    df$Precision=df$CumDescGood/(df$CumDescGood+df$CumDescBad)
    
    # Inverse Precision
    df$InvPrecision=df$TN/(df$FN+df$TN)
    
    # Youden Index
    df$YoudenJ=df$Sensitivity+df$Specificity-1
    # max(df$YoudenJ)
    
    # Optimal Cutoff
    optcut=df[df$YoudenJ==max(df$YoudenJ), ]$Prediction
    df$YoudenJ=NULL
    optcutcomment=" (Optimal)"
    
    # If cutoff is specified
    if(!is.na(cutoff)){
      if ((cutoff %in% df$Prediction)==FALSE){
        optcut=df[which.min(abs(as.numeric(df$Prediction)-cutoff)),1]
        optcutcomment=paste0(" (",cutoff," Not Found)")
      } else {
        optcut=cutoff
        optcutcomment=" (User Defined)"
        
      }
    }
    
    # For AUC Calculation (Trapezoid Method)
    df$TPR=df$Sensitivity
    df$FPR=1-df$Specificity
    df$MgAUC=0
    a=which(names(df)=="MgAUC")
    f=which(names(df)=="FPR")
    t=which(names(df)=="TPR")
    for (i in 1:nrow(df)-1) {
      df[i,a]=0.5*(df[i,t]+df[i+1,t])*(df[i,f]-df[i+1,f])
    }
    
    # AUC
    auc=sum(df$MgAUC)
    df$TPR=NULL
    df$FPR=NULL
    df$MgAUC=NULL
    
    # AUC Evaluation
    auceval=ifelse(auc<0.6,"Unpredictive",
                   ifelse(auc<0.7,"Poor",
                          ifelse(auc<0.8,"Fair",
                                 ifelse(auc<0.9,"Good","Excellent")))) 
    
    # KS
    df$MgKS=abs(df$PctCumAscGood-df$PctCumAscBad)
    ks=as.numeric(max(df$MgKS))
    scoreks=as.numeric(df[df$MgKS==ks, ]$Prediction)
    cgks=as.numeric(df[df$MgKS==ks, ]$PctCumAscGood)
    cbks=as.numeric(df[df$MgKS==ks, ]$PctCumAscBad)
    df$MgKS=NULL
    
    # KS Evaluation
    kseval=ifelse(ks<0.3,"Unpredictive",
                  ifelse(ks<0.4,"Fair",
                         ifelse(ks<0.5,"Good",
                                ifelse(ks<0.6,"Excellent", 
                                       ifelse(ks<0.7,"Awesome","That Good. Really?")))))
    # Evaluate Log model
    
    n <- dim(as.matrix(dataset_first))[1]; m <- dim(as.matrix(dataset_first))[2]-1 #- prediction column
    
    #residual sum of squares
    RSS <- sum((dataset_first[,actualclass] - dataset_first[,prediction]) * (dataset_first[,actualclass]  - dataset_first[,prediction]))
    
    #root mean square error
    RMSE <- sqrt(RSS/dim(dataset_first)[[1]])
    
    #residual standard error
    RSE <- sqrt(RSS/(n - m - 1)) 
    
    #Rsquared, Fisher cr
    Rsquared <- round((1 - RSS/sum((mean(dataset_first[,actualclass]) - dataset_first[,actualclass])^2)),4)
    # F(fact) > F(????(teor) <- good model
    Fisher_cr <- (sum((mean(dataset_first[,actualclass]) - dataset_first[,prediction])^2)/m)/(RSS/(n - m - 1))
    p_val <- pf(q = Fisher_cr, df1 = m, df2 = (n - m - 1), lower.tail = FALSE)
    
    k <- extractAIC(model)[1]
    AIC <- extractAIC(model)[2]
    AIC <- AIC(model)
    AICc <- AIC(model) + 2*k*(k + 1)/(n - k - 1)
    BIC <- BIC(model)
    BIC <- AIC(model, k = log(n))
    
    # loglikelihood
    #py - y pred
    loglikelihood <- function(y, py) {
      sum(y * log(py) + (1-y)*log(1 - py))
    }
    
    pnull <- mean(as.numeric(dataset$atRisk))
    
    pred <- predict(model, newdata=train, type="response")
    resid.dev <- -
      2*loglikelihood(as.numeric(train$atRisk), pred)
    
    # psevdo Rsquare
    # 1- (resid.dev/null.dev)
    lgLik <- logLik(model)
    D.null <- model$null.deviance 
    D <- model$deviance
    df <- with(model, df.null - df.residual)
    p <- pchisq(D.null - D, df, lower.tail = FALSE)
    PRsquare = 1 - D/D.null
    
    #entropy - avg qty of information(measure - bit) you can take, if you remove a certain instance of class
    #conditionalEntropy - show how much qty of information decrease due to the fact of 'dirty' data
    entropy <- function(x) { 
      xpos <- x[x > 0]
      scaled <- xpos/sum(xpos) ; sum(-scaled*log(scaled, 2))
    }
    print(entropy(table(spamTest$spam))) 
    
    
    conditionalEntropy <- function(t) {
      (sum(t[, 1])*entropy(t[, 1]) + sum(t[, 2])*entropy(t[, 2]))/sum(t)
    }
    print(conditionalEntropy(cM))
    
    
    
    
    # If report is activated (report = 1)
    if(report==1) {
      
      # Confusion Matrix Components
      tp=df[df$Prediction==optcut, ]$CumDescGood
      fp=df[df$Prediction==optcut, ]$CumDescBad
      fn=df[df$Prediction==optcut, ]$FN
      tn=df[df$Prediction==optcut, ]$TN
      p=SumGoods
      n=SumBads
      recsabovecutoff=df[df$Prediction==optcut, ]$CumDescTotal/SumRecords
      goodrate=df[df$Prediction==optcut, ]$GoodRateDesc
      badrate=df[df$Prediction==optcut, ]$BadRateDesc
      
      # Report on Metrics
      admetrics=character()
      admetrics=paste0(admetrics, "\n")
      admetrics=paste0(admetrics, "  Overall Performance Metrics \n")
      admetrics=paste0(admetrics, "  -------------------------------------------------- \n")
      admetrics=paste0(admetrics, "                     KS : ",sprintf("%.4f",round(ks,4))," (",kseval,")\n")
      admetrics=paste0(admetrics, "                    AUC : ",sprintf("%.4f",round(auc,4))," (",auceval,")\n")
      admetrics=paste0(admetrics, "\n")
      admetrics=paste0(admetrics, "  Classification Matrix \n")
      admetrics=paste0(admetrics, "  -------------------------------------------------- \n")
      admetrics=paste0(admetrics, "            Cutoff (>=) : ",round(as.numeric(optcut),4),optcutcomment,"\n")
      admetrics=paste0(admetrics, "    True Positives (TP) : ",tp,"\n")
      admetrics=paste0(admetrics, "   False Positives (FP) : ",fp,"\n")
      admetrics=paste0(admetrics, "   False Negatives (FN) : ",fn,"\n")
      admetrics=paste0(admetrics, "    True Negatives (TN) : ",tn,"\n")
      admetrics=paste0(admetrics, "    Total Positives (P) : ",p,"\n")
      admetrics=paste0(admetrics, "    Total Negatives (N) : ",n,"\n")
      admetrics=paste0(admetrics, "\n")
      admetrics=paste0(admetrics, "  Business/Performance Metrics \n")
      admetrics=paste0(admetrics, "  -------------------------------------------------- \n")
      admetrics=paste0(admetrics, "       %Records>=Cutoff : ",sprintf("%.4f",round(recsabovecutoff,4)),"\n")
      admetrics=paste0(admetrics, "              Good Rate : ",sprintf("%.4f",round(goodrate,4))," (Vs ",sprintf("%.4f",round(SumGoods/SumRecords,4))," Overall)\n")
      admetrics=paste0(admetrics, "               Bad Rate : ",sprintf("%.4f",round(badrate,4))," (Vs ",sprintf("%.4f",round(SumBads/SumRecords,4))," Overall)\n")
      admetrics=paste0(admetrics, "         Accuracy (ACC) : ",sprintf("%.4f",round((tp+tn)/(tp+fp+tn+fn),4)),"\n")
      admetrics=paste0(admetrics, "      Sensitivity (TPR) : ",sprintf("%.4f",round(tp/p,4)),"\n")
      admetrics=paste0(admetrics, "  False Neg. Rate (FNR) : ",sprintf("%.4f",round(fn/p,4)),"\n")
      admetrics=paste0(admetrics, "  False Pos. Rate (FPR) : ",sprintf("%.4f",round(fp/n,4)),"\n")
      admetrics=paste0(admetrics, "      Specificity (TNR) : ",sprintf("%.4f",round(tn/n,4)),"\n")
      admetrics=paste0(admetrics, "        Precision (PPV) : ",sprintf("%.4f",round(tp/(tp+fp),4)),"\n")
      admetrics=paste0(admetrics, "   False Discovery Rate : ",sprintf("%.4f",round(fp/(tp+fp),4)),"\n")
      admetrics=paste0(admetrics, "     False Omision Rate : ",sprintf("%.4f",round(fn/(fn+tn),4)),"\n")
      admetrics=paste0(admetrics, "   Inv. Precision (NPV) : ",sprintf("%.4f",round(tn/(fn+tn),4)),"\n")
      admetrics=paste0(admetrics,"\n")
      admetrics=paste0(admetrics, "   Evaluation of log model \n")
      admetrics=paste0(admetrics, "  -------------------------------------------------- \n")
      admetrics=paste0(admetrics, "       psevdo R_squared : ",sprintf("%.4f",PRsquare),"\n")
      admetrics=paste0(admetrics, "                   AIC  : ",sprintf("%.4f",AIC),"\n")
      admetrics=paste0(admetrics, "                   BIC  : ",sprintf("%.4f",BIC),"\n")
      admetrics=paste0(admetrics, "                  AICc  : ",sprintf("%.4f",AICc),"\n")
      admetrics=paste0(admetrics,"\n")
      admetrics=gsub(", ","",admetrics)
      
      # Metric report
      cat(admetrics)
      return(admetrics)
    } # End if for report
    
    if(returndf==1){return(df)}
  } # Close else
} # Close function
