#install.packages("ROCR")
#install.packages("pROC")
library(ROCR)
library(pROC)

file<-read.csv("WOE_CC_no_CSF_else.csv",sep=";")
data<-read.csv("Data_CC_no_CSF_else.csv",sep=";")

names(file)

#create different type of models
model1<-glm(target_for_calc ~ Gender_x_Marital +
                                Month_passport +
                                    Industry12 +
                                    Position12 +
                                     Education +
                               Work_Experience +
                                   credit_paym +
                                    income_add +
                               phone_in_bureau +
                                        Gender +
                                     Age_class, file, family = "binomial")
model2<-glm(target_for_calc ~ Gender_x_Marital +
                                Month_passport +
                                    Industry12 +
                                    Position12 +
                                     Education +
                               Work_Experience +
                                   credit_paym +
                                    income_add +
                                phone_in_bureau, file, family = "binomial")

model3<-glm(target_for_calc ~ Gender_x_Marital +
                                Month_passport +
                                    Industry12 +
                                    Position12 +
                                     Education +
                               Work_Experience +
                                   credit_paym +
                                    income_add +
                                        Gender +
                              Age_class, file, family = "binomial")

#add column with predicted values
file$target_predict_1<-predict(object = model1, type = "response")
file$target_predict_2<-predict(object = model2, type = "response")
file$target_predict_3<-predict(object = model3, type = "response")

#add variables for perfomance plots
pred1 <- prediction(file$target_predict_1,file$target_for_calc)
pred2 <- prediction(file$target_predict_2,file$target_for_calc)
pred3 <- prediction(file$target_predict_3,file$target_for_calc)
#str(pred1)
#sapply(slotNames(pred1), function(x) slot(pred1, x))

#roc curve

#"tpr","fpr" - True positive rate(TP/(P)), false positive rate(FP/N)
gini  <- (slot(performance(pred1, measure = "auc"),"y.values")[[1]])/2
plot(performance(pred1,"tpr","fpr"),lwd=2)
lines(c(0,1),c(0,1))
text(0.6,0.2,paste("Gini=", round(gini,4), sep=""), cex=1.4)
title("ROC Curve")

#build plot with 3 roc curve 

gini1  <- (slot(performance(pred1, measure = "auc"),"y.values")[[1]])/2
gini2  <- (slot(performance(pred2, measure = "auc"),"y.values")[[1]])/2
gini3  <- (slot(performance(pred3, measure = "auc"),"y.values")[[1]])/2

plot(performance(pred1,"tpr","fpr"), type="l", lwd=2 ,col = "red")
par(new=TRUE)
plot(performance(pred2,"tpr","fpr"), type="l", lwd=2 ,col = "green")
par(new=TRUE)
plot(performance(add=T, pred3,"tpr","fpr"), type="l", lwd=2)
text(0.6,0.2,paste("Gini=", round(gini1,4),";
        ",round(gini2,4),";
        ",round(gini3,4),".", sep=""), cex=1.4)
lines(c(0,1),c(0,1))
title("ROC Curve")
legend(x = 0.75,y = 0.1, c("model1", "model2", "model3"), 
       lty = 1, col =c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)


#bad distribution (TN/N)
perf3  <- performance(pred1, x.measure = "cutoff", measure = "spec")
plot(perf3, col = "red", lwd =2)

#good distribution (TP/P)
perf4  <- performance(pred1, x.measure = "cutoff", measure = "sens")
plot(add=T, perf4 , col = "green", lwd =2)

#acc=(TP+TN)/(N+P)
perf5  <- performance(pred1, x.measure = "cutoff", measure = "acc")
plot(add=T, perf5, lwd =2)

#Add legend
legend(x = 0.6,y = 0.3, c("spec", "sens", "accur"), 
       lty = 1, col =c('red', 'green', 'black'), bty = 'n', cex = 1, lwd = 2)



#calc optimal cuttoff

opt.cut = function(perf, pred1){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred1@cutoffs)
}

roc_perf<-performance(pred1,"tpr","fpr")
print(opt.cut(roc_perf, pred1))
#False negative rate = y-1 = TP/P -1 = TP/(TP+FN) - (TP+FN)/(TP+FN) = -TN/(TP+FN) 
#specificity = 1-x[[ind]]= 1-FP/N=1- FP/(TN+FP)=((TN+FP)/(TN+FP)) - FP/(TN+FP)= TN/(TN+FP) = TN/N - False positive rate(specificity)


#Limiting to a FPR: partial ROC curve
pROC = function(pred, fpr.stop){
  perf <- performance(pred,"tpr","fpr")
  for (iperf in seq_along(perf@x.values)){
    ind = which(perf@x.values[[iperf]] <= fpr.stop)
    perf@y.values[[iperf]] = perf@y.values[[iperf]][ind]
    perf@x.values[[iperf]] = perf@x.values[[iperf]][ind]
  }
  return(perf)
} 

pROC(pred1,0.15)
proc_perf = pROC(pred1, fpr.stop=0.2)
plot(proc_perf)
abline(a=0, b= 1)


cost.perf = performance(pred1, "cost", cost.fp = 2, cost.fn = 1)
pred1@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
