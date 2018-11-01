#install.packages("ROCR")
#install.packages("ggplot2")
#install.packages("MLmetrics")

library(ROCR)
library(ggplot2)
library(MLmetrics)

file<-read.csv("WOE_CC_no_CSF_else.csv",sep=";")
data<-read.csv("Data_CC_no_CSF_else.csv",sep=";")
str(data)

#recall column with "target",take universall name(target can be 'target_for_calc','target_for_calc_30' and so on) 
names(data)[grep("target", names(data))]<-"target"
names(file)[grep("target", names(file))]<-"target"

#generate "data" with first column "target" and another columns are sorted alphabetically
data<-cbind(data$target,(data[ , sort(names(data[,-which( colnames(data)=="target")]))]))
file<-cbind(file$target,(file[ , sort(names(file[,-which( colnames(file)=="target")]))]))

#recall first column,cauth automatically programe calls it "data$target"
names(data)[1]<-"target"
names(file)[1]<-"target"

##create different type of models
model1 <- glm(target ~ Gender_x_Marital +
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

model2 <- glm(target ~ Gender_x_Marital +
                         Month_passport +
                             Industry12 +
                             Position12 +
                              Education +
                        Work_Experience +
                            credit_paym +
                             income_add +
                         phone_in_bureau, file, family = "binomial")

model3 <- glm(target ~ Gender_x_Marital +
                         Month_passport +
                             Industry12 +
                             Position12 +
                              Education +
                        Work_Experience +
                            credit_paym +
                             income_add +
                                 Gender +
                               Age_class, file, family = "binomial")
##KS

#add column with predicted values
file$target_predict_1<-predict(object = model1, type = "response")

#add values for perfomance plots
pred1 <- prediction(file$target_predict_1,file$target)

perf1 <- performance(pred1,"tpr","fpr")
ks=max(attr(perf1,'y.values')[[1]]-attr(perf1,'x.values')[[1]])
#KS_Stat(file$target_predict_1,file$target_for_calc)

#Plot with KS

#find probability, where KC is located
x_value_ks <- perf1@alpha.values[[1]][which((attr(perf1,'y.values')[[1]]-attr(perf1,'x.values')[[1]]) == ks)]

#point for plot(Good Rate)
cdf1 <- ecdf(subset(file$target_predict_1,file$target == 1))
#point for plot(Bad Rate)
cdf2 <- ecdf(subset(file$target_predict_1,file$target == 0))

y_point <- cdf1_new(x_value_ks) 
y2_point <- cdf2_new(x_value_ks) 

KS_plot <- ggplot(file, aes(x = target_predict_1, group = target ,colour = target))+
                  stat_ecdf(size=1) +
                  theme_bw(base_size = 25) +
                  theme(legend.position ="none") +
                  xlab("Probability") +
                  ylab("ECDF") +
                  geom_segment(aes(x = x_value_ks, y = y_point, xend = x_value_ks, yend = y2_point),
                               linetype = "dashed", color = "red") +
                  geom_point(aes(x = x_value_ks , y= y_point), color="red", size=5) +
                  geom_point(aes(x = x_value_ks , y= y2_point), color="red", size=5) +
                  ggtitle(paste0("K-S Test: ",round(ks*100, 2))) +
                  theme(legend.title=element_blank())


##ROC Curve

#gini = auc*2 - 1
gini  <- (slot(performance(pred1, measure = "auc"),"y.values")[[1]])*2 - 1
#"tpr","fpr" - True positive rate(TP/(P)), false positive rate(FP/N)
plot(performance(pred1,"tpr","fpr"),lwd=2)
lines(c(0,1),c(0,1))
text(0.6,0.2,paste("Gini=", round(gini,4), sep=""), cex=1.4)
title("ROC Curve")

#build plot with 3 roc curve 

gini1  <- (slot(performance(pred1, measure = "auc"),"y.values")[[1]])*2 - 1
gini2  <- (slot(performance(pred2, measure = "auc"),"y.values")[[1]])*2 - 1
gini3  <- (slot(performance(pred3, measure = "auc"),"y.values")[[1]])*2 - 1

plot(performance(pred1,"tpr","fpr"), type="l", lwd=2 ,col = "red")
par(new=TRUE)
plot(performance(pred2,"tpr","fpr"), type="l", lwd=2 ,col = "green")
par(new=TRUE)
plot(performance(add=T, pred3,"tpr","fpr"), type="l", lwd=2)
text(0.7,0.2,paste("Gini=", round(gini1,4),";
        ",round(gini2,4),";
        ",round(gini3,4),".", sep=""), cex=1.4)
lines(c(0,1),c(0,1))
title("ROC Curve")
legend(x = 0.85,y = 0.18, c("model1", "model2", "model3"), 
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



##Calc optimal cuttoff

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

#Add predictive mark
data$result <- ifelse(file$target_predict_1 < 0.9256235, 0, 1)

aggregate_table_fact <- aggregate(. ~ data[,2], data = data[c(names(data)[1],names(data)[2])],
                             FUN = function(x) c(good = sum(x),
                                                 bad=(length(x)-sum(x))))[,c(1,2)]

#log(x) will produce NaN any time x is less than zero(calculating 'length(x)-sum(x)' we have '-' func 'log' see that and returns error
aggregate_table_fact<-cbind(aggregate_table_fact[,1],data.frame(aggregate_table_fact[,2]))
names(aggregate_table_fact)<-c( "Month_passport" ,"good, #","bad, #")

table<-data.frame(row.names = aggregate_table_fact[,1],aggregate_table_fact[,2:3])
str(table)

chisq.test(table)


cost.perf = performance(pred1, "cost", cost.fp = 2, cost.fn = 1)
pred1@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
