# install.packages("smbinning")
# install.packages("odbc")
# install.packages("data.table")
# install.packages("gridExtra")
# install.packages("ggpubr")
# install.packages("cowplot")
# install.packages("corrplot")
# install.packages("tidyr")
# install.packages("devtools")
# install.packages("tibble")
# install.packages("rpart")
# install.packages("InformationValue") 
# install.packages("corrplot")

library("odbc")
library(data.table)
#library(woe) #install this package from 'by hands', instruction -> (S:\0.Learning\R\Github_packages)
library(devtools)
source <- devtools:::source_pkg("S:/0.Learning/R/Github_packages")
install(source)

library(woe)
library(smbinning) 
library(classInt)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(lazyeval)
library(corrplot)
library(gtable)
library(grid)
library(tidyr)
library(tibble)
library(rpart)
library(InformationValue)
library(corrplot)

setwd("S:/PROJECTS/SCORING/0. Scorecard_Creation/R scripts")
################ Get sample

#setwd("S:/PROJECTS/SCORING/0. Scorecard_Creation/R scripts")
#set data for binning from Csv
#work_data <- read.csv2("cc_no_csf_else_bin.csv")

con_k <- dbConnect(odbc(),Driver = "SQL Server",Server = "khazardbp02\\hazard",Database = "Risk_test",trusted_connection=TRUE)
con_d <- dbConnect(odbc(),Driver = "SQL Server",Server = "dhazardbp01\\hazard",Database = "Risk_test",trusted_connection=TRUE)

sample_base <- as.data.table(dbGetQuery(con_k, "select top 100 * from risk_test.dbo.Scoring_DataMart_CC_base where target_60max12m in ('good', 'bad')"))

sample_appl <- as.data.table(dbGetQuery(con_d, "select t.* from risk_test.dbo.Scoring_DataMart_CC_appl as t join risk_test.dbo.Scoring_DataMart_CC_base as b on t.id_order = b.id_order where target_60max12m in ('good', 'bad')"))
sample_bureau <- as.data.table(dbGetQuery(con_d, "select t.* from risk_test.dbo.Scoring_DataMart_CC_bureau as t join risk_test.dbo.Scoring_DataMart_CC_base as b on t.id_order = b.id_order where target_60max12m in ('good', 'bad')"))

#join table - create all sample of data if you need
setkey(sample_base, id_order)
setkey(sample_appl, id_order)
total_sample<-sample_base[sample_appl, nomatch = 0]

setkey(sample_bureau, id_order)
total_sample<-total_sample[sample_bureau, nomatch = 0]

rm(sample_base)
rm(sample_appl)
rm(sample_bureau)

#for work with specified scoring variables you can choose columns from total_sample or create it in other way
work_data<-as.data.frame(total_sample[,c(1,90,94:(93+6))])

# Add 'target_for_calc'(1,0), if it doesn't exist
work_data <- add_column(work_data, 
                        target_for_calc = ifelse(work_data$target_60max12m == "good", 1,0), 
                        .after = "target_60max12m")

#save(work_data, file = "work_data.rda")

################ Create binning data 

# initial number of columns
begin_ncol <- ncol(work_data)

#initiate target column and first column with variables
target_ncol <- match("target_60max12m",names(work_data))

variable_fc <- 5 #put number of first variable column

target_calc <- match("target_for_calc",names(work_data))

#replace NA 
#from column to column by cicle or other way (for example from handy_scripts)
n1<- variable_fc
n2<- begin_ncol
for (i in c(n1:n2))
{ work_data[is.na(work_data[i]), i] <- -99
}

# binning every variable with different type of binning('equal', 'kmeans', 'smbinning')
bin_col <- match(names(work_data[,(variable_fc):ncol(work_data)])[(as.vector(sapply(work_data[,(variable_fc):ncol(work_data)], function(x) is.numeric(x))))],
                 names(work_data))

for (i in bin_col) 
{ 
  
  # equal 
  # Idea: equal number of observation in every class
  
  # create intervals
  eq<-classIntervals(work_data[,i], 5, style = 'quantile')
  
  # column name for new binning column, that bins with 'equal' method
  colname <- paste(names(work_data)[i], "cat_eq", sep="_")
  
  # set column, that bins with 'equal' method
  work_data[[colname]] <- with(work_data, cut(work_data[,i], 
                                    c(min(work_data[,i])-1,unique(eq$brks)),include.lowest = TRUE, 
                                    right = FALSE, ordered = TRUE))
  
  # for saving ordered factors all repleacements must be done on factor levels, not on work_data(!!!)
  levels(work_data[[colname]])<-gsub(",", ";", levels(work_data[[colname]]))
  
  # kmeans 
  # Idea: search k 'center' points, that have biggest spreading of points around this 'center'(every interval have his own 'center' point)
  
  # k in {6,5,4,3,2}
  interval_count<-6
  repeat{
    km<-classIntervals(work_data[,i], interval_count, style = 'kmeans')
    colname <- paste(names(work_data)[i], "cat_km", sep="_")
    work_data[[colname]] <- with(work_data, cut(work_data[,i], km$brks,include.lowest = TRUE, 
                                      right = FALSE, ordered = TRUE))
    levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
    interval_count<-interval_count-1
    
    # any class have to be bigger than 3% of all work_data
    if (min(table(work_data[[colname]])/(nrow(work_data)/100)>3)) {break}
    
    # interval_count - {6,5,4,3,2}, if interval_count<2 - end
    if (interval_count<2) {break}
  }
  
  # smbinning
  # Idea: 'optimal binning' (maximization IV)
  sb<-smbinning(work_data,y=names(work_data)[target_calc],x=names(work_data)[i])
  if (length(sb) > 1) ##check this condition !!!!
  {colname <- paste(names(work_data)[i], "cat_sb", sep="_")
  work_data[[colname]] <- with(work_data, cut(work_data[,i], c(min(work_data[,i])-1,unique(sb$bands)),
                                    right = TRUE, left = FALSE, ordered = TRUE))
  levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
  }
  # save binnings intervals into 'rda' files
  #save(eq, km, sb, file = paste(names(work_data[i]),".rda", sep = ""))
}

rm(eq)
rm(km)
rm(sb)
rm(bin_col)
rm(interval_count)


## Create variable "bin_data" only with bining data (order columns by IV(in descending order))

# select only bining data (remove column without binning) and initial column that you need
#bin_data <- work_data[,c(1, target_ncol, target_calc,(begin_ncol+1):ncol(work_data))]
#ncol(bin_data)

#if original columns and categorical are arranged in other order or not all original columns have categorical 
#you can use this one
#remove original columns
bin_data<-work_data
n1<- variable_fc
n2<- length(names(bin_data))

for (i in c(n2:n1))
{ 
  print(paste(i, names(bin_data[i])), quote = FALSE)
  if (length(grep(paste(names(bin_data[i]), "_cat", sep = ""), names(bin_data))) > 0)
  {bin_data[i] <- NULL
  print("delete", quote = FALSE)
  }
  else
  {print("skip", quote = FALSE)}
}

rm(n1)
rm(n2)

#for simplification save in bin_data only id_order, target and variables
#if you need more columns put them between id_order and target
bin_data <- bin_data[c(match("id_order",names(bin_data))
                       ,c(1:(variable_fc-1))[-match("id_order",names(bin_data))][-(match(names(work_data[target_ncol]),names(bin_data))-1)][-(match("target_for_calc",names(bin_data))-2)]
                       ,match(names(work_data[target_ncol]),names(bin_data))
                       ,match("target_for_calc",names(bin_data))
                       ,variable_fc:length(names(bin_data)))]

target_ncol_bin <- match(names(work_data[target_ncol]),names(bin_data))
target_calc_bin <- match("target_for_calc",names(bin_data))
variable_fc_bin <- target_calc_bin + 1
bin_ncol <- ncol(bin_data)

## All variables should have 'factor' type, so convert variables, that not is a 'factor', to 'factor' 

# select variables that shouldn`t conver to the 'fator'
factor_vars <- c(names(bin_data)[1:(variable_fc_bin-1)]
                 ,names(which(sapply(bin_data[,variable_fc_bin:ncol(bin_data)], is.factor))))

# convert "unfactor" variables to 'factor'
bin_data[setdiff(names(bin_data),factor_vars)] <- data.frame(
                                                      sapply(
                                                        select(bin_data, -factor_vars), as.factor))
## IV - statistic table
iv_table <- arrange(cbind.data.frame(variables = names(bin_data[variable_fc_bin:bin_ncol])
                                     ,IV = sapply(bin_data[variable_fc_bin:bin_ncol], function(x) round(IV(X=x, Y=bin_data$target_for_calc)[1],4)), row.names = NULL),
                    desc(IV))

# Add strength of variables
iv_table$Strength <- ifelse(iv_table$IV>=1, "Suspicious",
                            ifelse(iv_table$IV>=.5, "Very strong",
                                   ifelse(iv_table$IV>=.2, "Strong",
                                          ifelse(iv_table$IV>=.1, "Average",
                                                 ifelse(iv_table$IV>=.02, "Weak", "Wery weak")))))

## Create loop for BR (add columns with BR for every binning variable)

bin_ncol<-ncol(bin_data)
for (i in variable_fc_bin:bin_ncol){
  
  #create 'br_table'. It consists of 2 column("BR" + name_of_variables, BR_value)
  var_for_group <- names(bin_data)[i]
  column_br <- paste("BR", 
                     names(bin_data)[i]
                     , sep="_")
  
  br_table <- bin_data %>%
    select(c(i,target_for_calc)) %>%
    group_by_(.dots = var_for_group) %>%
    summarise_all(funs(!!column_br := (n() - sum(.))/n()))
  
  # join 'br_table' to the table with bining variables
  bin_data <- left_join(bin_data, br_table,by=names(bin_data)[i])
}

rm(var_for_group)
rm(column_br)
rm(br_table)

############### Create PDF-file with statistics for all bining variables

Total<-length(bin_data$target_for_calc)
Good<-sum(bin_data$target_for_calc)
Bad<-Total-Good

pdf("Statistic_of_variables.pdf",width = 12, height=9, paper='special')

# table with IV values(descending order)
ggtexttable(iv_table, rows = NULL, theme = ttheme("lGreenWhite"))

# loop, that creates statistics for every column 
for (j in variable_fc_bin:bin_ncol) {
  
  plot1_hist <- ggplot(bin_data, aes(bin_data[,j])) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    scale_y_continuous(labels=scales::percent)+ 
    geom_text(aes( y = ((..count..)/sum(..count..)),
                   label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
    theme(axis.text.x = element_text(angle=10, vjust=0.9),
          plot.margin = unit(c(1,1,1,1), "cm") ) + 
    labs( y = "Class", x = "")
  
  plot2_BR_line <- ggplot(bin_data, aes(x=bin_data[,j],y=bin_data[,j-variable_fc_bin+bin_ncol+1],group=1)) + 
    geom_line(color="forestgreen",size=1)+
    geom_point() +
    theme(axis.text.x = element_text(angle=10, vjust=0.9),
          plot.margin = unit(c(1,1,1,1), "cm") ) + 
    scale_y_continuous(limits=c(0, 0.3),breaks=c(0.05,0.1,0.15, 0.2, 0.25, 0.3), 
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
  table <- ggtexttable(aggregate_table, rows = NULL, theme = ttheme("lGreenWhite"))

  # set name of variable and her 'Strength'(dependense of IV: 'Strong', Weak, 'Very weak' and etc)
  text1 <- paste0("
              ",names(bin_data)[j],": ", iv_table$Strength[iv_table$variables == names(bin_data)[j]])
  
  # set style of 'text1'
  title1 <- ggparagraph(text = text1, face = "italic", size = 25, color = "black")
  
  
  text2 <- paste0("                  ","IV ="
                  ,round(iv_table$IV[iv_table$variables == names(bin_data)[j]],4), sep = " ")
  title2 <- ggparagraph(text = text2, face = "italic", size = 20, color = "black")
  
  
  text3 <- paste0("                  ","Chisq.test = "
                  ,chisq, "; p_value = ", p_value_chisq, sep = "  ")
  title3 <- ggparagraph(text = text3, face = "italic", size = 20, color = "black")
  
  
  # union 4 object in one file: 
  print(ggarrange(title1, title2, title3, g, table , 
                  ncol = 1, nrow = 5,heights = c(0.08, 0.03, 0.03, 0.3, 0.2)))
  
}

dev.off()


## Remove unused columns

bin_data$Age_of_passport_month_km_4 <- NULL


## Correlation
# Calc_WOE

# select only bining data (remove column with 'BR') and sorting columns by IV

target_calc_bin <- match("target_for_calc",names(bin_data))
variable_fc_bin <- target_calc_bin + 1
bin_ncol <- ncol(bin_data)

# IV - statistic table
## IV - statistic table
iv_table <- arrange(cbind.data.frame(variables = names(bin_data[variable_fc_bin:bin_ncol])
                                     ,IV = sapply(bin_data[variable_fc_bin:bin_ncol], function(x) round(IV(X=x, Y=bin_data$target_for_calc)[1],4)), row.names = NULL),
                    desc(IV))

# Add strength of variables
iv_table$Strength <- ifelse(iv_table$IV>=1, "Suspicious",
                            ifelse(iv_table$IV>=.5, "Very strong",
                                   ifelse(iv_table$IV>=.2, "Strong",
                                          ifelse(iv_table$IV>=.1, "Average",
                                                 ifelse(iv_table$IV>=.02, "Weak", "Wery weak")))))

data_woe <- select(bin_data, c(target_for_calc, 
                               match(iv_table$variables, names(bin_data)))) %>%
            select(-grep("BR", names(bin_data)))
   
# create 'woe_table'. It consists of 2 column("WOE" + name_of_variables, WOE_value)
for (i in 2:ncol(data_woe)){
  var_for_group <- names(data_woe)[i]
  column_woe <- paste("WOE", names(data_woe)[i] , sep="_")
  woe_table <- data_woe %>%
    select(c(i,1)) %>%
    group_by_(.dots = var_for_group) %>%
    summarise_all(funs(!!column_woe := log((sum(.)/Good)/((length(.)-sum(.))/Bad))))
   
  # join 'woe_table' to the table with bining variables                      
  data_woe <- left_join(data_woe,woe_table,by=names(data_woe)[i])
} 

# Create file for Corelation:

# select onle 'WOE' columns
data_cor <- select(data_woe, grep("WOE", names(data_woe))) 

# calc Correlation
cor_table <- cor( data_cor[!is.infinite(rowSums(data_cor)),])

#build Correlation plot in pdf
pdf("CorrPlot111.pdf",width = 25,height=25,paper='special')
corrplot(cor_table, method="number",type = "upper",tl.col = "black",diag = FALSE)
dev.off()

## Create file for log_regresion: 

data_log_regr<- select(data_woe, c(1, grep("WOE", names(data_woe))))
################### Evaluating models(Gini, KS)
# install.packages("ROCR")
# install.packages("ggplot2")
# install.packages("MLmetrics")

library(ROCR)
library(ggplot2)
library(MLmetrics)

#recall column with "target",take universall name(target can be 'target_for_calc','target_for_calc_30' and so on) 
names(data_log_regr)[grep("target", names(data_log_regr))]<-"target"


#generate "data_log_regr" with first column "target" and another columns are sorted alphabetically
data_log_regr<-cbind(data_log_regr$target,(data_log_regr[ , sort(names(data_log_regr[,-which( colnames(data_log_regr)=="target")]))]))

#recall first column,cauth automatically programe calls it "data$target"
names(data_log_regr)[1]<-"target"

#data_log_regr <- read.csv2("WOE_CC_no_CSF_else.csv")

## create different type of models 
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
                Age_class, data_log_regr, family = "binomial")

model2 <- glm(target ~ Gender_x_Marital +
                Month_passport +
                Industry12 +
                Position12 +
                Education +
                Work_Experience +
                credit_paym +
                income_add +
                phone_in_bureau, data_log_regr, family = "binomial")

model3 <- glm(target ~ Gender_x_Marital +
                Month_passport +
                Industry12 +
                Position12 +
                Education +
                Work_Experience +
                credit_paym +
                income_add +
                Gender +
                Age_class, data_log_regr, family = "binomial")

############# KS

## Preparation {

#add column with predicted values
data_log_regr$target_predict_1<-predict(object = model1, type = "response")
data_log_regr$target_predict_2<-predict(object = model2, type = "response")
data_log_regr$target_predict_3<-predict(object = model3, type = "response")

#add values for perfomance plots(only for )
pred1 <- prediction(data_log_regr$target_predict_1,data_log_regr$target)
pred2 <- prediction(data_log_regr$target_predict_1,data_log_regr$target)
pred3 <- prediction(data_log_regr$target_predict_1,data_log_regr$target)

perf1 <- performance(pred1,"tpr","fpr")
ks = max(attr(perf1,'y.values')[[1]]-attr(perf1,'x.values')[[1]])

#Plot with KS

#find probability, where KC is located
x_value_ks <- perf1@alpha.values[[1]][which((attr(perf1,'y.values')[[1]]-attr(perf1,'x.values')[[1]]) == ks)]

#point for plot(Good Rate)
cdf1 <- ecdf(subset(data_log_regr$target_predict_1, data_log_regr$target == 1))
#point for plot(Bad Rate)
cdf2 <- ecdf(subset(data_log_regr$target_predict_1, data_log_regr$target == 0))

y_point <- cdf1(x_value_ks) 
y2_point <- cdf2(x_value_ks) 
# }

# KS plot 
KS_plot <- ggplot(data_log_regr, aes(x = target_predict_1, group = target ,colour = target))+
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

#######ROC Curve

#gini = auc*2 - 1
gini  <- (slot(performance(pred1, measure = "auc"),"y.values")[[1]])*2 - 1

#"tpr","fpr" - True positive rate(TP/(P)), false positive rate(FP/N)

#gini plot (set all together)
  plot(performance(pred1,"tpr","fpr"),lwd=2)
  lines(c(0,1),c(0,1))
  text(0.6,0.2,paste("Gini=", round(gini,4), sep=""), cex=1.4)
  title("ROC Curve")

# build plot with 3 roc curve 

gini1  <- (slot(performance(pred1, measure = "auc"),"y.values")[[1]])*2 - 1
gini2  <- (slot(performance(pred2, measure = "auc"),"y.values")[[1]])*2 - 1
gini3  <- (slot(performance(pred3, measure = "auc"),"y.values")[[1]])*2 - 1
    
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


















