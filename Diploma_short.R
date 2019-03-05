# https://www.datacamp.com/projects/tech:r?utm_source=adwords_ppc&utm_campaignid=1457170719&utm_adgroupid=59514459274&utm_device=c&utm_keyword=ggplot2&utm_matchtype=p&utm_network=g&utm_adpostion=1t1&utm_creative=278660198064&utm_targetid=kwd-301408186532&utm_loc_interest_ms=&utm_loc_physical_ms=9061015&gclid=EAIaIQobChMIm5PGz_vq4AIVVamaCh1yxgFAEAAYASACEgIiDfD_BwE
# detach(package:neuralnet)
library(DT)          # For Data Tables
library(dplyr) 
library(lattice)     # The lattice add-on of Trellis graphics
library(knitr) 
library(ggplot)    
library(ggplot2)     
library(ClustOfVar)  # Clustering of variables 
library(ape)         # Analyses of Phylogenetics and Evolution (as.phylo) 
library(Information) # Data Exploration with Information Theory (Weight-of-Evidence and Information Value)
library(ROCR)        # Model Performance and ROC curve
library(caret)       # Classification and Regression Training -  for any machine learning algorithms
library(rpart)       # Recursive partitioning for classification, regression and survival trees
library(rpart.utils) # Tools for parsing and manipulating rpart objects, including generating machine readable rules
library(rpart.plot)  # Plot 'rpart' Models: An Enhanced Version of 'plot.rpart'
library(randomForest)# Leo Breiman and Cutler's Random Forests for Classification and Regression 
library(party)       # A computational toolbox for recursive partitioning - Conditional inference Trees
library(bnlearn)     # Bayesian Network Structure Learning, Parameter Learning and Inference
library(DAAG)        # Data Analysis and Graphics Data and Functions
library(vcd)         # Visualizing Categorical Data
library(kernlab)     # Support Vector Machine
library(neuralnet)   # Neural Network 
library(lars)        # For Least Angle Regression, Lasso and Forward Stagewise
library(glmnet)      # Lasso and Elastic-Net Regularized Generalized Linear Models
library(classInt)    # Clustering data for bining  
library(smbinning)   # type of bining
library(InformationValue)
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

# Read a numeric copy: Numeric data for Neural network & Lasso

cdatanum<-read.table("https://raw.githubusercontent.com/Srisai85/GermanCredit/master/german.data", h=F, sep="") 
cdatanum <- as.data.frame(sapply(cdatanum, as.numeric ))

kable(as.data.frame(colnames(work_data)))
DT::datatable(work_data[1:100,])
glimpse(work_data)


work_data$duration_month_2  <- as.numeric(work_data$duration_month_2)
work_data$credit_amount_5   <-  as.numeric(work_data$credit_amount_5 )
work_data$instalment_pct_8 <-  as.numeric(work_data$instalment_pct_8)
work_data$present_residence_since_11 <-  as.numeric(work_data$present_residence_since_11)
work_data$age_in_yrs_13        <-  as.numeric(work_data$age_in_yrs_13)
work_data$number_cards_this_bank_16    <-  as.numeric(work_data$number_cards_this_bank_16)
work_data$no_people_liable_for_mntnance_18 <-  as.numeric(work_data$no_people_liable_for_mntnance_18)
work_data <- data.frame(target = ifelse(work_data$good_bad_21 == 1, 1,0), work_data)
work_data$good_bad_21 <- NULL  



# Create some groups from contious variables
work_data$duration_month_2 <-as.factor(ifelse(work_data$duration_month_2<=6,'00-06',
                                              ifelse(work_data$duration_month_2<=12,'06-12',
                                                     ifelse(work_data$duration_month_2<=24,'12-24', 
                                                            ifelse(work_data$duration_month_2<=30,'24-30',
                                                                   ifelse(work_data$duration_month_2<=36,'30-36',
                                                                          ifelse(work_data$duration_month_2<=42,'36-42','42+')))))))

work_data$credit_history_3<-as.factor(ifelse(work_data$credit_history_3 == "A30", "01.A30",
                                             ifelse(work_data$credit_history_3 == "A31","02.A31",
                                                    ifelse(work_data$credit_history_3 == "A32","03.A32.A33",
                                                           ifelse(work_data$credit_history_3 == "A33","03.A32.A33", "04.A34")))))

work_data$credit_amount_5<-as.factor(ifelse(work_data$credit_amount_5<=1400,'0-1400',
                                            ifelse(work_data$credit_amount_5<=2500,'1400-2500',
                                                   ifelse(work_data$credit_amount_5<=3500,'2500-3500', 
                                                          ifelse(work_data$credit_amount_5<=4500,'3500-4500',
                                                                 ifelse(work_data$credit_amount_5<=5500,'4500-5500','5500+'))))))


work_data$age_in_yrs_13 <- as.factor(ifelse(work_data$age_in_yrs_13<=25, '0-25',
                                            ifelse(work_data$age_in_yrs_13<=30, '25-30',
                                                   ifelse(work_data$age_in_yrs_13<=35, '30-35', 
                                                          ifelse(work_data$age_in_yrs_13<=40, '35-40', 
                                                                 ifelse(work_data$age_in_yrs_13<=45, '40-45', 
                                                                        ifelse(work_data$age_in_yrs_13<=50, '45-50',
                                                                               ifelse(work_data$age_in_yrs_13<=60, '50-60', '60+'))))))))


bin_data <- work_data
variable_fc_bin <- which( colnames(bin_data)=="target" ) + 1
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




## Create loop for BR (add columns with BR for every binning variable)
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

rm(var_for_group)
rm(column_br)
rm(br_table)

save(bin_data, file = "bin_data_before_pdf.rda")

############### Create HTML-file with statistics for all bining variables
#install.package("R2HTML")
library("R2HTML")

# set name of folder
folder_name <- "html_plots_6"

# create folder
dir.create(folder_name)

initial_path <- getwd()
setwd(paste0(getwd(), "/", folder_name))

Total<-length(bin_data$target_for_calc)
Good<-sum(bin_data$target_for_calc)
Bad<-Total-Good

i <- 1
k <- 1

# table with IV values(descending order)
for (j in seq(1,(bin_ncol-variable_fc_bin), 100)){
  
  png(file = paste0(i,".png"),width = 1000, height=1100)
  if ((nrow(iv_table)-j)>50)
  {
    table1 <-  ggtexttable(iv_table[j:min((j+49),nrow(iv_table)),], rows = NULL, theme = ttheme("lBlueWhite"))
    table2 <-  ggtexttable(iv_table[(j+50):min((j+99),nrow(iv_table)),], rows = NULL, theme = ttheme("lBlueWhite"))
    print(ggarrange(table1, table2, 
                    ncol = 2, nrow = 1, heights = c(0.1, 0.1)))
  }
  else
  {
    table1 <-  ggtexttable(iv_table[j:min((j+49),nrow(iv_table)),], rows = NULL, theme = ttheme("lBlueWhite"))
    print(ggarrange(table1, 
                    ncol = 1, nrow = 1, heights = c(0.1, 0.1))) 
  }
  i<-i+1
  dev.off() 
}

rm(table1)
rm(table2)

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
    scale_y_continuous(labels=scales::percent) + 
    labs( y = "BR", x = "")
  
  
  g1<-ggplot_gtable(ggplot_build(plot1_hist))
  g2<-ggplot_gtable(ggplot_build(plot2_BR_line))
  
  pp<-c(subset(g1$layout,name=="panel",se=t:r))
  g<-gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]],pp$t,pp$l,pp$b,pp$l)
  
  ia<-which(g2$layout$name=="axis-l")
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
                       bad = (n() - sum(.)),
                       bad = n())) %>%
    select(-1) %>% 
    t() 
  
  # set chisq.test value and p_value
  chisq <- round(as.data.frame(chisq.test(chisq_table)[1])[1,1], 2)
  p_value_chisq <- round(as.data.frame(chisq.test(chisq_table)[3])[1,1], 4)
  
  # Data visualization in pdf
  # value from 'aggregate_table' sets in the ggplot object
  table <- ggtexttable(aggregate_table, rows = NULL, theme = ttheme("lRedWhite"))
  
  # set name of variable and her 'Strength'(dependense of IV: 'Strong', Weak, 'Very weak' and etc)
  text1 <- paste0("
                  ",k,". ",names(bin_data)[j],": ", iv_table$Strength[iv_table$variables == names(bin_data)[j]])
  
  
  # set style of 'text1'
  title1 <- ggparagraph(text = text1, face = "italic", size = 25, color = "black")
  
  
  text2 <- paste0("                  ","IV ="
                  ,round(iv_table$IV[iv_table$variables == names(bin_data)[j]],4), sep = " ")
  title2 <- ggparagraph(text = text2, face = "italic", size = 20, color = "black")
  
  
  text3 <- paste0("                  ","Chisq.test = "
                  ,chisq, "; p_value = ", p_value_chisq, sep = "  ")
  title3 <- ggparagraph(text = text3, face = "italic", size = 20, color = "black")
  
  print(paste0(k,". ", names(bin_data)[j]))
  k <- k+1
  png(file = paste0(i,".png"),width = 1200, height=1200)
  i <- i+1
  # union 4 object in one file: 
  print(ggarrange(title1, title2, title3, g, table , 
                  ncol = 1, nrow = 5,heights = c(0.1, 0.04, 0.04, 0.3, 0.2)))
  
  dev.off() 
  
}

rm(aggregate_table)
rm(ax)
rm(chisq)
rm(chisq_table)
rm(g)
rm(g1)
rm(g2)
rm(ga)
rm(ia)
rm(j)
rm(k)
rm(p_value_chisq)
rm(table)
rm(text1)
rm(text2)
rm(text3)
rm(title1)
rm(title2)
rm(title3)
rm(var_for_group)
rm(height)

rm(pp)
rm(plot1_hist)
rm(plot2_BR_line)

# create HTML file

setwd(initial_path)
HTMLStart(folder_name)

for (j in 1:i){
  name <- paste0(j,".png")
  HTMLInsertGraph(GraphFileName = name)
}

HTMLStop()

rm(i)
rm(j)
rm(folder_name)
rm(name)


########select variables####################

keep<- c(1:9,13,14)
cdata_2 <- bin_data[,keep]
str(cdata_2)


#select training sample 
set.seed(123)
train<-cdata_2[sort(sample(nrow(cdata_2), nrow(cdata_2)*.7)),] # 70% here
test<-cdata_2[-sort(sample(nrow(cdata_2), nrow(cdata_2)*.7)),] # rest of the 30% data goes here

table(train$target)
table(test$target)
table(cdata_2$target)


table(train$target)[2]/table(train$target)[1]
table(test$target)[2]/table(test$target)[1]
table(cdata_2$target)[2]/table(cdata_2$target)[1]

# target name:
measurevar <- "target"
data_log_regr <- train

iv_table[sapply(iv_table[,2], 
                function(x) x %in% names(data_log_regr)), ]

i <- 1
# data_log_regr <- data_log_regr[1:500,]
# table(data_log_regr[,1])
################## create pdf file 
pdf("Evaluation_of_models1.pdf",width = 7, height=14, pointsize = 12, paper='special')
options(warn = -1) 

# loop, that creates 3 graphics(Gini, KS, Statistical graphic) 
for (i in 1:nm) {
  
  # This returns the model:
  model <- glm(as.formula(paste(measurevar,paste(groupvars[[i]], collapse=" + "), sep=" ~ "))
               ,data_log_regr, family = "binomial")
  

  pred_target <- predict(object = model, type = "response", data_log_regr)

  # add values for perfomance plots(only for calc)
  pred <- prediction(pred_target , data_log_regr$target)
  
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
  Gini  <- (slot(performance(pred, measure = "auc"),"y.values")[[1]])*2 - 1
  
  gini_plot <- ggplot(setNames(data.frame(perf@x.values, perf@y.values), c('x_val', 'y_val')), 
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
  
  stat_table <- data.frame(Measure = c("Accuracy", "Precision", "Recall","F1","Optimal_cutoff"),
                           Formula = c("(TP+TN)/(TP+FP+TN+FN)", "TP/(TP+FP)", "TP/(TP+FN)", "2*P*R/(P+R)",""),
                           Value=round(c((cM[1,1]+cM[2,2])/sum(cM), # Accuracy
                                         cM[2,2]/(cM[2,2]+cM[1,2]), # Precision
                                         cM[2,2]/(cM[2,2]+cM[2,1]), # Recall
                                         2*(cM[2,2]/(cM[2,2]+cM[1,2])) * (cM[2,2]/(cM[2,2]+cM[2,1]))/
                                           (cM[2,2]/(cM[2,2]+cM[1,2]) + (cM[2,2]/(cM[2,2]+cM[2,1]))), #F1
                                         opt_cut[3,1]),4)) 
  
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
    
    model <- glm(as.formula(paste(names(train)[1],paste(groupvars[[i]], collapse=" + "), sep=" ~ "))
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

########## C4.5

install.packages("C50")
library(C50)

# fit model
model2 <- C5.0(x = train[,-1] , y = as.factor(train[,1]))

# make predictions
pred_target <- predict(model2, type = "prob", train)

# add values for perfomance plots(only for calc)
pred <- prediction(pred_target[,2], as.factor(train$target))

perf <- performance(pred,"tpr","fpr") 

##### Plot with KS

ks = max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])



##### ROC Curve

#gini = auc*2 - 1
Gini  <- (slot(performance(pred, measure = "auc"),"y.values")[[1]])*2 - 1











########## SVM
# Basic Model
m7_1 <- ksvm(target ~ ., data = train, kernel = "vanilladot")

m7_1_pred <- predict(m7_1, test[,2:11], type="response")

# Compute at the prediction scores
m7_1_score <- predict(m7_1,test, type="decision")
m7_1_pred <- prediction(m7_1_score, as.integer(test$target))

# Plot ROC curve
m7_1_perf <- performance(m7_1_pred, measure = "tpr", x.measure = "fpr")
plot(m7_1_perf, colorize=TRUE, lwd=2, main="m7_1 SVM:Plot ROC curve - Vanilladot")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
lines(x=c(1, 0), y=c(0, 1), col="green", lwd=1, lty=4)












