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

install.packages("tree")
# Load data
work_data <- read.table("https://raw.githubusercontent.com/Srisai85/GermanCredit/master/german.data", h=F, sep="")

# work_data <- work_data2[,c(1:7, 19, 20, 21, 11)]

### Check missing data
# [%,#] missing
options(scipen = 99)
kable(data.frame(obs = nrow(work_data),
                 qty_NA = colSums(sapply(work_data, is.na)),
                 prop_NA = colSums(sapply(work_data, is.na))/nrow(work_data)), digits = 3)


work_data %>%
  missing_plot()

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

## Type of house
# density of na 
ggplot(work_data[is.na(work_data[,16]),], aes(x=target)) + 
  geom_density(aes(group=target, colour=target, fill=target), alpha=0.3) +
  labs(x = "Type of house") +
  theme(legend.position ="none")

# density w/o na
ggplot(work_data[is.na(work_data[,16])==F,], aes(x=target)) + 
  geom_density(aes(group=target, colour=target, fill=target), alpha=0.3) +
  labs(x = "Type of house") +
  theme(legend.position ="none")  


## Number of cards
# density of na 
ggplot(work_data[is.na(work_data[,17]),], aes(x=target)) + 
  geom_density(aes(group=target, colour=target, fill=target), alpha=0.3) +
  labs(x = "Type of house") +
  theme(legend.position ="none")

# density w/o na
ggplot(work_data[is.na(work_data[,17])==F,], aes(x=target)) + 
  geom_density(aes(group=target, colour=target, fill=target), alpha=0.3) +
  labs(x = "Number of cards") +
  theme(legend.position ="none")  



## Type of job
# density of na 
ggplot(work_data[is.na(work_data[,18]),], aes(x=target)) + 
  geom_density(aes(group=target, colour=target, fill=target), alpha=0.3) +
  labs(x = "Type of house") +
  theme(legend.position ="none")

# density w/o na
ggplot(work_data[is.na(work_data[,18])==F,], aes(x=target)) + 
  geom_density(aes(group=target, colour=target, fill=target), alpha=0.3) +
  labs(x = "Type of job") +
  theme(legend.position ="none")  


## chisq.test
j <- 17
agg_table <- data.frame(true = table(work_data[is.na(work_data[,16])==F, 1]),
                        na = table(work_data[is.na(work_data[,16]), 1]))


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


#### Missing value imputation
ini <- mice(work_data[, -1], maxit=0, print=F)
meth<- ini$meth

pred <- ini$pred
pas.imp <- mice(work_data[, -1], meth=meth, pred=pred, maxit=10, seed=123, print=F)
completedData <- complete(pas.imp)
work_data <- data.frame(target = work_data[,1], completedData)
work_data_if_smth_go_wrong <- work_data 

################ Create binning data 

# initial number of columns
begin_ncol <- ncol(work_data)

#initiate target column and first column with variables
target_ncol <- match("target",names(work_data))

variable_fc <- 2 #put number of first variable column

target_calc <- match("target",names(work_data))


# binning every variable with different type of binning('equal', 'kmeans', 'smbinning', '')
bin_col <- match(names(work_data[,(variable_fc):ncol(work_data)])[(as.vector(sapply(work_data[,(variable_fc):ncol(work_data)], 
                                                                                    function(x) is.numeric(x))))], names(work_data))

digit<- c()
digit[begin_ncol]<-NA

#Enter precision using next loop or directly like digit[97]<-100 digit[c(3:5)]<-1 digit[135]<-0.01 etc


save(digit, file = "digit.rda")
load(file = 'digit.rda')

error_list <- c()  

work_data <- work_data_if_smth_go_wrong
#loop with binning and creating cat vatiables in one
for (i in bin_col) 
{ 
  print(i)
  print(names(work_data[i]), quote = FALSE)
  # equal 
  # Idea: equal number of observation in every class
  print('calculating equal_depth method...', quote = FALSE)
  # create intervals
  eq_d<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], 5, style = 'quantile')
  
  # column name for new binning column, that bins with 'equal' method
  colname <- paste(names(work_data)[i], "cat_Eq_depth ", sep="_")
  
  # set column, that bins with 'equal' method
  work_data[[colname]] <- with(work_data, cut(work_data[,i], 
                                              c(min(work_data[,i])-1,unique(eq_d$brks)),include.lowest = TRUE, 
                                              right = FALSE, ordered = TRUE,dig.lab = 10))
  
  # for saving ordered factors all repleacements must be done on factor levels, not on work_data(!!!)
  levels(work_data[[colname]])<-gsub(",", ";", levels(work_data[[colname]]))
  
  print('calculating equal_width method...', quote = FALSE)
  # create intervals
  eq_w<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], 5, style = 'equal')
  
  # column name for new binning column, that bins with 'equal' method
  colname <- paste(names(work_data)[i], "cat_eq_width", sep="_")
  
  # set column, that bins with 'equal' method
  work_data[[colname]] <- with(work_data, cut(work_data[,i], 
                                              c(min(work_data[,i])-1,unique(eq_w$brks)),include.lowest = TRUE, 
                                              right = FALSE, ordered = TRUE,dig.lab = 10))
  
  # for saving ordered factors all repleacements must be done on factor levels, not on work_data(!!!)
  levels(work_data[[colname]])<-gsub(",", ";", levels(work_data[[colname]]))
  
  print('calculating jenks method...', quote = FALSE)
  # create intervals
  jk<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], 5, style = 'jenks')
  
  # column name for new binning column, that bins with 'equal' method
  colname <- paste(names(work_data)[i], "cat_jenks", sep="_")
  
  # set column, that bins with 'equal' method
  work_data[[colname]] <- with(work_data, cut(work_data[,i], 
                                              c(min(work_data[,i])-1,unique(jk$brks)),include.lowest = TRUE, 
                                              right = FALSE, ordered = TRUE,dig.lab = 10))
  
  # for saving ordered factors all repleacements must be done on factor levels, not on work_data(!!!)
  levels(work_data[[colname]])<-gsub(",", ";", levels(work_data[[colname]]))
  
  # kmeans 
  # Idea: search k 'center' points, that have biggest spreading of points around this 'center'(every interval have his own 'center' point)
  print('calculating kmeans method...', quote = FALSE)
  # k in {6,5,4,3,2}
  interval_count<-6
  repeat{
    km<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], interval_count, style = 'kmeans')
    colname <- paste(names(work_data)[i], "cat_km", interval_count, sep="_")
    work_data[[colname]] <- with(work_data, cut(work_data[,i], km$brks,include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
    interval_count<-interval_count-1
    
    # any class have to be bigger than 3% of all work_data
    #if (min(table(work_data[[colname]])/(nrow(work_data)/100)>3)) {break}
    
    # interval_count - {6,5,4,3,2}, if interval_count<2 - end
    if (interval_count<2) {break}
  }
  
  # hclust 
  # Idea: search k 'center' points, that have biggest spreading of points around this 'center'(every interval have his own 'center' point)
  print('calculating hclust method...', quote = FALSE)
  # k in {6,5,4,3,2}
  interval_count<-6
  repeat{
    hcl<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], interval_count, style = 'hclust')
    colname <- paste(names(work_data)[i], "cat_hcl", interval_count,  sep="_")
    work_data[[colname]] <- with(work_data, cut(work_data[,i], hcl$brks,include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
    interval_count<-interval_count-1
    
    # any class have to be bigger than 3% of all work_data
    #if (min(table(work_data[[colname]])/(nrow(work_data)/100)>3)) {break}
    
    # interval_count - {6,5,4,3,2}, if interval_count<2 - end
    if (interval_count<2) {break}
  }
  
  # smbinning
  print('calculating smbinning method...', quote = FALSE)
  # Idea: 'optimal binning' (maximization IV)
  i <- 3
  sb_data<-work_data[c(target_calc,i)]
  sb_data[2]<-ceiling(sb_data[2]/digit[i])*digit[i]
  sb<-try(smbinning(sb_data,y=names(work_data)[target_calc],x=names(work_data)[i]), FALSE)
  if (length(sb) > 1) ##check this condition !!!!
  {colname <- paste(names(work_data)[i], "cat_sb", sep="_")
  work_data[[colname]] <- with(work_data, cut(work_data[,i], c(min(work_data[,i])-1,unique(sb$bands)),
                                              right = TRUE, left = FALSE, ordered = TRUE,dig.lab = 10))
  levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
  }
  if (length(grep('Error', sb)) > 0 )
  {
    error_list <- c(error_list, i)
  }
  # save binnings intervals into 'rda' files
  save(eq_w, eq_d, jk, km, sb, file = paste(names(work_data[i]),".rda", sep = ""))
  
  rm(eq_w)
  rm(eq_d)
  rm(jk)
  rm(km)
  rm(sb)
  rm(interval_count)
  rm(colname) 
}

## IV

work_data$duration_month_2 <- NULL
work_data$credit_amount_5 <- NULL
work_data$age_in_yrs_13 <- NULL

bin_data <- work_data
variable_fc_bin <- which( colnames(bin_data)=="target" ) + 1
bin_ncol <- ncol(bin_data)
## All variables should have 'factor' type, so convert variables, that not is a 'factor', to 'factor' 
# select variables that shouldn`t conver to the 'fator'
factor_vars <- c(names(bin_data[,-1])[1:(variable_fc_bin-1)]
                 ,names(which(sapply(bin_data[,variable_fc_bin:ncol(bin_data)], is.factor))))

work_data$duration_month_2 <- NULL
work_data$credit_amount_5 <- NULL
work_data$age_in_yrs_13 <- NULL

bin_data <- work_data
variable_fc_bin <- which( colnames(bin_data)=="target" ) + 1
bin_ncol <- ncol(bin_data)
## All variables should have 'factor' type, so convert variables, that not is a 'factor', to 'factor' 
# select variables that shouldn`t conver to the 'fator'
factor_vars <- c(names(bin_data[,-1])[1:(variable_fc_bin-1)]
                 ,names(which(sapply(bin_data[,variable_fc_bin:ncol(bin_data)], is.factor))))


## BR analysis
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

target_calc_bin <- 1
variable_fc_bin <- 2
bin_ncol <- 21
k <- 1
i <- 1

Total<-length(bin_data$target_for_calc)
Good<-sum(bin_data$target_for_calc)
Bad<-Total-Good

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

## select variable
duration_month <- iv_table[grepl("duration_month", iv_table$variables),c(2,3)]
credit_amount <- iv_table[grepl("credit_amount", iv_table$variables),c(2,3)]
age_in_yrs <- iv_table[grepl("age_in_yrs", iv_table$variables),c(2,3)]

table1 <- ggtexttable(duration_month, rows = NULL, theme = ttheme(base_style ="lRedWhite", base_size = 10))

tab1 <- table_cell_font(table1, row = 2, column = 2, face = "bold")
tab1 <- table_cell_font(tab1, row = 5, column = 2, face = "bold")

table2 <- ggtexttable(credit_amount, rows = NULL, theme = ttheme(base_style ="lRedWhite", base_size = 10))

tab2 <- table_cell_font(table2, row = 2, column = 2, face = "bold")
tab2 <- table_cell_font(tab2, row = 8, column = 2, face = "bold")


table3 <- ggtexttable(age_in_yrs, rows = NULL, theme = ttheme(base_style ="lRedWhite", base_size = 10))

tab3 <- table_cell_font(table3, row = 2, column = 2, face = "bold")
tab3 <- table_cell_font(tab3, row = 3, column = 2, face = "bold")

### Modeling

## Stratified Random Sampling(test, train)
div_part_1 <- createDataPartition(y = work_data$target, p = 0.7, list = F)

# Training Sample
train <- work_data[div_part_1,] # 70%

# Test Sample
test <- work_data[-div_part_1,] # 30% 
table(test$target)

#### Log regr
mod1 <- glm(target~.,data=train,family=binomial())
# m1 <- step(m1)
# summary(m1)

#score test data set
test$mod1_score <- predict(mod1,type='response',test)
mod1_pred <- prediction(test$mod1_score, test$target)
mod1_perf <- performance(mod1_pred,"tpr","fpr")

ev_df_mod1 <- data.frame(Gini = round(((slot(performance(mod1_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(mod1_pred, measure = "auc")@y.values[[1]]*100, 2))
ev_df_mod1

ev_df_mod1 <- ggtexttable(ev_df_mod1, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

Gini1 <- round(((slot(performance(m1_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2)
gini_plot <- ggplot(setNames(data.frame(mod1_perf@x.values, mod1_perf@y.values), c('x_val', 'y_val')), 
                    aes(x = x_val, y = y_val), color=sort_criterion) + 
  ggtitle(paste("Gini=", round(Gini1,4), sep="")) +
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  ggtitle(paste("Gini=", round(Gini1,4), sep="")) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")

### LASSO
work_data_num <- read.csv2("https://raw.githubusercontent.com/Sokolheavy/R_Scoring/master/Diploma/german-numeric.csv")
work_data_num[,25] <- ifelse(work_data_num[,25]==1,1,0)
names(work_data_num) <- c('V1','V2','V3','V4','V5','V6','V7','V8','V9','V10','V11','V12','V13',
                          'V14','V15','V16','V17','V18','V19','V20','V21','V22','V23','V24','target')

set.seed(123)
div_part_1 <- createDataPartition(y = work_data_num$target, p = 0.7, list = F)
train <- work_data_num[div_part_1,] # 70% 
test <- work_data_num[-div_part_1,] # 30% 

mat1 <- model.matrix(target ~ . , data = train  ) # convert to numeric matrix
mat2 <- model.matrix(target ~ . , data = test  )  # convert to numeric matrix

mod2 <-cv.glmnet(mat1, as.numeric(train$target), alpha=1, family="binomial", type.measure = 'auc')


# Apply model to testing dataset
test$mod2_score <- predict(mod2, type="response", newx =mat2[,-25], s = 'lambda.min')
mod2_pred <- prediction(test$mod2_score, test$target)

# calculate probabilities for TPR/FPR for predictions
mod2_perf <- performance(m2_pred,"tpr","fpr")


ev_df_m2 <- data.frame(Gini = round(((slot(performance(mod2_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(mod2_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m2 <- ggtexttable(ev_df_m2, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

Gini2 <- round(((slot(performance(m3_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2)
gini_plot <- ggplot(setNames(data.frame(m3_perf@x.values, m3_perf@y.values), c('x_val', 'y_val')), 
                    aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  ggtitle(paste("Gini=", round(Gini2,4), sep="")) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")

png(filename="LASSO.png", res=150, width = 1000, height = 1000)
plot(mod2)
dev.off()

log(mod2$lambda.min)

coef(mod2, s=mod2$lambda.min)



### CART
mod3 <- rpart(target~.,data=train,method=	"class")

# score test data
test$mod3_score <- predict(mod3,type='prob',test)
mod3_pred <- prediction(test$mod3_score[,2],test$target)
mod3_perf <- performance(mod3_pred,"tpr","fpr")

ev_df_mod3 <- data.frame(Gini = round(((slot(performance(mod3_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(mod3_pred, measure = "auc")@y.values[[1]]*100, 2))
ev_df_mod3

ev_df_mod3 <- ggtexttable(ev_df_mod3, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

Gini3 <- round(((slot(performance(m3_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2)
mod3_gini_plot <- ggplot(setNames(data.frame(mod3_perf@x.values, mod3_perf@y.values), c('x_val', 'y_val')), 
                       aes(x = x_val, y = y_val), color=sort_criterion) + 
  ggtitle(paste("Gini=", round(Gini3,4), sep="")) +
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")


### C4.5
mod4 <- ctree(target~., train)

test$mod4_score <- predict(mod4, test[,-1])
mod4_pred <- prediction(test$mod4_score, test$target)
mod4_perf <- performance(mod4_pred,"tpr","fpr")

ev_df_mod4 <- data.frame(Gini = round(((slot(performance(mod4_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                         AUC = round(performance(mod4_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_mod4 <- ggtexttable(ev_df_mod4, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

mod4_gini_plot <- ggplot(setNames(data.frame(mod4_perf@x.values, mod4_perf@y.values), c('x_val', 'y_val')), 
                         aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")

### GB
work_data_num <- read.csv2("https://raw.githubusercontent.com/Sokolheavy/R_Scoring/master/Diploma/german-numeric.csv")
work_data_num[,25] <- ifelse(work_data_num[,25]==1,1,0)
names(work_data_num) <- c('V1','V2','V3','V4','V5','V6','V7','V8','V9','V10','V11','V12','V13',
                          'V14','V15','V16','V17','V18','V19','V20','V21','V22','V23','V24','target')

set.seed(123)
div_part_1 <- createDataPartition(y = work_data_num$target, p = 0.7, list = F)
train <- as.matrix(work_data_num[div_part_1,]) # 70% 
test <- work_data_num[-div_part_1,] # 30% 

mat1 <- model.matrix(target ~ . , data = train  ) 
mat2 <- model.matrix(target ~ . , data = test  )

xtr = mat1[,1:24]
ytr = mat1[,25] 
xte = mat2[,1:24]
yte = mat2[,25]


xgb = xgboost(data = xtr, 
              label = ytr, 
              eta = 0.0005,
              max_depth = 3, 
              gamma = 0,
              objective = "binary:logistic",
              nrounds = 1000, 
              subsample = 0.7,
              colsample_bytree = 0.7,
              seed = 1,
              eval_metric = "error")

#computing classification rate in training set
y_pred = predict(xgb, xtr)
y_train = rep(0,length(y_pred))
for(i in 1:length(y_pred)){
  if(y_pred[i]<=0.5){y_train[i]=0}
  else{y_train[i]=1}
}
test_result3 = table(y_train,ytr)
acc_train = (test_result3[1,1]+test_result3[2,2])/sum(test_result3)
gini_train <- 2*acc_train - 1
gini_train

#computing classification rate in testing set
y_pred = predict(xgb, xte)
y_test = rep(0,length(y_pred))
for(i in 1:length(y_pred)){
  if(y_pred[i]<=0.5){y_test[i]=0}
  else{y_test[i]=1}
}
test_result4 = table(y_test,yte)
acc_test = (test_result4[1,1]+test_result4[2,2])/sum(test_result4)
gini_test <- 2*acc_test - 1
gini_test
gini_train



par(mar=c(1,1,1,1))
# Training & test error plot
e <- data.frame(xgb$evaluation_log)

plot(e$iter, e$train_mlogloss, col = 'blue')
lines(e$iter, e$test_mlogloss, col = 'red')

