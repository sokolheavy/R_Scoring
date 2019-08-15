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
str(work_data)
### Check missing data
# [%,#] missing
options(scipen = 99)
kable(data.frame(obs = nrow(work_data),
                 qty_NA = colSums(sapply(work_data, is.na)),
                 prop_NA = colSums(sapply(work_data, is.na))/nrow(work_data)), digits = 3)

work_data %>%
  missing_plot()

ggplot(work_data, aes(housing_type)) +
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


ggplot(work_data, aes(number_cards_this_bank)) +
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
  geom_density(aes(group=target, colour=factor(target)), alpha=0.3) +
  scale_x_continuous(breaks= c(0, 1)) +
  labs(x = "Type of house with missing") +
  theme(legend.position ="none") 

# density w/o na
ggplot(work_data[is.na(work_data[,16])==F,], aes(x=target)) + 
  geom_density(aes(group=target, colour=factor(target)), alpha=0.3) +
  scale_x_continuous(breaks= c(0, 1)) +
  labs(x = "Type of house without missing") +
  theme(legend.position ="none") 


## Number of cards
# density of na 
ggplot(work_data[is.na(work_data[,17]),], aes(x=target)) + 
  geom_density(aes(group=target, colour=factor(target)), alpha=0.3) +
  scale_x_continuous(breaks= c(0, 1)) +
  labs(x = "Number of cards with missing") +
  theme(legend.position ="none")

# density w/o na
ggplot(work_data[is.na(work_data[,17])==F,], aes(x=target)) + 
  geom_density(aes(group=target, colour=factor(target)), alpha=0.3) +
  scale_x_continuous(breaks= c(0, 1)) +
  labs(x = "Number of cards without missing") +
  theme(legend.position ="none")  



## Type of job
# density of na 
ggplot(work_data[is.na(work_data[,18]),], aes(x=target)) + 
  geom_density(aes(group=target, colour=factor(target)), alpha=0.3) +
  scale_x_continuous(breaks= c(0, 1)) +
  labs(x = "Type of job with missing") +
  theme(legend.position ="none")

# density w/o na
ggplot(work_data[is.na(work_data[,18])==F,], aes(x=target)) + 
  geom_density(aes(group=target, colour=factor(target)), alpha=0.3) +
  scale_x_continuous(breaks= c(0, 1)) +
  labs(x = "Type of job without missing") +
  theme(legend.position ="none")  


## chisq.test
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

#### Missing value imputation
init <- mice(work_data[, -1], maxit=0, print=F)
methnew<- init$meth

pred <- init$pred
pas <- mice(work_data[, -1], meth=methnew, pred=pred, maxit=10, seed=123, print=F)
completedData <- complete(pas)
work_data <- data.frame(target = work_data[,1], completedData)
work_data_if_smth_go_wrong <- work_data 


#### Create binning data 
## data visualization before binning
ggplot(work_data, aes(x = duration_month)) +
  geom_density(adjust = .5, fill = "lightblue4")

ggplot(work_data, aes(x = credit_amount)) +
  geom_density(adjust = .5, fill = "slategray1") 

ggplot(work_data, aes(x = age_in_yrs)) +
  geom_density(adjust = .5, fill = "beige") 

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

# Enter precision using next loop or directly like digit[97]<-100 digit[c(3:5)]<-1 digit[135]<-0.01 etc
for (i in bin_col)
{ 
  if(is.na(digit[i]))
  {
    print(paste(i, ':', names(work_data[i])), quote = FALSE)
    print(paste('from', min(as.numeric(work_data[,i])), 'to', max(as.numeric(work_data[,i])), 'with', nrow(unique(work_data[i])), 'unique values in', nrow(work_data[i]), 'records'), quote = FALSE)
    digit[i] <- as.numeric(readline(prompt="Enter precision: "))
  }
}

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
  colname <- paste(names(work_data)[i], "cat_Eq_depth", sep="_")
  
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

work_data$duration_month <- NULL
work_data$credit_amount <- NULL
work_data$age_in_yrs <- NULL

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

setwd("F:/Дипломна робота_2/картинки/binning_br")
target_calc_bin <- 1
variable_fc_bin <- 2
bin_ncol <- 58
k <- 1
i <- 1

Total<-length(bin_data$target_for_calc)
Good<-sum(bin_data$target_for_calc)
Bad<-Total-Good

for (j in variable_fc_bin:bin_ncol) {
  
  j <- grep("age_in_yrs_cat_Eq_depth", names(work_data))
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
  
  table <- ggtexttable(aggregate_table, rows = NULL, theme = ttheme("lRedWhite"))
  
  # set name of variable and her 'Strength'(dependense of IV: 'Strong', Weak, 'Very weak' and etc)
  text1 <- paste0("
                  ",k,". ",names(bin_data)[j])
  
  
  # set style of 'text1'
  title1 <- ggparagraph(text = text1, face = "italic", size = 25, color = "black")
  
  # set name of variable and her 'Strength'(dependense of IV: 'Strong', Weak, 'Very weak' and etc)
  print(paste0(k,". ", names(bin_data)[j]))
  k <- k+1
  png(file = paste0(i,".png"),width = 1200, height=1200)
  i <- i+1
  # union 4 object in one file: 
  print(ggarrange(title1, g, table , 
                  ncol = 1, nrow = 3,heights = c(0.1, 0.3, 0.2)))
  
  dev.off() 
}


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


## select variable
unique(bin_data$age_in_yrs_cat_Eq_depth)

work_data[work_data$credit_amount_cat_eq_width==" [45;75]", 
          grep("credit_amount_cat_eq_width", names(work_data))] <- "[11154.4;14789.2)"

work_data[work_data$credit_amount_cat_eq_width=="[14789.2;18424]", 
          grep("age_in_yrs_cat_Eq_depth", names(work_data))] <- "[36;45)"


duration_month <- iv_table[grepl("duration_month", iv_table$variables)&
                             !grepl('hcl',iv_table$variables)&
                             !grepl('sb',iv_table$variables),c(2,3)]
credit_amount <- iv_table[grepl("credit_amount", iv_table$variables)&
                            !grepl('hcl',iv_table$variables)&
                            !grepl('sb',iv_table$variables),c(2,3)]
age_in_yrs <- iv_table[grepl("age_in_yrs", iv_table$variables)&
                         !grepl('hcl',iv_table$variables)&
                         !grepl('sb',iv_table$variables),c(2,3)]


table1 <- ggtexttable(duration_month, rows = NULL, theme = ttheme(base_style ="lRedWhite", base_size = 10))

tab1 <- table_cell_font(table1, row = 2, column = 2, face = "bold")


table2 <- ggtexttable(credit_amount, rows = NULL, theme = ttheme(base_style ="lRedWhite", base_size = 10))

tab2 <- table_cell_font(table2, row = 2, column = 2, face = "bold")



table3 <- ggtexttable(age_in_yrs, rows = NULL, theme = ttheme(base_style ="lRedWhite", base_size = 10))

tab3 <- table_cell_font(table3, row = 2, column = 2, face = "bold")

### Modeling
work_data <- work_data_if_smth_go_wrong
work_data_if_smth_go_wrong <- work_data
work_data <- work_data[ ,c(1,2,22,3,4,37,5,6,7,11,52)]
# write.csv2(work_data, 'work_data_for_modeling.csv')
work_data <- read.csv2("https://raw.githubusercontent.com/Sokolheavy/R_Scoring/master/Diploma/work_data_for_modeling.csv")

## Stratified Random Sampling(test, train)
set.seed(123)
div_part_1 <- createDataPartition(y = work_data$target, p = 0.7, list = F)

# Training Sample
train <- work_data[div_part_1,] # 70%

# Test Sample
test <- work_data[-div_part_1,] # 30% 
table(test$target)

work_data <- work_datan
#### Log regr
mod1 <- glm(target~.,data=train,family=binomial())
# m1 <- step(m1)
# summary(m1)

#score test data set
## test 
test$mod1_score <- predict(mod1,type='response',test)
mod1_pred <- prediction(test$mod1_score, test$target)
mod1_perf <- performance(mod1_pred,"tpr","fpr")

ev_df_mod1 <- data.frame(Gini = round(((slot(performance(mod1_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                         AUC = round(performance(mod1_pred, measure = "auc")@y.values[[1]]*100, 2))
ev_df_mod1

ev_df_mod1 <- ggtexttable(ev_df_mod1, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

acc1 <- round(performance(mod1_pred, measure = "auc")@y.values[[1]]*100, 2)
Gini1 <- round(((slot(performance(mod1_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2)
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
test$mod2_score <- predict(mod2, type='response', newx=mat2, s = 0.0008879864)
mod2_pred <- prediction(test$mod2_score, test$target)
mod2_perf <- performance(mod2_pred,"tpr","fpr")

ev_df_m2 <- data.frame(Gini = round(((slot(performance(mod2_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(mod2_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m2 <- ggtexttable(ev_df_m2, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

acc2 <- 63.09
Gini2 <- 81.54
gini_plot <- ggplot(setNames(data.frame(mod2_perf@x.values, mod2_perf@y.values), c('x_val', 'y_val')), 
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
set.seed(12)
mod3 <- rpart(target~.,data=train,method=	"class")

# score test data
test$mod3_score <- predict(mod3,type='prob',test)
mod3_pred <- prediction(test$mod3_score[,2],test$target)
mod3_perf <- performance(mod3_pred,"tpr","fpr")

ev_df_mod3 <- data.frame(Gini = round(((slot(performance(mod3_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                         AUC = round(performance(mod3_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_mod3 <- ggtexttable(ev_df_mod3, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

acc3 <- round(performance(mod3_pred, measure = "auc")@y.values[[1]]*100, 2)
Gini3 <- round(((slot(performance(mod3_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2)
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


### C5.0
set.seed(123)
mod4 <- ctree(target~., train)

test$mod4_score <- predict(mod4, test[,-1])
mod4_pred <- prediction(test$mod4_score, test$target)
mod4_perf <- performance(mod4_pred,"tpr","fpr")

ev_df_mod4 <- data.frame(Gini = round(((slot(performance(mod4_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                         AUC = round(performance(mod4_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_mod4 <- ggtexttable(ev_df_mod4, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

acc4 <- round(performance(mod4_pred, measure = "auc")@y.values[[1]]*100, 2)
Gini4 <- round(((slot(performance(mod4_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2)
mod4_gini_plot <- ggplot(setNames(data.frame(mod4_perf@x.values, mod4_perf@y.values), c('x_val', 'y_val')), 
                         aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  ggtitle(paste("Gini=", round(Gini4,4), sep="")) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")


### RF 
ntree <- c(100, 200)
mtry <- c(10,14)
nodesize < c(2,4)
k <- 10
n <- floor(nrow(german_credit)/k)
rf_accuracy_poss <- c()
rf_accuracy_all=data.frame("Trees" = integer(0),"Variables"=integer(0),
                           "Nodesize" = integer(0), "Acc"= numeric(0))

for (z in ntree){
  for (m in mtry){
    for (n in nodesize){
      for (i in 1:k){
        rf_fit<-randomForest(x=train[,-c(1)], y = as.factor(train[,c(1)]), 
                             ntree = z, mtry = m, nodesize = n)
        rf_pred <- predict(rf_fit, test, type = "prob")[,2]
        rf_pred_class <- ifelse(rf_pred>0.5, 1, 0)
        rf_accuracy_poss[i]<- 1 - sum(test$target!=rf_pred_class)/nrow(test)
      } 
      print(paste("Trees: ",z,"Variables: ", m, "Nodesize :", n,
                  "Cross-Validation mean Accuracy",mean(rf_accuracy_poss)))
      rf_accuracy_all<- rbind(rf_accuracy_all, data.frame(t,m,n,mean(rf_accuracy_poss)))
    }
  }
}



work_datan <- work_data_num
j <- 1
k<-10
n<-floor(nrow(work_datan)/k)
rf_acc<-c()
for (j in 1:k){
  d1 = ((j-1)*n+1)
  d2 = (j*n)
  dataset = d1:d2
  rf_tr<- work_datan[-dataset,]
  rf_te<- work_datan[dataset,]
  rf_fit<-randomForest(x=rf_tr[,-c(1)], y = as.factor(rf_tr[,c(1)]),
                       ntree = 200, mtry = 3, nodesize = 6)
  rf_pred <- predict(rf_fit, rf_te, type = "prob")[,2]
  rf_pred2 <- prediction(rf_pred, rf_te$target)
  rf_perf <- performance(rf_pred2,"tpr","fpr")
  Gini = round(((slot(performance(rf_pred2, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2)
  ACC = round(performance(rf_pred2, measure = "auc")@y.values[[1]]*100, 2)
  
  print(paste("RF Accuracy: ", ACC))
  rf_acc[j]<- ACC
}

rf_acc1 <- data.frame(acc=c(83.68, 74.53, 82.42, 81.51, 75.33, 84.30, 70.11, 82.87, 81.63, 77.73))
rf_acc1_m <- mean(rf_acc1$acc)

ev_df_m6_1 <- data.frame(Gini = rf_acc1_m*2-100, AUC = rf_acc1_m)
ev_df_m6_1 <- ggtexttable(ev_df_m6_1, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10), rows = NULL)


# ntree = 1000, mtry = 15, nodesize = 6)
rf_acc2 <- data.frame(acc=c(82.73, 77.89, 82.42, 83.00, 75.50, 84.36, 75.72, 81.38, 79.90, 77.15))
rf_acc2_m <- mean(rf_acc2$acc)

acc6 = rf_acc2_m
Gini6 = rf_acc2_m*2-100
ev_df_m6_2 <- data.frame(Gini = rf_acc2_m*2-100, AUC = rf_acc2_m)
ev_df_m6_2 <- ggtexttable(ev_df_m6_2, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10), rows = NULL)


# GB
work_data_num <- data.frame(target=work_data_num$target, work_data_num[,-25])
j <- 1
k<-10
n<-floor(nrow(work_datan)/k)
p <- ()		
#passing different sets of parameters using expand.grid
parameters <- expand.grid(nround = c(200),#400,#500
                    eta=c(0.1),#0.01,0.02
                    gamma= c(1),#2,4
                    max_depth=c(6),#7,9
                    min_child_weight = c(1),#2,4
                    subsample = c(1),
                    colsample_bytree = c(0.8),
                    num_parallel_tree = c(500))

p=data.frame("nround" = integer(0),
             "max_depth"=integer(0),
             "eta"=integer(0), 
             "gamma"=integer(0), 
             "min_child_weight"=integer(0),
             "subsample"=integer(0),
             "colsample_bytree"=integer(0),
             "num_parallel_tree"= integer(0),
             "Acc"= numeric(0))


 
for (k in 1:nrow(parameters)) {
  xgb_acc=c()
  for (j in 1:k){
    d1 = ((j-1)*n+1)
    d2 = (j*n)
    dataset = d1:d2
    data_m <- sparse.model.matrix(target ~ .-1, data = work_data_num)
    x_tr<- data_m[-dataset,]
    x_te<- data_m[dataset,]
    y_tr<-work_data_num[-dataset,c(1)]
    y_tr<-as.integer(y_tr)
    y_te<-work_data_num[dataset,c(1)]
    y_te<-as.integer(y_te)
    dxgb_train <- xgb.DMatrix(data = x_train, label = y_tr)
    prm <- parameters[k,]
    n_proc <- detectCores()
    mod7 <- xgb.train(data = dxgb_train, nthread = n_proc, 
                    objective = "binary:logistic",
                    nround = prm$nround, 
                    max_depth = prm$max_depth, 
                    eta = prm$eta, 
                    gamma = prm$gamma,
                    min_child_weight = prm$min_child_weight,
                    subsample = prm$subsample, 
                    colsample_bytree = prm$colsample_bytree,
                    eval_metric = "error",
                    early_stop_round = 100, printEveryN = 100,
                    num_parallel_tree = prm$num_parallel_tree)
    xgb_pred <- predict(mod7, x_test)
    xgb_pred2 <- prediction(xgb_pred, y_te)
    xgb_perf <- performance(xgb_pred2,"tpr","fpr")
    Gini = round(((slot(performance(xgb_pred2, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2)
    ACC = round(performance(xgb_pred2, measure = "auc")@y.values[[1]]*100, 2)
    
    print(paste("XGB Accuracy: ", ACC))
    xgb_acc[i]<- ACC
    
    print(xgb_acc[i])
  }
  
  p <- rbind(p,data.frame(prm$nround, 
                        prm$max_depth, 
                        prm$eta, 
                        prm$gamma, 
                        prm$min_child_weight, 
                        prm$subsample, 
                        prm$colsample_bytree,
                        mean(xgb_acc)))
  print(p)
}


rf_acc1 <- data.frame(acc=c(83.68, 74.53, 82.42, 81.51, 75.33, 84.30, 70.11, 82.87, 81.63, 77.73))
rf_acc1_m <- mean(rf_acc1$acc)

ev_df_m6_1 <- data.frame(Gini = rf_acc1_m*2-100, AUC = rf_acc1_m)
ev_df_m6_1 <- ggtexttable(ev_df_m6_1, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10), rows = NULL)


# ntree = 1000, mtry = 15, nodesize = 6)
rf_acc2 <- data.frame(acc=c(82.73, 77.89, 82.42, 83.00, 75.50, 84.36, 75.72, 81.38, 79.90, 77.15))
rf_acc2_m <- mean(rf_acc2$acc)

acc6 = rf_acc2_m
Gini6 = rf_acc2_m*2-100
ev_df_m6_2 <- data.frame(Gini = rf_acc2_m*2-100, AUC = rf_acc2_m)
ev_df_m6_2 <- ggtexttable(ev_df_m6_2, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10), rows = NULL)


gb_acc1 <- data.frame(acc = c(0.7878788,
                              0.7171717,
                              0.7878788,
                              0.7979798,
                              0.7676768,
                              0.6969697,
                              0.7575758,
                              0.7979798,
                              0.7878788,
                              0.7575758))

gb_acc1_m <- round(mean(gb_acc1$acc), 5)

Gini7 = gb_acc1_m*200-100
ev_df_m7_1 <- data.frame(Gini = gb_acc1_m*200-100, AUC = gb_acc1_m*100)
ev_df_m7_1 <- ggtexttable(ev_df_m7_1, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

# 500 6 0.01  
gb_acc2 <- data.frame(acc = c(82.38,76.38, 79.06, 79.32, 74.80, 84.73, 75.84, 80.88, 80.79, 77.57))
gb_acc2_m <- mean(gb_acc2$acc)
Gini7 = gb_acc2_m*2-100
ev_df_m7_2 <- data.frame(Gini = gb_acc2_m*2-100, AUC = gb_acc2_m)
ev_df_m7_2 <- ggtexttable(ev_df_m7_2, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10), rows = NULL)

acc7 = gb_acc2_m
Gini7 = gb_acc2_m*2-100


### Comparision
models <- c('Logistic regression',
            'LASSO',
            'CART',
            'C5.0',
            'Random forest',
            'Gradient boosting')

models_Gini <- c(Gini1, Gini2, Gini3, Gini4, 
                 Gini6, Gini7)


models_acc <- c(acc1, acc2, acc3, acc4, 
                acc6, acc7)

ev_df <- data.frame(Model = models, Gini = models_Gini, AUC = models_acc) 
ev_df <- arrange(ev_df, desc(Gini))

ev_df <- ggtexttable(ev_df, rows = NULL, 
                     theme = ttheme(
                       colnames.style = colnames_style(fill = "white"),
                       tbody.style = tbody_style(fill = get_palette("RdBu", 6))))
                             
                             
                             
                             
 set.seed(7)
library(mlbench)
library(caret)
numCores <- detectCores()
registerDoParallel(numCores)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=5)
# run the RFE algorithm
results <- rfe(train_matrix_s, y, sizes = seq(15, 40, by=2), rfeControl=control)

print(results)
predictors(results)
plot(results, type=c("g", "o"))                            
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
                             
