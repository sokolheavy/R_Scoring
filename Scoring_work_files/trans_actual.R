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
# install.packages("classInt")



library("odbc")
library(data.table)
#library(woe) #install this package from 'by hands', instruction -> (S:\0.Learning\R\Github_packages)
library(devtools)
#source <- devtools:::source_pkg("S:/0.Learning/R/Github_packages")
#install(source)

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

setwd("S:/PROJECTS/SCORING/2018.10.31 Scoring CC/CC_no_CSF_trans_alfa")
#setwd("E:/??????????????????/????????????_??????????????/Scoring CC_no_CSF/Scoring CC_no_CSF trans alfa")
################ Get sample

#setwd("S:/PROJECTS/SCORING/0. Scorecard_Creation/R scripts")
#set data for binning from Csv
#work_data <- read.csv2("cc_no_csf_else_bin.csv")

con_k <- dbConnect(odbc(),Driver = "SQL Server",Server = "khazardbp02\\hazard",Database = "Risk_test",trusted_connection=TRUE)
con_d <- dbConnect(odbc(),Driver = "SQL Server",Server = "dhazardbp01\\hazard",Database = "Risk_test",trusted_connection=TRUE)

sample_base <- as.data.table(dbGetQuery(con_k, "select * from risk_test.dbo.Scoring_DataMart_CC_base where productgroup = 'CC' and sample_type in ('dev') and client_type = 'alfa'"))
sample_trans<- as.data.table(dbGetQuery(con_k, "select t.* from risk_test.dbo.Scoring_DataMart_CC_trans as t join risk_test.dbo.Scoring_DataMart_CC_base as b on t.id_order = b.id_order where target_60max12m in ('good', 'bad')"))

#join table - create all sample of data if you need
setkey(sample_base, id_order)
setkey(sample_trans, id_order)
total_sample<-sample_base[sample_trans, nomatch = 0]

rm(sample_base)
rm(sample_trans)

save(total_sample, file = "total_sample_trans.rda")
load(file = 'total_sample_trans.rda')

#for work with specified scoring variables you can choose columns from total_sample or create it in other way
work_data<-as.data.frame(total_sample[, c(1:14, 77:350)])

# Add 'target_for_calc'(1,0), if it doesn't exist
work_data <- add_column(work_data, 
                        target_for_calc = ifelse(work_data$target_60max12m == "good", 1,0), 
                        .after = "target_60max12m")

save(work_data, file = "work_data_init.rda")
rm(total_sample)

load(file = 'work_data_init.rda')

str(work_data)

names(work_data)[names(work_data)=='Missed_payment_ever,%']<- 'Missed_payment_ever_proc'
names(work_data)[names(work_data)=='Missed_payment_12m,%']<- 'Missed_payment_12m_proc'
names(work_data)[names(work_data)=='Missed_payment_6m,%']<- 'Missed_payment_6m_proc'
names(work_data)[names(work_data)=='Missed_payment_3m,%']<- 'Missed_payment_3m_proc'
names(work_data)[names(work_data)=='Missed_payment_1m,%']<- 'Missed_payment_1m_proc'

names(work_data)[names(work_data) %like% "#_"] 

names(work_data) <- gsub("#_", "count_", names(work_data))

names(work_data)[names(work_data) %like% "count_"] 

names(work_data)[names(work_data) %like% "% "]

names(work_data) <- gsub("% ", "proc_", names(work_data))

names(work_data)[names(work_data) %like% "proc_"] 

names(work_data) <- gsub("#, ", "count_", names(work_data))

work_data<-work_data[work_data$status == 1,]
work_data$i.inn <- NULL

sink('structure.txt')

names(work_data)

for (i in c(5:ncol(work_data)))
{
  print(names(work_data[i]))
  print(table(work_data[i], useNA = "always"))
  # readline(prompt="Press enter to next")
} 

sink()

t<-work_data[(work_data$subgroup == 'CC_A-CLUB'), ]


work_data[(work_data$Max_month_from_date_begin != work_data$Min_month_from_date_begin),]

work_data[is.na(work_data$sum_amountbegin_close_loans), ]

work_data[(work_data$Max_dpd_ever == 0) & (work_data$max_dpd_then_pay_all_payment_ever != 0),]


################ Create binning data 

# initial number of columns
begin_ncol <- ncol(work_data)

#initiate target column and first column with variables
target_ncol <- match("target_60max12m",names(work_data))

variable_fc <- 34 #put number of first variable column

target_calc <- match("target_for_calc",names(work_data))

#replace NA 
#from column to column by cicle or other way (for example from handy_scripts)
n1<- variable_fc
n2<- begin_ncol

for (i in c(n1:n2))
{ 
  print(names(work_data[i]))
  print(length(work_data[is.na(work_data[i]), i]))
 
}


for (i in c(n1:n2))
{ 
  if (names(work_data)[i] %like% "Month_since_del_more_then")
  {
    work_data[is.na(work_data[i]), i] <- 9999
  }
}


for (i in c(n1:n2))
{ work_data[is.na(work_data[i]), i] <- -999
}

sink('structure2.txt')

names(work_data)

for (i in c(5:ncol(work_data)))
{
  print(names(work_data[i]))
  print(table(work_data[i], useNA = "always"))
  # readline(prompt="Press enter to next")
} 

sink()


save(work_data, file = "work_data_corrected.rda")

load(file = 'work_data_corrected.rda')


rm(n1)
rm(n2)

# binning every variable with different type of binning('equal', 'kmeans', 'smbinning')
bin_col <- match(names(work_data[,(variable_fc):ncol(work_data)])[(as.vector(sapply(work_data[,(variable_fc):ncol(work_data)], function(x) is.numeric(x))))],
                 names(work_data))

digit<- c()
digit[begin_ncol]<-NA

#Enter precision using next loop or directly like digit[97]<-100 digit[c(3:5)]<-1 digit[135]<-0.01 etc
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


setwd("S:/PROJECTS/SCORING/2018.10.31 Scoring CC/CC_no_CSF_trans_alfa/binning")



error_list <- c()  
#loops with separate binning and creating cat vatiables
for (i in bin_col) 
{ 
  #i<-64
  print(i)
  print(names(work_data[i]), quote = FALSE)
  # equal 
  # Idea: equal number of observation in every class
  print('calculating equal method...', quote = FALSE)
  # create intervals
  eq<-try(classIntervals(floor(work_data[,i]/digit[i])*digit[i], 5, style = 'quantile'))
  if (length(grep('Error', eq)) > 0 )
  {
    error_list <- c(error_list, i)
  }else
    {
    eq$brks[length(eq$brks)]<-eq$brks[length(eq$brks)]+digit[i]
    print(eq$brks)
    }
  # kmeans 
  # Idea: search k 'center' points, that have biggest spreading of points around this 'center'(every interval have his own 'center' point)
  print('calculating kmeans method...', quote = FALSE)
  # k in {6,5,4,3,2}
  interval_count<-6
  repeat{
    set.seed(12345)
    km<-try(classIntervals(floor(work_data[,i]/digit[i])*digit[i], interval_count, style = 'kmeans'))
    
    if (length(grep('Error', km)) > 0 )
    {
      error_list <- c(error_list, i)
    }else
    {
    km$brks[length(km$brks)]<-km$brks[length(km$brks)]+digit[i]
    col_data <- with(work_data, cut(work_data[,i], km$brks,include.lowest = TRUE, 
                                    right = FALSE, ordered = TRUE,dig.lab = 10))
    
    # any class have to be bigger than 3% of all work_data
    if (min(table(col_data)/(nrow(work_data)/100)>3)) {break}
    }
    interval_count<-interval_count-1
    
    # interval_count - {6,5,4,3,2}, if interval_count<2 - end
    if (interval_count<2) {break}
  }
  if (length(grep('Error', km)) > 0 )
  {
    error_list <- c(error_list, i)
  }else
  {
  print(km$brks)}
  # smbinning
  print('calculating smbinning method...', quote = FALSE)
  # Idea: 'optimal binning' (maximization IV)
  sb_data<-work_data[c(target_calc,i)]
  sb_data[2]<-ceiling(sb_data[2]/digit[i])*digit[i]
  sb<-try(smbinning(sb_data,y=names(work_data)[target_calc],x=names(work_data)[i]))
  
  if (length(grep('Error', sb)) > 0 )
  {
    error_list <- c(error_list, i)
  }else
  {if (length(sb) > 1) {
    sb$bands[1]<-sb$bands[1]-digit[i]
    print(sb$bands)} else {print(sb)}}
  # save binnings intervals into 'rda' files
  save(eq, km, sb, file = paste(names(work_data[i]),".rda", sep = ""))
  
  rm(eq)
  rm(km)
  rm(sb)
  rm(interval_count)
  rm(col_data)
  gc()
  print('')
}

for (i in bin_col) 
{ 
  print(i)
  print(names(work_data[i]), quote = FALSE)
  
  if (file.exists(paste(names(work_data[i]),".rda", sep = "")))
  {
    load(file = paste(names(work_data[i]),".rda", sep = ""))
    
    # equal 
    if (length(eq) > 1) ##check this condition !!!!
    {
    # column name for new binning column, that bins with 'equal' method
    colname <- paste(names(work_data)[i], "cat_eq", sep="_")
    
    # set column, that bins with 'equal' method
    work_data[[colname]] <- with(work_data, cut(as.numeric(work_data[,i]), 
                                                c(min(work_data[,i])-5,unique(eq$brks)),include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    
    # for saving ordered factors all repleacements must be done on factor levels, not on work_data(!!!)
    levels(work_data[[colname]])<-gsub(",", ";", levels(work_data[[colname]]))
    }
    
    # kmeans 
    if (length(km) > 1) ##check this condition !!!!
    {
    colname <- paste(names(work_data)[i], "cat_km", sep="_")
    work_data[[colname]] <- with(work_data, cut(as.numeric(work_data[,i]), km$brks,include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
    }
    
    # smbinning
    if (length(sb) > 1) ##check this condition !!!!
    {colname <- paste(names(work_data)[i], "cat_sb", sep="_")
    work_data[[colname]] <- with(work_data, cut(as.numeric(work_data[,i]), unique(c(min(work_data[,i])-1,sb$bands)),
                                                right = TRUE, left = FALSE, ordered = TRUE,dig.lab = 10))
    levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
    }
    
  }
}

rm(eq)
rm(km)
rm(sb)
rm(colname)
gc()

rm(sb_data)
rm(digit)
rm(error_list)



################ Create sql code for binning data 
sql_bin_code<-c()

#put table name 
TableName<-"risk_test.dbo.Scoring_test_table" 
sqlcodetable = as.list(matrix(ncol = 0, nrow = 0))

for (j in bin_col) 
{ 
  print(j)
  print(names(work_data[j]), quote = FALSE)
  
  if (file.exists(paste(names(work_data[j]),".rda", sep = "")))
  {
    load(file = paste(names(work_data[j]),".rda", sep = ""))
    
    # equal 
    if (length(eq) > 1) ##check this condition !!!!
    {
    # column name for new binning column, that bins with 'equal' method
    colname <- paste(names(work_data)[j], "cat_eq", sep="_")
    brks<-c(min(work_data[,j])-1,unique(eq$brks))
    #begin
    lines <- length(brks) - 2
    sqlcodetable = rbind(paste(" alter table", TableName,"add", colname,"varchar(100) \n \n update", TableName, "set", colname, "=\n case\n"))
    for (k in 1:lines) {
      sqlcodetable = paste(sqlcodetable, paste("when", 
                                               names(work_data)[j], "<", brks[k+1], "then", "'", sprintf("%02d", 
                                                                                                         k), ":", names(work_data)[j], "<", brks[k+1], "'\n"))
    }
    k=k+1
    sqlcodetable = paste(sqlcodetable, paste("when", 
                                             names(work_data)[j], ">=", brks[k], "then", "'", sprintf("%02d", 
                                                                                                      k), ":", names(work_data)[j], ">=", brks[k], "'\n"))                                                          
    k=k+1
    sqlcodetable = paste(sqlcodetable, paste("when", names(work_data)[j], 
                                             "Is Null then", "'", sprintf("%02d", k), ":", names(work_data)[j], 
                                             "Is Null' \n"))
    sqlcodetable = paste(sqlcodetable, paste("else '99: Error' end\n\n"))
    
    sqlcodetable = gsub(" '", "'", sqlcodetable)
    sqlcodetable = gsub("' ", "'", sqlcodetable)
    sqlcodetable = gsub("then", "then ", sqlcodetable)
    sqlcodetable = gsub("'then", "' then ", sqlcodetable)
    sqlcodetable = gsub("then  ", "then ", sqlcodetable)
    sqlcodetable = gsub("else", "else ", sqlcodetable)
    sqlcodetable = gsub("'end", "' end ", sqlcodetable)
    sqlcodetable = gsub(" :", ":", sqlcodetable)
    sqlcodetable = gsub("='", "= '", sqlcodetable)
    #end
    sql_bin_code<-rbind(sql_bin_code,sqlcodetable)
  }
    
    # kmeans 
    if (length(km) > 1) ##check this condition !!!!
    {
    colname <- paste(names(work_data)[j], "cat_km", sep="_")
    brks<-km$brks
    #begin
    lines <- length(brks) - 2
    sqlcodetable = rbind(paste(" alter table", TableName,"add", colname,"varchar(100) \n \n update", TableName, "set", colname, "=\n case\n"))
    for (k in 1:lines) {
      sqlcodetable = paste(sqlcodetable, paste("when", 
                                               names(work_data)[j], "<", brks[k+1], "then", "'", sprintf("%02d", 
                                                                                                         k), ":", names(work_data)[j], "<", brks[k+1], "'\n"))
    }
    k=k+1
    sqlcodetable = paste(sqlcodetable, paste("when", 
                                             names(work_data)[j], ">=", brks[k], "then", "'", sprintf("%02d", 
                                                                                                      k), ":", names(work_data)[j], ">=", brks[k], "'\n"))                                                          
    k=k+1
    sqlcodetable = paste(sqlcodetable, paste("when", names(work_data)[j], 
                                             "Is Null then", "'", sprintf("%02d", k), ":", names(work_data)[j], 
                                             "Is Null' \n"))
    sqlcodetable = paste(sqlcodetable, paste("else '99: Error' end\n\n"))
    
    sqlcodetable = gsub(" '", "'", sqlcodetable)
    sqlcodetable = gsub("' ", "'", sqlcodetable)
    sqlcodetable = gsub("then", "then ", sqlcodetable)
    sqlcodetable = gsub("'then", "' then ", sqlcodetable)
    sqlcodetable = gsub("then  ", "then ", sqlcodetable)
    sqlcodetable = gsub("else", "else ", sqlcodetable)
    sqlcodetable = gsub("'end", "' end ", sqlcodetable)
    sqlcodetable = gsub(" :", ":", sqlcodetable)
    sqlcodetable = gsub("='", "= '", sqlcodetable)
    #end
    sql_bin_code<-rbind(sql_bin_code,sqlcodetable)
    }
    
    # smbinning
    if (length(sb) > 1) ##check this condition !!!!
    {colname <- paste(names(work_data)[j], "cat_sb", sep="_")
    ## code of smbinning.sql(sb) with some changes
    ivout<-sb
    if (is.null(ivout$groups)) {
      lines = nrow(ivout$ivtable) - 2
      sqlcodetable = rbind(paste(" alter table", TableName,"add", colname,"varchar(100) \n \n update", TableName, "set", colname, "=\n case\n"))
      for (k in 1:lines) {
        sqlcodetable = paste(sqlcodetable, paste("when", 
                                                 ivout$x, ivout$ivtable[k, 1], "then", "'", sprintf("%02d", 
                                                                                                    k), ":", ivout$x, gsub("'", "", ivout$ivtable[k, 
                                                                                                                                                  1]), "'\n"))
      }
      k = nrow(ivout$ivtable) - 1
      sqlcodetable = paste(sqlcodetable, paste("when", ivout$x, 
                                               "Is Null then", "'", sprintf("%02d", k), ":", ivout$x, 
                                               "Is Null' \n"))
      sqlcodetable = paste(sqlcodetable, paste("else '99: Error' end\n\n"))
      sqlcodetable
      sqlcodetable = gsub(" '", "'", sqlcodetable)
      sqlcodetable = gsub("' ", "'", sqlcodetable)
      sqlcodetable = gsub("then", "then ", sqlcodetable)
      sqlcodetable = gsub("'then", "' then ", sqlcodetable)
      sqlcodetable = gsub("then  ", "then ", sqlcodetable)
      sqlcodetable = gsub("else", "else ", sqlcodetable)
      sqlcodetable = gsub("'end", "' end ", sqlcodetable)
      sqlcodetable = gsub(" :", ":", sqlcodetable)
      sqlcodetable = gsub("='", "= '", sqlcodetable)
    }
    else {
      sqlcodetable = as.list(matrix(ncol = 0, nrow = 0))
      sqlcodetable = rbind(paste(" alter table", TableName,"add", colname,"varchar(100) \n \n update", TableName, "set", colname, "=\n case\n"))
      for (i in 1:length(ivout$groups)) {
        sqlcodetable = paste(sqlcodetable, paste(" when", 
                                                 ivout$x, "in (", ivout$groups[i], ") then", 
                                                 "'", sprintf("%02d", i), ":", ivout$x, gsub("'", 
                                                                                             "", ivout$groups[i]), "'\n"))
      }
      i = length(ivout$groups) + 1
      sqlcodetable = paste(sqlcodetable, paste(" when", ivout$x, 
                                               "Is Null then", "'", sprintf("%02d", i), ":", ivout$x, 
                                               "Is Null'\n"))
      sqlcodetable = paste(sqlcodetable, paste(" else '99: Error' end\n\n"))
      sqlcodetable = gsub(" '", "'", sqlcodetable)
      sqlcodetable = gsub("' ", "'", sqlcodetable)
      sqlcodetable = gsub("then", "then ", sqlcodetable)
      sqlcodetable = gsub("'then", "' then ", sqlcodetable)
      sqlcodetable = gsub("then  ", "then ", sqlcodetable)
      sqlcodetable = gsub("else", "else ", sqlcodetable)
      sqlcodetable = gsub("'end", "' end ", sqlcodetable)
      sqlcodetable = gsub(" :", ":", sqlcodetable)
      sqlcodetable = gsub("='", "= '", sqlcodetable)
    }
    sql_bin_code<-rbind(sql_bin_code,sqlcodetable)
    }
    
  }
  
}

rm(eq)
rm(km)
rm(sb)
rm(brks)
rm(colname)
rm(TableName)
rm(ivout)
rm(i)
rm(j)
rm(k)
rm(lines)
rm(sqlcodetable)

gc()

setwd("S:/PROJECTS/SCORING/2018.10.31 Scoring CC/CC_no_CSF_trans_alfa")

sink('sql_bin_code.txt')

for (i in c(1:nrow(sql_bin_code)))
{
  cat(gsub(", ", "", toString(sql_bin_code[i])))
}

sink()

rm(bin_col)

save(work_data, file = "work_data_bin.rda")
rm(work_data)

load(file = 'work_data_bin.rda')

#initiate target column and first column with variables
target_ncol <- match("target_60max12m",names(work_data))
variable_fc <- 34 #put number of first variable column
target_calc <- match("target_for_calc",names(work_data))

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

rm(factor_vars)

## IV - statistic table
iv_table <- arrange(cbind.data.frame(variables = names(bin_data[variable_fc_bin:bin_ncol])
                                     ,IV = sapply(bin_data[variable_fc_bin:bin_ncol], function(x) round(IV(X=x, Y=bin_data$target_for_calc)[1],4)), row.names = NULL),
                    desc(IV))

# Add strength of variables
iv_table$Strength <- ifelse(iv_table$IV>=1, "Suspicious",
                            ifelse(iv_table$IV>=.5, "Very strong",
                                   ifelse(iv_table$IV>=.2, "Strong",
                                          ifelse(iv_table$IV>=.1, "Average",
                                                 ifelse(iv_table$IV>=.02, "Weak", "Very weak")))))



## Remove very weak colums
n1<- variable_fc
n2<- length(names(bin_data))

for (i in c(n2:n1))
{if (iv_table$Strength[names(bin_data[i]) == iv_table$variables] == "Very weak")
{
  bin_data[i]<-NULL
}
}

bin_ncol <- ncol(bin_data)

## Remove unused columns

bin_data$overdraft_sanc_count_ever <- NULL
bin_data$fine_count_ever <- NULL
bin_data$commission_count_ever <- NULL
bin_data$overdraft_sanc_sum_ever <- NULL
bin_data$fine_sum_ever <- NULL
bin_data$commission_sum_ever <- NULL
bin_data$count_all_loans_cat_km <- NULL
bin_data$sum_amountbegin_loans_cat_eq <- NULL
bin_data$sum_amountbegin_loans_cat_km <- NULL
bin_data$sum_amountbegin_open_loans_cat_km <- NULL
bin_data$sum_amountbegin_close_loans_cat_km <- NULL
bin_data$sum_amountmax_all_loans_cat_km <- NULL
bin_data$sum_amountmax_open_loans_cat_eq <- NULL
bin_data$sum_amountmax_open_loans_cat_sb <- NULL
bin_data$sum_amountmax_close_loans_cat_km <- NULL
bin_data$max_amountbegin_all_loans_cat_eq <- NULL
bin_data$max_amountbegin_all_loans_cat_km <- NULL
bin_data$max_amountmax_all_loans_cat_sb <- NULL
bin_data$max_amountmax_all_loans_cat_km <- NULL
bin_data$out_to_amountbegin_cat_km <- NULL
bin_data$out_to_amountmax_cat_km <- NULL
bin_data$Sum_cur_out_cat_eq <- NULL
bin_data$Sum_cur_out_cat_km <- NULL
bin_data$Max_month_from_date_begin_cat_eq <- NULL
bin_data$Max_month_from_date_begin_cat_km <- NULL
bin_data$Min_month_from_date_begin_cat_eq <- NULL
bin_data$Min_month_from_date_begin_cat_km <- NULL
bin_data$From_first_to_last_loan_monthes_cat_km <- NULL
bin_data$Max_dpd_ever_cat_eq <- NULL
bin_data$Max_dpd_ever_cat_km <- NULL
bin_data$Max_dpd_12m_cat_km <- NULL
bin_data$Max_dpd_6m_cat_km <- NULL
bin_data$MaxCC_out_ever_cat_km <- NULL
bin_data$MaxCC_out_12m_cat_km <- NULL
bin_data$MaxCC_out_6m_cat_km <- NULL
bin_data$MaxCC_out_3m_cat_km <- NULL
bin_data$MaxCC_out_1m_cat_km <- NULL
bin_data$MaxCC_usage_ever_cat_km <- NULL
bin_data$MaxCC_usage_ever_cat_sb <- NULL
bin_data$MaxCC_usage_12m_cat_km <- NULL
bin_data$MaxCC_usage_12m_cat_sb <- NULL
bin_data$MaxCC_usage_6m_cat_eq <- NULL
bin_data$MaxCC_usage_6m_cat_km <- NULL
bin_data$MaxCC_out_3m_cat_km <- NULL
bin_data$MaxCC_out_3m_cat_sb <- NULL
bin_data$MaxCC_out_1m_cat_eq <- NULL
bin_data$MaxCC_out_1m_cat_km <- NULL
bin_data$Missed_payment_ever_cat_eq <- NULL
bin_data$Missed_payment_ever_cat_km <- NULL
bin_data$Missed_payment_ever_12m_cat_eq <- NULL
bin_data$Missed_payment_ever_12m_cat_km <- NULL
bin_data$Missed_payment_ever_6m_cat_eq <- NULL
bin_data$Missed_payment_ever_6m_cat_km <- NULL
bin_data$Missed_payment_ever_3m_cat_km <- NULL
bin_data$Missed_payment_ever_proc_cat_eq <- NULL
bin_data$Missed_payment_12m_proc_cat_eq <- NULL
bin_data$Missed_payment_6m_proc_cat_eq <- NULL
bin_data$max_dpd_then_pay_all_payment_ever_cat_eq <- NULL
bin_data$max_dpd_then_pay_all_payment_ever_cat_km <- NULL
bin_data$max_dpd_then_pay_all_payment_12m_cat_km <- NULL
bin_data$max_dpd_then_pay_all_payment_6m_cat_km <- NULL
bin_data$dpd_frequency_ever_cat_eq <- NULL
bin_data$dpd_frequency_ever_cat_km <- NULL
bin_data$dpd_frequency_12m_cat_eq <- NULL
bin_data$dpd_frequency_12m_cat_km <- NULL
bin_data$dpd_frequency_6m_cat_eq <- NULL
bin_data$dpd_frequency_3m_cat_km <- NULL
bin_data$dpd_frequency_1m_cat_eq <- NULL
bin_data$Month_since_del_more_then_0_cat_km <- NULL
bin_data$Month_since_del_more_then_7_cat_eq <- NULL
bin_data$Month_since_del_more_then_7_cat_km <- NULL
bin_data$Month_since_del_more_then_15_cat_eq <- NULL
bin_data$Month_since_del_more_then_15_cat_km <- NULL
bin_data$Month_since_del_more_then_30_cat_km <- NULL
bin_data$Month_since_del_more_then_60_cat_km <- NULL
bin_data$Month_since_del_more_then_90_cat_km <- NULL
bin_data$proc_Purchase_ratio_ever_cat_km <- NULL
bin_data$proc_Cash_ratio_ever_cat_eq <- NULL
bin_data$avg_Purchase_sum_ever_cat_km <- NULL
bin_data$max_Purchase_sum_ever_cat_km <- NULL
bin_data$Purchase_count_ever_cat_eq <- NULL
bin_data$Purchase_count_ever_cat_km <- NULL
bin_data$Trans_avg_ever_cat_km <- NULL
bin_data$Trans_avg_ever_cat_eq <- NULL
bin_data$Trans_max_ever_cat_eq <- NULL
bin_data$Trans_count_ever_cat_km <- NULL
bin_data$Trans_count_ever_cat_eq <- NULL
bin_data$cafe_client_ever_cat_km <- NULL
bin_data$shop_client_ever_cat_km <- NULL
bin_data$shop_client_ever_cat_eq <- NULL
bin_data$supermarket_client_ever_cat_eq <- NULL
bin_data$supermarket_client_ever_cat_km <- NULL
bin_data$proc_Purchase_ratio_12m_cat_eq <- NULL
bin_data$proc_Purchase_ratio_12m_cat_km <- NULL
bin_data$proc_Cash_ratio_12m_cat_eq <- NULL
bin_data$proc_Cash_ratio_12m_cat_km <- NULL
bin_data$max_Purchase_sum_12m_cat_eq <- NULL
bin_data$max_Purchase_sum_12m_cat_km <- NULL
bin_data$Purchase_count_12m_cat_eq <- NULL
bin_data$max_Cash_12m_cat_eq <- NULL
bin_data$trans_avg_12m_cat_km <- NULL
bin_data$trans_avg_12m_cat_eq <- NULL
bin_data$trans_max_12m_cat_eq <- NULL
bin_data$supermarket_client_12m_cat_eq <- NULL
bin_data$shop_client_12m_cat_eq <- NULL
bin_data$usage_ratio_6m_cat_eq <- NULL
bin_data$proc_Purchase_ratio_6m_cat_eq <- NULL
bin_data$proc_Purchase_ratio_6m_cat_km <- NULL
bin_data$proc_Cash_ratio_6m_cat_eq <- NULL
bin_data$proc_Cash_ratio_6m_cat_km <- NULL
bin_data$avg_Purchase_sum_6m_cat_eq <- NULL
bin_data$max_Purchase_sum_6m_cat_eq <- NULL
bin_data$max_Purchase_sum_6m_cat_km <- NULL
bin_data$Purchase_count_6m_cat_eq <- NULL
bin_data$Purchase_count_6m_cat_km <- NULL
bin_data$avg_Cash_6m_cat_eq <- NULL
bin_data$avg_Cash_6m_cat_km <- NULL
bin_data$max_Cash_6m_cat_km <- NULL
bin_data$Cash_count_6m_cat_sb <- NULL
bin_data$trans_avg_6m_cat_eq <- NULL
bin_data$trans_avg_6m_cat_km <- NULL
bin_data$trans_max_6m_cat_eq <- NULL
bin_data$trans_count_6m_cat_eq <- NULL
bin_data$trans_count_6m_cat_km <- NULL
bin_data$shop_client_6m_cat_eq <- NULL
bin_data$shop_client_6m_cat_km <- NULL
bin_data$supermarket_client_6m_cat_eq <- NULL
bin_data$proc_Purchase_ratio_3m_cat_eq <- NULL
bin_data$proc_Purchase_ratio_3m_cat_km <- NULL
bin_data$proc_Cash_ratio_3m_cat_eq <- NULL
bin_data$proc_Cash_ratio_3m_cat_sb <- NULL
bin_data$avg_Purchase_sum_3m_cat_eq <- NULL
bin_data$max_Purchase_sum_3m_cat_eq <- NULL
bin_data$max_Purchase_sum_3m_cat_km <- NULL
bin_data$Purchase_count_3m_cat_km <- NULL
bin_data$Purchase_count_3m_cat_sb <- NULL
bin_data$avg_Cash_3m_cat_eq <- NULL
bin_data$avg_Cash_3m_cat_km <- NULL
bin_data$Cash_count_3m_cat_eq <- NULL
bin_data$Cash_count_3m_cat_km <- NULL
bin_data$trans_avg_3m_cat_eq <- NULL
bin_data$trans_avg_3m_cat_km <- NULL
bin_data$trans_max_3m_cat_km <- NULL
bin_data$trans_max_3m_cat_sb <- NULL
bin_data$trans_count_3m_cat_eq <- NULL
bin_data$trans_count_3m_cat_km <- NULL
bin_data$shop_client_3m_cat_eq <- NULL
bin_data$shop_client_3m_cat_km <- NULL
bin_data$supermarket_client_3m_cat_eq <- NULL
bin_data$usage_ratio_1m_cat_eq <- NULL
bin_data$proc_Purchase_ratio_1m_cat_eq <- NULL
bin_data$proc_Cash_ratio_1m_cat_eq <- NULL
bin_data$proc_Cash_ratio_1m_cat_sb <- NULL
bin_data$avg_Purchase_sum_1m_cat_eq <- NULL
bin_data$max_Purchase_sum_1m_cat_eq <- NULL
bin_data$Purchase_count_1m_cat_eq <- NULL
bin_data$Purchase_count_1m_cat_km <- NULL
bin_data$Cash_count_1m_cat_eq <- NULL
bin_data$trans_avg_1m_cat_eq <- NULL
bin_data$trans_max_1m_cat_km <- NULL
bin_data$trans_max_1m_cat_eq <- NULL
bin_data$trans_count_1m_cat_km <- NULL
bin_data$max_date_diff_cash_ever_cat_eq <- NULL
bin_data$max_date_diff_cash_ever_cat_km <- NULL
bin_data$avg_date_diff_cash_ever_cat_sb <- NULL
bin_data$max_date_diff_cash_12m_cat_eq <- NULL
bin_data$max_date_diff_cash_12m_cat_km <- NULL
bin_data$avg_date_diff_cash_12m_cat_eq <- NULL
bin_data$max_date_diff_cash_6m_cat_eq <- NULL
bin_data$avg_date_diff_cash_6m_cat_km <- NULL
bin_data$max_date_diff_purch_3m_cat_eq <- NULL
bin_data$max_date_diff_cash_3m_cat_eq <- NULL
bin_data$avg_date_diff_cash_3m_cat_eq <- NULL
bin_data$avg_date_diff_cash_1m_cat_km <- NULL
bin_data$proc_trans_limit_cur_ever_cat_km <- NULL
bin_data$proc_trans_limit_cur_ever_cat_sb <- NULL
bin_data$proc_cash_limit_cur_ever_cat_eq <- NULL
bin_data$proc_trans_limit_cur_12m_cat_km <- NULL
bin_data$proc_trans_limit_cur_12m_cat_sb <- NULL
bin_data$proc_cash_limit_cur_12m_cat_eq <- NULL
bin_data$proc_trans_limit_cur_6m_cat_km <- NULL
bin_data$proc_trans_limit_cur_6m_cat_sb <- NULL
bin_data$proc_cash_limit_cur_6m_cat_sb <- NULL
bin_data$proc_trans_limit_cur_3m_cat_km <- NULL
bin_data$proc_trans_limit_cur_3m_cat_sb <- NULL
bin_data$proc_Purchase_limit_cur_3m_cat_km <- NULL
bin_data$proc_Purchase_limit_cur_3m_cat_sb <- NULL
bin_data$proc_cash_limit_cur_3m_cat_eq <- NULL
bin_data$proc_cash_limit_cur_3m_cat_km <- NULL
bin_data$proc_cash_limit_cur_1m_cat_eq <- NULL
bin_data$count_max_trans_wk_ever_cat_eq <- NULL
bin_data$max_trans_day_sum_ever_cat_km <- NULL
bin_data$max_trans_wk_sum_ever_cat_km <- NULL
bin_data$max_trans_day_sum_12m_cat_km <- NULL
bin_data$max_trans_wk_sum_12m_cat_km <- NULL
bin_data$count_max_trans_day_6m_cat_eq <- NULL
bin_data$count_max_trans_wk_6m_cat_km <- NULL
bin_data$count_max_trans_wk_6m_cat_sb <- NULL
bin_data$count_max_trans_day_3m_cat_eq <- NULL
bin_data$count_max_trans_day_3m_cat_sb <- NULL
bin_data$count_max_trans_wk_3m_cat_eq <- NULL
bin_data$max_trans_day_sum_3m_cat_eq <- NULL
bin_data$max_trans_day_sum_3m_cat_km <- NULL
bin_data$max_trans_wk_sum_3m_cat_eq <- NULL
bin_data$max_trans_wk_sum_3m_cat_km <- NULL
bin_data$max_trans_day_sum_1m_cat_eq <- NULL
bin_data$max_trans_day_sum_1m_cat_km <- NULL
bin_data$max_trans_wk_sum_1m_cat_eq <- NULL
bin_data$max_trans_wk_sum_1m_cat_km <- NULL
bin_data$Max_consecutive_Dates_ever_cat_sb <- NULL
bin_data$Max_consecutive_Dates_6m_cat_eq <- NULL
bin_data$Max_consecutive_Dates_3m_cat_sb <- NULL
bin_data$credit_period_max_sum_cat_sb <- NULL

names_bin_data_after_clearing <- names(bin_data)
#save(names_bin_data_after_clearing, file = 'names_bin_data_after_clearing.rda')

bin_data$sum_amountbegin_open_loans_cat_eq <- NULL
bin_data$avg_Purchase_sum_ever_cat_eq <- NULL
bin_data$avg_Purchase_sum_12m_cat_eq <- NULL
bin_data$Cash_count_12m_cat_eq <- NULL
bin_data$Month_since_del_more_then_0_cat_eq <- NULL

#with unstability in BR
bin_data$Max_dpd_12m_cat_eq <- NULL
bin_data$Max_dpd_6m_cat_eq <- NULL
bin_data$proc_Cash_ratio_ever_cat_km <- NULL
bin_data$count_max_trans_day_3m_cat_km <- NULL
bin_data$trans_count_12m_cat_eq <- NULL
bin_data$max_date_diff_purch_6m_cat_km <- NULL
bin_data$max_date_diff_cash_6m_cat_km <- NULL
bin_data$max_date_diff_purch_3m_cat_km <- NULL
bin_data$max_date_diff_cash_3m_cat_km <- NULL
bin_data$avg_date_diff_cash_3m_cat_km <- NULL
bin_data$max_dpd_then_pay_all_payment_12m_cat_eq <- NULL
bin_data$max_dpd_then_pay_all_payment_6m_cat_eq <- NULL
bin_data$dpd_frequency_6m_cat_km <- NULL


bin_ncol <- ncol(bin_data)

#change variables categories

levels(bin_data$count_open_loans_cat_eq)[levels(bin_data$count_open_loans_cat_eq) == "[1.5;2.5)"] <- "[1.5;4.5]"
levels(bin_data$count_open_loans_cat_eq)[levels(bin_data$count_open_loans_cat_eq) == "[2.5;4.5]"] <- "[1.5;4.5]"

levels(bin_data$Missed_payment_ever_1m_cat_eq)[levels(bin_data$Missed_payment_ever_1m_cat_eq) == "[0.5;1.5)"] <- "[0.5;170]"
levels(bin_data$Missed_payment_ever_1m_cat_eq)[levels(bin_data$Missed_payment_ever_1m_cat_eq) == "[1.5;169.8333333]"] <- "[0.5;170]"

levels(bin_data$proc_Cash_ratio_3m_cat_km)[levels(bin_data$proc_Cash_ratio_3m_cat_km) == "[-999;-499.5)"] <- "[-999;0.145)"
levels(bin_data$proc_Cash_ratio_3m_cat_km)[levels(bin_data$proc_Cash_ratio_3m_cat_km) == "[-499.5;0.145)"] <- "[-999;0.145)"

levels(bin_data$proc_Cash_ratio_1m_cat_km)[levels(bin_data$proc_Cash_ratio_1m_cat_km) == "[0.375;0.625)"] <- "[0.375;0.865)"
levels(bin_data$proc_Cash_ratio_1m_cat_km)[levels(bin_data$proc_Cash_ratio_1m_cat_km) == "[0.625;0.865)"] <- "[0.375;0.865)"

levels(bin_data$proc_trans_limit_cur_6m_cat_eq)[levels(bin_data$proc_trans_limit_cur_6m_cat_eq) == "[-1004;-999)"] <- "[-1004;0.17)"
levels(bin_data$proc_trans_limit_cur_6m_cat_eq)[levels(bin_data$proc_trans_limit_cur_6m_cat_eq) == "[-999;-99)"] <- "[-1004;0.17)"
levels(bin_data$proc_trans_limit_cur_6m_cat_eq)[levels(bin_data$proc_trans_limit_cur_6m_cat_eq) == "[-99;0.17)"] <- "[-1004;0.17)"

levels(bin_data$proc_cash_limit_cur_6m_cat_eq)[levels(bin_data$proc_cash_limit_cur_6m_cat_eq) == "[-1004;-999)"] <- "[-1004;0.16)"
levels(bin_data$proc_cash_limit_cur_6m_cat_eq)[levels(bin_data$proc_cash_limit_cur_6m_cat_eq) == "[-999;0)"] <- "[-1004;0.16)"
levels(bin_data$proc_cash_limit_cur_6m_cat_eq)[levels(bin_data$proc_cash_limit_cur_6m_cat_eq) == "[0;0.16)"] <- "[-1004;0.16)"

levels(bin_data$proc_cash_limit_cur_3m_cat_sb)[levels(bin_data$proc_cash_limit_cur_3m_cat_sb) == "(-1000;-999.01]"] <- "(-1000;0.06]"
levels(bin_data$proc_cash_limit_cur_3m_cat_sb)[levels(bin_data$proc_cash_limit_cur_3m_cat_sb) == "(-999.01;-999]"] <- "(-1000;0.06]"
levels(bin_data$proc_cash_limit_cur_3m_cat_sb)[levels(bin_data$proc_cash_limit_cur_3m_cat_sb) == "(-999;0.06]"] <- "(-1000;0.06]"

levels(bin_data$max_Cash_ever_cat_sb)[levels(bin_data$max_Cash_ever_cat_sb) == "(100;450]"] <- "(100;950]"
levels(bin_data$max_Cash_ever_cat_sb)[levels(bin_data$max_Cash_ever_cat_sb) == "(450;950]"] <- "(100;950]"

bin_ncol <- ncol(bin_data)

#remove variables related to Application scorecard

bin_data$MaxCC_usage_ever_cat_eq <- NULL
bin_data$MaxCC_usage_ever_cat_km <- NULL
bin_data$MaxCC_usage_ever_cat_sb <- NULL

bin_data$MaxCC_out_12m_cat_eq <- NULL
bin_data$MaxCC_out_12m_cat_km <- NULL
bin_data$MaxCC_out_12m_cat_sb <- NULL

bin_data$MaxCC_usage_1m_cat_eq <- NULL
bin_data$MaxCC_usage_1m_cat_km <- NULL
bin_data$MaxCC_usage_1m_cat_sb <- NULL

bin_data$max_amountbegin_all_loans_cat_eq <- NULL
bin_data$max_amountbegin_all_loans_cat_km <- NULL
bin_data$max_amountbegin_all_loans_cat_sb <- NULL

### and correlation >= 0.75

bin_data$MaxCC_usage_3m_cat_eq <- NULL
bin_data$MaxCC_usage_3m_cat_km <- NULL
bin_data$MaxCC_usage_3m_cat_sb <- NULL

bin_data$MaxCC_usage_12m_cat_eq <- NULL
bin_data$MaxCC_usage_12m_cat_km <- NULL
bin_data$MaxCC_usage_12m_cat_sb <- NULL

bin_data$MaxCC_usage_6m_cat_eq <- NULL
bin_data$MaxCC_usage_6m_cat_km <- NULL
bin_data$MaxCC_usage_6m_cat_sb <- NULL

bin_data$sum_amountbegin_loans_cat_eq<-NULL
bin_data$sum_amountbegin_loans_cat_km<-NULL
bin_data$sum_amountbegin_loans_cat_sb<-NULL

bin_data$sum_amountbegin_open_loans_cat_eq<-NULL
bin_data$sum_amountbegin_open_loans_cat_km<-NULL
bin_data$sum_amountbegin_open_loans_cat_sb<-NULL

bin_data$sum_amountmax_all_loans_cat_eq<-NULL
bin_data$sum_amountmax_all_loans_cat_km<-NULL
bin_data$sum_amountmax_all_loans_cat_sb<-NULL

bin_data$MaxCC_out_ever_cat_eq <- NULL
bin_data$MaxCC_out_ever_cat_km <- NULL
bin_data$MaxCC_out_ever_cat_sb <- NULL

bin_data$MaxCC_out_6m_cat_eq <- NULL
bin_data$MaxCC_out_6m_cat_km <- NULL
bin_data$MaxCC_out_6m_cat_sb <- NULL

bin_data$MaxCC_out_1m_cat_eq <- NULL
bin_data$MaxCC_out_1m_cat_km <- NULL
bin_data$MaxCC_out_1m_cat_sb <- NULL

bin_data$MaxCC_out_3m_cat_eq <- NULL
bin_data$MaxCC_out_3m_cat_km <- NULL
bin_data$MaxCC_out_3m_cat_sb <- NULL

bin_ncol <- ncol(bin_data)

#remove high correlation > 0.8
bin_data$max_trans_day_sum_ever_cat_eq <- NULL
bin_data$max_trans_day_sum_ever_cat_sb <- NULL
bin_data$max_trans_day_sum_12m_cat_eq <- NULL
bin_data$max_trans_day_sum_12m_cat_sb <- NULL
bin_data$max_trans_day_sum_6m_cat_eq <- NULL
bin_data$max_trans_day_sum_6m_cat_sb <- NULL
bin_data$max_trans_day_sum_3m_cat_eq <- NULL
bin_data$max_trans_day_sum_3m_cat_sb <- NULL
bin_data$max_trans_day_sum_1m_cat_eq <- NULL
bin_data$max_trans_day_sum_1m_cat_sb <- NULL
bin_data$avg_date_diff_cash_1m_cat_sb <- NULL
bin_data$avg_Cash_1m_cat_km <- NULL
bin_data$Missed_payment_ever_1m_cat_eq <- NULL
bin_data$avg_date_diff_cash_3m_cat_sb <- NULL

bin_data$credit_period_max_sum_cat_km <- NULL
bin_data$max_date_diff_purch_3m_cat_km <- NULL

bin_ncol <- ncol(bin_data)

######add new cross columns - combination all columns with each other
# names(bin_data)
# for(i in variable_fc_bin:(bin_ncol-1))
# {
#   for (j in (i+1):bin_ncol)
#   {
#     if (((substr(names(bin_data[i]),1,gregexpr('_cat', names(bin_data[i]), fixed=TRUE)[[1]][1]-1) == "")|(substr(names(bin_data[j]),1,gregexpr('_cat', names(bin_data[j]), fixed=TRUE)[[1]][1]-1) == "")|(substr(names(bin_data[i]),1,gregexpr('_cat', names(bin_data[i]), fixed=TRUE)[[1]][1]-1) != substr(names(bin_data[j]),1,gregexpr('_cat', names(bin_data[j]), fixed=TRUE)[[1]][1]-1) ))) 
#     {
#       print(paste(i, '_', j, ': ',names(bin_data[i]), ' and ', names(bin_data[j]), ' = ', names(bin_data[i]), '_x_', names(bin_data[j]), sep = ''))
#       colname <- paste(names(bin_data[i]), 'x', names(bin_data[j]), sep = '_')
#       bin_data[[colname]] <- paste(bin_data[,i], 'x', bin_data[,j], sep = '_')
#     }
#     else
#     {
#       print(paste(i, '_', j, ': ',names(bin_data[i]), ' and ', names(bin_data[j]), ' = ...', sep = ''))
#     }
#   }
# }
# 
# rm(colname)
# rm(i)
# rm(j)
# 
# bin_ncol <- ncol(bin_data)

#unique(bin_data$new_column)
#unique(bin_data[98])
#bin_data[1,97]
#bin_data[1,98]
#bin_data[1,175]

## All variables should have 'factor' type, so convert variables, that not is a 'factor', to 'factor' 

# select variables that shouldn`t conver to the 'fator'
factor_vars <- c(names(bin_data)[1:(variable_fc_bin-1)]
                 ,names(which(sapply(bin_data[,variable_fc_bin:ncol(bin_data)], is.factor))))

# convert "unfactor" variables to 'factor'
bin_data[setdiff(names(bin_data),factor_vars)] <- data.frame(
  sapply(
    select(bin_data, -factor_vars), as.factor))

rm(factor_vars)

## IV - statistic table
iv_table <- arrange(cbind.data.frame(variables = names(bin_data[variable_fc_bin:bin_ncol])
                                     ,IV = sapply(bin_data[variable_fc_bin:bin_ncol], function(x) round(IV(X=x, Y=bin_data$target_for_calc)[1],4)), row.names = NULL),
                    desc(IV))

# Add strength of variables
iv_table$Strength <- ifelse(iv_table$IV>=1, "Suspicious",
                            ifelse(iv_table$IV>=.5, "Very strong",
                                   ifelse(iv_table$IV>=.2, "Strong",
                                          ifelse(iv_table$IV>=.1, "Average",
                                                 ifelse(iv_table$IV>=.02, "Weak", "Very weak")))))

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

names(bin_data)

############### Create HTML-file with statistics for all bining variables
#install.package("R2HTML")
library("R2HTML")

# set name of folder
folder_name <- "html_plots_3"

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

############### Create PDF-file with statistics for all bining variables
# 
# Total<-length(bin_data$target_for_calc)
# Good<-sum(bin_data$target_for_calc)
# Bad<-Total-Good
# 
# i <- 1
# 
# pdf("IV_3.pdf",width = 16, height=14, paper='special')
# 
# # table with IV values(descending order)
# for (i in seq(1,(bin_ncol-variable_fc_bin), 100)){
#   table1 <-  ggtexttable(iv_table[i:min((i+49),nrow(iv_table)),], rows = NULL, theme = ttheme("lBlueWhite"))
#   table2 <-  ggtexttable(iv_table[(i+50):min((i+99),nrow(iv_table)),], rows = NULL, theme = ttheme("lBlueWhite"))
#   print(ggarrange(table1, table2, 
#                   ncol = 2, nrow = 1, heights = c(0.1, 0.1)))
# }
# 
# dev.off()
# 
# rm(table1)
# rm(table2)
# 
# pdf_name <- 'Statistic_of_variables_3'
# pdf_count<-20
# k <- 1
# # loop, that creates statistics for every column 
# for (i in seq(1,(bin_ncol-variable_fc_bin), pdf_count))
# {
#   print(paste(pdf_name, "_part", ((i-1)/pdf_count)+1, ".pdf", sep = ''))
#   pdf(paste(pdf_name, "_part", ((i-1)/pdf_count)+1, ".pdf", sep = ''),width = 16, height=14, paper='special')
#   for (j in (variable_fc_bin+i-1):min(variable_fc_bin+i+pdf_count-2,bin_ncol)) {
#     
#     print(paste(k, j, names(bin_data)[j]))
#     
#     plot1_hist <- ggplot(bin_data, aes(bin_data[,j])) + 
#       geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
#       scale_y_continuous(labels=scales::percent)+ 
#       geom_text(aes( y = ((..count..)/sum(..count..)),
#                      label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
#       theme(axis.text.x = element_text(angle=10, vjust=0.9),
#             plot.margin = unit(c(1,1,1,1), "cm") ) + 
#       labs( y = "Class", x = "")
#     
#     plot2_BR_line <- ggplot(bin_data, aes(x=bin_data[,j],y=bin_data[,j-variable_fc_bin+bin_ncol+1],group=1)) + 
#       geom_line(color="indianred3",size=1)+
#       geom_point(color="indianred3") +
#       theme(axis.text.x = element_text(angle=10, vjust=0.9),
#             plot.margin = unit(c(1,1,1,1), "cm") ) + 
#       scale_y_continuous(limits=c(0, 0.3),breaks=c(0.05,0.1,0.15, 0.2, 0.25, 0.3), 
#                          labels = function(x) paste0(x*100, "%"))+
#       labs( y = "BR", x = "")
#     
#     # union 2 graphics(plot1_hist, plot2_BR_line) in 1 
#     # extract gtable
#     g1 <- ggplot_gtable(ggplot_build(plot1_hist))
#     g2 <- ggplot_gtable(ggplot_build(plot2_BR_line))
#     
#     # overlap the panel of 2nd plot on that of 1st plot
#     pp <- c(subset(g1$layout, name == "panel", se = t:r))
#     g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
#                          pp$l, pp$b, pp$l)
#     
#     # axis tweaks
#     ia <- which(g2$layout$name == "axis-l")
#     ga <- g2$grobs[[ia]]
#     ax <- ga$children[[2]]
#     ax$widths <- rev(ax$widths)
#     ax$grobs <- rev(ax$grobs)
#     ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
#     g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
#     g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
#     
#     #log(x) will produce NaN any time x is less than zero(calculating 'length(x)-sum(x)' we have '-' func 'log' see that and returns error
#     options(warn = -1) 
#     
#     # calc statistic values for every column
#     aggregate_table<-aggregate(. ~ bin_data[,j], data = bin_data[c(names(bin_data)[target_calc_bin],names(bin_data)[j])],
#                                FUN = function(x) c(good = sum(x),
#                                                    bad=length(x)-sum(x),
#                                                    total = length(x),
#                                                    good2=  round((sum(x)*100)/Good,2),
#                                                    bad2=round((length(x)-sum(x))*100/Bad,2),
#                                                    total2=round((length(x)*100)/Total,2),
#                                                    BR=round((length(x)-sum(x))*100/length(x),2),
#                                                    WOE=round(log((sum(x)/Good)/((length(x)-sum(x))/Bad)),4)))[,c(1,2)]
#     
#     #log(x) will produce NaN any time x is less than zero(calculating 'length(x)-sum(x)' we have '-' func 'log' see that and returns error
#     aggregate_table<-cbind(aggregate_table[,1],data.frame(aggregate_table[,2]))
#     names(aggregate_table)<-c(names(bin_data)[j],"good, #","bad, #","total, #","good, %","bad, %","total, %","BR, %","WOE")
#     
#     # chisq.test
#     
#     var_for_group <- names(bin_data)[j]
#     chisq_table <-  bin_data %>%
#       select(c(j,target_calc_bin)) %>%
#       group_by_(.dots = var_for_group) %>%
#       summarise_all(funs(good = sum(.),
#                          bad = (n() - sum(.)))) %>%
#       select(-1) %>% 
#       t() 
#     
#     # set chisq.test value and p_value
#     chisq <- round(as.data.frame(chisq.test(chisq_table)[1])[1,1], 2)
#     p_value_chisq <- round(as.data.frame(chisq.test(chisq_table)[3])[1,1], 4)
#     
#     # Data visualization in pdf
#     # value from 'aggregate_table' sets in the ggplot object
#     table <- ggtexttable(aggregate_table, rows = NULL, theme = ttheme("lRedWhite"))
#     
#     # set name of variable and her 'Strength'(dependense of IV: 'Strong', Weak, 'Very weak' and etc)
#     text1 <- paste0("
#                     ",k,". ",names(bin_data)[j],": ", iv_table$Strength[iv_table$variables == names(bin_data)[j]])
#     k <- k+1
#     # set style of 'text1'
#     title1 <- ggparagraph(text = text1, face = "italic", size = 25, color = "black")
#     
#     
#     text2 <- paste0("                  ","IV ="
#                     ,round(iv_table$IV[iv_table$variables == names(bin_data)[j]],4), sep = " ")
#     title2 <- ggparagraph(text = text2, face = "italic", size = 20, color = "black")
#     
#     
#     text3 <- paste0("                  ","Chisq.test = "
#                     ,chisq, "; p_value = ", p_value_chisq, sep = "  ")
#     title3 <- ggparagraph(text = text3, face = "italic", size = 20, color = "black")
#     
#     
#     
#     # union 4 object in one file: 
#     print(ggarrange(title1, title2, title3, g, table , 
#                     ncol = 1, nrow = 5,heights = c(0.08, 0.04, 0.04, 0.3, 0.2)))
#     
#   }
#   
#   dev.off()
# }
# 
# rm(aggregate_table)
# rm(ax)
# rm(chisq)
# rm(chisq_table)
# rm(g)
# rm(g1)
# rm(g2)
# rm(ga)
# rm(i)
# rm(ia)
# rm(j)
# rm(p_value_chisq)
# rm(table)
# rm(text1)
# rm(text2)
# rm(text3)
# rm(title1)
# rm(title2)
# rm(title3)
# rm(var_for_group)
# rm(pdf_name)
# rm(pdf_name_init)
# rm(pdf_count)
# 
# rm(pp)
# rm(plot1_hist)
# rm(plot2_BR_line)

## Correlation
# Calc_WOE

# select only bining data (remove column with 'BR') and sorting columns by IV

data_woe <- select(bin_data, c(target_for_calc, 
                               match(iv_table$variables, names(bin_data)))) %>%
  select(-(grep("BR", names(bin_data))-target_calc_bin+1))

# create 'woe_table'. It consists of 2 column("WOE" + name_of_variables, WOE_value)
woe_list = list()
for (i in 2:ncol(data_woe)){
  var_for_group <- names(data_woe)[i]
  column_woe <- paste("WOE", names(data_woe)[i] , sep="_")
  woe_table <- data_woe %>%
    select(c(i,1)) %>%
    group_by_(.dots = var_for_group) %>%
    summarise_all(funs(!!column_woe := log((sum(.)/Good)/((length(.)-sum(.))/Bad))))
  
  # join 'woe_table' to the table with bining variables                      
  data_woe <- left_join(data_woe,woe_table,by=names(data_woe)[i])
  
  #create woe_list
  woe_list[[i]] = woe_table
} 

rm(var_for_group)
rm(column_woe)
rm(woe_table)
rm(i)

# Create file for Corelation:

# select onle 'WOE' columns
data_cor <- select(data_woe, grep("WOE", names(data_woe))) 

# calc Correlation
cor_table <- cor( data_cor[!is.infinite(rowSums(data_cor)),])

#build Correlation plot in pdf
pdf("CorrPlot_1_5.pdf",width = 25,height=25,paper='special')

cex.before <- par("cex")
par(cex = 0.28)

corrplot(cor_table, method="number",type = "upper",tl.col = "black", diag = FALSE)

corrplot(cor_table, method="color", 
         diag=FALSE, 
         type="upper", 
         tl.col = "black",
         addCoef.col = "black", # Add coefficient of correlation
         insig = "blank"
)
par(cex = cex.before)

dev.off()

rm(cor_table)
rm(data_cor)

## Create file for log_regresion: 

data_log_regr<- select(data_woe, c(1, grep("WOE", names(data_woe))))
#data_log_regr<- select(data_woe, -grep("WOE", names(data_woe)))

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

# if exists rows with Inf value - delete them 
data_log_regr<-data_log_regr[!is.infinite(rowSums(data_log_regr)),]

#data_log_regr<-data_log_regr[!is.infinite(rowSums(select(data_woe, c(1, grep("WOE", names(data_woe)))))),]

# factor_vars<-names(data_log_regr)[1]
# data_log_regr[setdiff(names(data_log_regr),factor_vars)] <- data.frame(
#   sapply(
#     select(data_log_regr, -factor_vars), as.factor))


## Create models
groupvars[[length(groupvars)]]
# target name:
measurevar <- "target"

# These are the variable names for different models:
groupvars = list()
groupvars[[1]]  <- names(data_log_regr)[-1]
groupvars[[2]]  <- groupvars[[1]][-5] #dti
groupvars[[3]]  <- groupvars[[2]][-25] #worktype
groupvars[[4]]  <- groupvars[[3]][-13] #number of dep
groupvars[[5]]  <- groupvars[[4]][-4] #di
groupvars[[6]]  <- groupvars[[5]][-4] #dti
groupvars[[7]]  <- groupvars[[4]][-21] #total income
groupvars[[8]]  <- groupvars[[7]][-4] #di
groupvars[[9]]  <- groupvars[[8]][-4] #dti
groupvars[[10]]  <- groupvars[[9]][-18] #Spouse soc stat
groupvars[[10]]  <- groupvars[[9]][-9]
groupvars[[11]]  <- groupvars[[10]][-15]
groupvars[[12]]  <- groupvars[[11]][-10]
groupvars[[13]]  <- groupvars[[12]][-13]
groupvars[[14]]  <- groupvars[[13]][-11]

model_num <- length(groupvars)

sink('Model_result_2.txt')

for (i in (1:model_num))
  {
  t_gv<-groupvars[[i]]
  # This returns the model:
  model <- glm(as.formula(paste(measurevar,paste(t_gv, collapse=" + "), sep=" ~ ")), data_log_regr, family = "binomial") 
               
  print(paste("Model", i))
  print("")
  #add column with predicted values
  colname <- paste('target_predict_model_', i, sep = '')
  data_log_regr[[colname]] <- predict(object = model, type = "response")
               
  #add values for perfomance plots(only for )
  pred1 <- prediction(data_log_regr[[colname]],data_log_regr$target)
               
  perf1 <- performance(pred1,"tpr","fpr")
  ks = max(attr(perf1,'y.values')[[1]]-attr(perf1,'x.values')[[1]])
  
  print(paste("KS = ",ks*100))
               
  #Plot with KS
               
  #find probability, where KC is located
  x_value_ks <- perf1@alpha.values[[1]][which((attr(perf1,'y.values')[[1]]-attr(perf1,'x.values')[[1]]) == ks)]
               
  #point for plot(Good Rate)
  cdf1 <- ecdf(subset(data_log_regr[[colname]], data_log_regr$target == 1))
  #point for plot(Bad Rate)
  cdf2 <- ecdf(subset(data_log_regr[[colname]], data_log_regr$target == 0))
               
  y_point <- cdf1(x_value_ks) 
  y2_point <- cdf2(x_value_ks) 
  # }
               
  # KS plot 
  KS_plot <- ggplot(data_log_regr, aes(x = colname, group = target ,colour = target))+
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
   
  print(paste("Gini = ",gini*100))           
  #"tpr","fpr" - True positive rate(TP/(P)), false positive rate(FP/N)
               
  #gini plot (set all together)
  plot(performance(pred1,"tpr","fpr"),lwd=2)
  lines(c(0,1),c(0,1))
  text(0.6,0.2,paste("Gini=", round(gini,4), sep=""), cex=1.4)
  title("ROC Curve")
               
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
  legend(x = 0.15,y = 0.6, c("spec", "sens", "accur"), 
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
  print("")
  print(model[["coefficients"]])
  print("")
  #False negative rate = y-1 = TP/P -1 = TP/(TP+FN) - (TP+FN)/(TP+FN) = -TN/(TP+FN) 
  #specificity = 1-x[[ind]]= 1-FP/N=1- FP/(TN+FP)=((TN+FP)/(TN+FP)) - FP/(TN+FP)= TN/(TN+FP) = TN/N - False positive rate(specificity)
}     

sink()

rm(t_gv)
rm(i)
rm(colname)
rm(perf1)
rm(perf4)
rm(perf3)
rm(perf5)
rm(colname)
rm(pred1)
rm(roc_perf)
rm(cdf1)
rm(cdf2)
rm(x_value_ks)
rm(y_point)
rm(y2_point)
rm(ks)
rm(gini)
rm(KS_plot)

#smbinning.scaling(model, pdo = 20, score = 720, odds = 99)

rm(model)
rm(measurevar)
rm(model_num)

pdo<-100.0
score<-500
odds<-2.25
A<-pdo/logb(2)
B<-score-A*logb(odds)
score_list <- list()
k<-1
for (i in (2:length(model[["coefficients"]])))
    {
      for (j in (1:length(woe_list))) 
      {
        if (length(names(woe_list[[j]][2]))>0)
        {
        if (names(model[["coefficients"]])[i] == names(woe_list[[j]][2]))
        {
          score_table<-woe_list[[j]] 
          score_table$mult<-score_table[[2]]*model[["coefficients"]][i]+model[["coefficients"]][1]/(length(model[["coefficients"]])-1)
          score_table$score_init<-score_table[[3]]*A+B/(length(model[["coefficients"]])-1)
          score_table$score<-round(score_table[[4]])
          score_list[[k]]<-score_table
          k<-k+1
        }
        }
      }
}


sample_score <- as.data.table(dbGetQuery(con_d, "select id_order, target_60max12m, appl_score, bur_score from risk_test.dbo.Scoring_test_table where sample_type in ('dev')" ))
save(sample_score, file = "sample_score.rda")

sample_score <- add_column(sample_score, 
                        target_for_calc = ifelse(sample_score$target_60max12m == "good", 1,0), 
                        .after = "target_60max12m")


#recall column with "target",take universall name(target can be 'target_for_calc','target_for_calc_30' and so on) 
names(sample_score)[grep("target_for", names(sample_score))]<-"target"

#generate "data_log_regr" with first column "target" and another columns are sorted alphabetically
data_log_regr<-sample_score[,c(3,4,5)]
#recall first column,cauth automatically programe calls it "data$target"
names(data_log_regr)[1]<-"target"

# if exists rows with Inf value - delete them 
data_log_regr<-data_log_regr[!is.infinite(rowSums(data_log_regr)),]

#data_log_regr<-data_log_regr[!is.infinite(rowSums(select(data_woe, c(1, grep("WOE", names(data_woe)))))),]

# factor_vars<-names(data_log_regr)[1]
# data_log_regr[setdiff(names(data_log_regr),factor_vars)] <- data.frame(
#   sapply(
#     select(data_log_regr, -factor_vars), as.factor))


## Create models
groupvars[[length(groupvars)]]
# target name:
measurevar <- "target"

# These are the variable names for different models:
groupvars = list()
groupvars[[1]]  <- names(data_log_regr)[-1]

model_num <- length(groupvars)

sink('Model_result_join.txt')

for (i in (1:model_num))
{
  t_gv<-groupvars[[i]]
  # This returns the model:
  model_1 <- glm(as.formula(paste(measurevar,paste(t_gv, collapse=" + "), sep=" ~ ")), data_log_regr, family = "binomial") 
  
  print(paste("Model", i))
  print("")
  #add column with predicted values
  colname <- paste('target_predict_model_', i, sep = '')
  data_log_regr[[colname]] <- predict(object = model, type = "response")
  
  #add values for perfomance plots(only for )
  pred1 <- prediction(data_log_regr[[colname]],data_log_regr$target)
  
  perf1 <- performance(pred1,"tpr","fpr")
  ks = max(attr(perf1,'y.values')[[1]]-attr(perf1,'x.values')[[1]])
  
  print(paste("KS = ",ks*100))
  
  #Plot with KS
  
  #find probability, where KC is located
  x_value_ks <- perf1@alpha.values[[1]][which((attr(perf1,'y.values')[[1]]-attr(perf1,'x.values')[[1]]) == ks)]
  
  #point for plot(Good Rate)
  cdf1 <- ecdf(subset(data_log_regr[[colname]], data_log_regr$target == 1))
  #point for plot(Bad Rate)
  cdf2 <- ecdf(subset(data_log_regr[[colname]], data_log_regr$target == 0))
  
  y_point <- cdf1(x_value_ks) 
  y2_point <- cdf2(x_value_ks) 
  # }
  
  # KS plot 
  KS_plot <- ggplot(data_log_regr, aes(x = colname, group = target ,colour = target))+
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
  
  print(paste("Gini = ",gini*100))           
  #"tpr","fpr" - True positive rate(TP/(P)), false positive rate(FP/N)
  
  #gini plot (set all together)
  plot(performance(pred1,"tpr","fpr"),lwd=2)
  lines(c(0,1),c(0,1))
  text(0.6,0.2,paste("Gini=", round(gini,4), sep=""), cex=1.4)
  title("ROC Curve")
  
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
  legend(x = 0.15,y = 0.6, c("spec", "sens", "accur"), 
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
  print("")
  print(model[["coefficients"]])
  print("")
  #False negative rate = y-1 = TP/P -1 = TP/(TP+FN) - (TP+FN)/(TP+FN) = -TN/(TP+FN) 
  #specificity = 1-x[[ind]]= 1-FP/N=1- FP/(TN+FP)=((TN+FP)/(TN+FP)) - FP/(TN+FP)= TN/(TN+FP) = TN/N - False positive rate(specificity)
}     

sink()

rm(t_gv)
rm(i)
rm(colname)
rm(perf1)
rm(perf4)
rm(perf3)
rm(perf5)
rm(colname)
rm(pred1)
rm(roc_perf)
rm(cdf1)
rm(cdf2)
rm(x_value_ks)
rm(y_point)
rm(y2_point)
rm(ks)
rm(gini)
rm(KS_plot)
