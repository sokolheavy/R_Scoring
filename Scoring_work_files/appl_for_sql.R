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

setwd("S:/PROJECTS/SCORING/2018.10.31 Scoring CC/CC_no_CSF_appl_new")
################ Get sample

#setwd("S:/PROJECTS/SCORING/0. Scorecard_Creation/R scripts")
#set data for binning from Csv
#work_data <- read.csv2("cc_no_csf_else_bin.csv")

con_k <- dbConnect(odbc(),Driver = "SQL Server",Server = "khazardbp02\\hazard",Database = "Risk_test",trusted_connection=TRUE)
con_d <- dbConnect(odbc(),Driver = "SQL Server",Server = "dhazardbp01\\hazard",Database = "Risk_test",trusted_connection=TRUE)

sample_base <- as.data.table(dbGetQuery(con_k, "select * from risk_test.dbo.Scoring_DataMart_CC_base where productgroup = 'CC' and sample_type in ('dev') and client_type = 'new'"))

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
work_data<-as.data.frame(total_sample[,c(1:143)])

# Add 'target_for_calc'(1,0), if it doesn't exist
work_data <- add_column(work_data, 
                        target_for_calc = ifelse(work_data$target_60max12m == "good", 1,0), 
                        .after = "target_60max12m")

save(total_sample, file = "total_sample.rda")
save(work_data, file = "work_data_init.rda")
rm(total_sample)

load(file = 'work_data_init.rda')

work_data$Number_of_children <- NULL
work_data$Time_in_marriage_y <- NULL
work_data$Time_in_marriage_m <- NULL
work_data$Requested_monthly_Payment <- NULL
work_data$Requested_loan_amount <- NULL
work_data$Requested_contract_amount <- NULL
work_data$Requested_loan_Initial_cash_payment<-NULL
work_data$Requested_loan_term <- NULL
work_data$Use_of_credits_last_5_years <- NULL
work_data$Loan_amount_in_banks_from_client<-NULL
work_data$Registration_term_in_months<-NULL

work_data[is.na(work_data$dti), ]
work_data<-work_data[complete.cases(work_data[,133]),]

work_data[is.na(work_data$Add_Income_from_UW), ]
names(work_data)
work_data[is.na(work_data$Add_Income_from_UW), 124] <- 0

work_data[is.na(work_data$Payments_in_alfa), ]
names(work_data)
work_data[is.na(work_data$Payments_in_alfa), 131] <- 0

work_data[is.na(work_data$Number_of_dependants), ]
names(work_data)
work_data<-work_data[complete.cases(work_data[,105]),]

work_data[is.na(work_data$Residing_term_in_months), ]
names(work_data)
work_data<-work_data[complete.cases(work_data[,108]),]

work_data <- work_data[(work_data$subgroup != 'CC_A-CLUB'), ]
unique(work_data$subgroup)

work_data$dti_clear<-(work_data$Payments_in_alfa+work_data$Payments_in_banks_total)/work_data$Total_Income
################ Create binning data 

# initial number of columns
begin_ncol <- ncol(work_data)

#initiate target column and first column with variables
target_ncol <- match("target_60max12m",names(work_data))

variable_fc <- 97 #put number of first variable column

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
{ work_data[is.na(work_data[i]), i] <- -999
}


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
    print(paste('from', min(work_data[i]), 'to', max(work_data[i]), 'with', nrow(unique(work_data[i])), 'unique values in', nrow(work_data[i]), 'records'), quote = FALSE)
    digit[i] <- as.numeric(readline(prompt="Enter precision: "))
  }
}

save(digit, file = "digit.rda")
load(file = 'digit.rda')

error_list <- c()  

##next three loops are repeating one algorithm of binning and creating cat vatiables. First one = second+third
##please, use combination of loops that better for your project

#loop with binning and creating cat vatiables in one
for (i in bin_col) 
{ 
  print(i)
  print(names(work_data[i]), quote = FALSE)
  # equal 
  # Idea: equal number of observation in every class
  print('calculating equal method...', quote = FALSE)
  # create intervals
  eq<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], 5, style = 'quantile')
  
  # column name for new binning column, that bins with 'equal' method
  colname <- paste(names(work_data)[i], "cat_eq", sep="_")
  
  # set column, that bins with 'equal' method
  work_data[[colname]] <- with(work_data, cut(work_data[,i], 
                                              c(min(work_data[,i])-1,unique(eq$brks)),include.lowest = TRUE, 
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
    colname <- paste(names(work_data)[i], "cat_km", sep="_")
    work_data[[colname]] <- with(work_data, cut(work_data[,i], km$brks,include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
    interval_count<-interval_count-1
    
    # any class have to be bigger than 3% of all work_data
    if (min(table(work_data[[colname]])/(nrow(work_data)/100)>3)) {break}
    
    # interval_count - {6,5,4,3,2}, if interval_count<2 - end
    if (interval_count<2) {break}
  }
  
  # smbinning
  print('calculating smbinning method...', quote = FALSE)
  # Idea: 'optimal binning' (maximization IV)
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
  save(eq, km, sb, file = paste(names(work_data[i]),".rda", sep = ""))
  
  rm(eq)
  rm(km)
  rm(sb)
  rm(interval_count)
  rm(colname) 
}



#loops with separate binning and creating cat vatiables
for (i in bin_col) 
{ 
  print(i)
  print(names(work_data[i]), quote = FALSE)
  # equal 
  # Idea: equal number of observation in every class
  print('calculating equal method...', quote = FALSE)
  # create intervals
  eq<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], 5, style = 'quantile')
  print(eq$brks)
  # kmeans 
  # Idea: search k 'center' points, that have biggest spreading of points around this 'center'(every interval have his own 'center' point)
  print('calculating kmeans method...', quote = FALSE)
  # k in {6,5,4,3,2}
  interval_count<-6
  repeat{
    km<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], interval_count, style = 'kmeans')
    col_data <- with(work_data, cut(work_data[,i], km$brks,include.lowest = TRUE, 
                                    right = FALSE, ordered = TRUE,dig.lab = 10))
    interval_count<-interval_count-1
    
    # any class have to be bigger than 3% of all work_data
    if (min(table(col_data)/(nrow(work_data)/100)>3)) {break}
    
    # interval_count - {6,5,4,3,2}, if interval_count<2 - end
    if (interval_count<2) {break}
  }
  print(km$brks)
  # smbinning
  print('calculating smbinning method...', quote = FALSE)
  # Idea: 'optimal binning' (maximization IV)
  sb_data<-work_data[c(target_calc,i)]
  sb_data[2]<-ceiling(sb_data[2]/digit[i])*digit[i]
  sb<-try(smbinning(sb_data,y=names(work_data)[target_calc],x=names(work_data)[i]))
  
  if (length(grep('Error', sb)) > 0 )
  {
    error_list <- c(error_list, i)
  }
  else
  {if (length(sb) > 1) {print(sb$bands)} else {print(sb)}}
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

setwd("S:/PROJECTS/SCORING/2018.10.31 Scoring CC/CC_no_CSF_appl_new/binning")

for (i in bin_col) 
{ 
  print(i)
  print(names(work_data[i]), quote = FALSE)
  
  if (file.exists(paste(names(work_data[i]),".rda", sep = "")))
  {
    load(file = paste(names(work_data[i]),".rda", sep = ""))
    
    # equal 
    
    # column name for new binning column, that bins with 'equal' method
    colname <- paste(names(work_data)[i], "cat_eq", sep="_")
    
    # set column, that bins with 'equal' method
    work_data[[colname]] <- with(work_data, cut(work_data[,i], 
                                                c(min(work_data[,i])-5,unique(eq$brks)),include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    
    # for saving ordered factors all repleacements must be done on factor levels, not on work_data(!!!)
    levels(work_data[[colname]])<-gsub(",", ";", levels(work_data[[colname]]))
    
    # kmeans 
    colname <- paste(names(work_data)[i], "cat_km", sep="_")
    work_data[[colname]] <- with(work_data, cut(work_data[,i], km$brks,include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
    
    # smbinning
    if (length(sb) > 1) ##check this condition !!!!
    {colname <- paste(names(work_data)[i], "cat_sb", sep="_")
    work_data[[colname]] <- with(work_data, cut(work_data[,i], c(min(work_data[,i])-1,unique(sb$bands)),
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
work_data <- bin_data_dev
TableName<-"risk_test.dbo.Scoring_trans_test_table" 
sqlcodetable = as.list(matrix(ncol = 0, nrow = 0))

str(bin_data_dev)
for (j in bin_data_dev) 
{ 
  print(j)
  print(names(work_data[j]), quote = FALSE)
  
  if (file.exists(paste(names(work_data[j]),".rda", sep = "")))
  {
    load(file = paste(names(work_data[j]),".rda", sep = ""))
    
    # equal 
    
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
    
    # kmeans 
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

setwd("S:/PROJECTS/SCORING/2018.10.31 Scoring CC/CC_no_CSF_appl_new")

sink('sql_bin_code_3.txt')

for (i in c(1:nrow(sql_bin_code)))
{
  cat(gsub(", ", "", toString(sql_bin_code[i])))
}

sink()

rm(bin_col)

save(work_data, file = "work_data_bin_2.rda")
rm(work_data)

load(file = 'work_data_bin_2.rda')

#initiate target column and first column with variables
target_ncol <- match("target_60max12m",names(work_data))
variable_fc <- 97 #put number of first variable column
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

## Remove unused columns

bin_data$dti_cat_km <- NULL
bin_data$Age_y_cat_km<- NULL
bin_data$Total_work_experience_in_months_cat_km <- NULL
bin_data$Total_work_experience_in_months_cat_eq <- NULL
bin_data$Total_work_experience_in_years_cat_eq <- NULL
bin_data$Passport_Age_y_cat_sb<-NULL
bin_data$Passport_Age_y_cat_eq<-NULL
bin_data$Passport_Age_y_cat_km <- NULL
bin_data$Current_work_experience_in_years_cat_eq<- NULL
bin_data$Current_work_experience_in_years_cat_km <- NULL
bin_data$Current_work_experience_in_months_cat_eq <- NULL
bin_data$Main_Income_from_client_cat_eq<-NULL
bin_data$Has_a_car_cat_km<-NULL
bin_data$Payments_in_banks_UW_cat_eq <- NULL
bin_data$Main_Income_from_UW_cat_eq <- NULL
bin_data$Payments_in_banks_total_cat_eq <- NULL
bin_data$Total_Income_cat_eq <- NULL
bin_data$Total_Income_cat_km<-NULL
bin_data$Payments_in_banks_from_client_cat_eq<-NULL
bin_data$di_cat_sb <- NULL
bin_data$Main_Income_from_UW_cat_km<- NULL
bin_data$Residing_term_in_months_cat_km <- NULL
bin_data$Residing_term_in_months_cat_sb <- NULL
bin_data$Main_Income_from_client_cat_km <- NULL
bin_data$Payments_in_alfa_cat_km<-NULL
bin_data$Monthly_charges_cat_km<-NULL
bin_data$Payments_in_banks_UW_cat_km<- NULL
bin_data$Payments_in_banks_from_client_cat_km <- NULL
bin_data$Add_Income_from_client_cat_eq <- NULL
bin_data$di_cat_km <- NULL
bin_data$Add_Income_from_UW_cat_eq<-NULL
bin_data$Add_Income_from_client_cat_km<-NULL
bin_data$Monthly_charges_cat_eq<-NULL
bin_data$Add_Income_from_UW_cat_km<- NULL
bin_data$Number_of_dependants_cat_km <- NULL
bin_data$Number_of_dependants_UW_cat_km <- NULL
bin_data$Way_of_purchase_of_habitation <- NULL
bin_data$Payments_in_banks_total_cat_km<-NULL
bin_data$Payments_in_alfa_cat_eq<-NULL

bin_ncol <- ncol(bin_data)

#change variables categories

unique(bin_data$Relationship_with_contact_person)
table(bin_data$Total_Income_cat_sb)


levels(bin_data$Education)[levels(bin_data$Education) == "HIGH"] <- "HIGH+"
levels(bin_data$Education)[levels(bin_data$Education) == "ACADEMIC_DEGREE"] <- "HIGH+"

levels(bin_data$Relationship_with_contact_person)[levels(bin_data$Relationship_with_contact_person) == "BROTHER"] <- "Other"
levels(bin_data$Relationship_with_contact_person)[levels(bin_data$Relationship_with_contact_person) == "EMPLOYEE"] <- "Other"
levels(bin_data$Relationship_with_contact_person)[levels(bin_data$Relationship_with_contact_person) == "NEIGHBOUR"] <- "Other"
levels(bin_data$Relationship_with_contact_person)[levels(bin_data$Relationship_with_contact_person) == "RELATIVE"] <- "Other"

levels(bin_data$Permanent_residence_data_full_ownership)[levels(bin_data$Permanent_residence_data_full_ownership) == "HOSTEL"] <- "NO_own_property"
levels(bin_data$Permanent_residence_data_full_ownership)[levels(bin_data$Permanent_residence_data_full_ownership) == "LIVING_WITH_FRIENDS"] <- "NO_own_property"
levels(bin_data$Permanent_residence_data_full_ownership)[levels(bin_data$Permanent_residence_data_full_ownership) == "RENT"] <- "NO_own_property"

bin_data$Number_of_employees <- factor(bin_data$Number_of_employees, ordered = TRUE, levels = c("", "LESS_THAN_4", "4_TO_15","16_TO_50","51_TO_100", "101_TO_500", "OVER_500"))

levels(bin_data$Main_income_type)[levels(bin_data$Main_income_type) == "HIRING"] <- "OFFICIAL"
levels(bin_data$Main_income_type)[levels(bin_data$Main_income_type) == "LOAN"] <- "NONOFFICIAL_and_OTHER"
levels(bin_data$Main_income_type)[levels(bin_data$Main_income_type) == "NONOFFICIAL"] <- "NONOFFICIAL_and_OTHER"
levels(bin_data$Main_income_type)[levels(bin_data$Main_income_type) == "OTHER"] <- "NONOFFICIAL_and_OTHER"

levels(bin_data$Age_y_cat_sb)[levels(bin_data$Age_y_cat_sb) == "(20;21]"] <- "(20;25]"
levels(bin_data$Age_y_cat_sb)[levels(bin_data$Age_y_cat_sb) == "(21;22]"] <- "(20;25]"
levels(bin_data$Age_y_cat_sb)[levels(bin_data$Age_y_cat_sb) == "(22;25]"] <- "(20;25]"

levels(bin_data$Total_work_experience_in_months_cat_sb)[levels(bin_data$Total_work_experience_in_months_cat_sb) == "(3;4]"] <- "(3;120]"
levels(bin_data$Total_work_experience_in_months_cat_sb)[levels(bin_data$Total_work_experience_in_months_cat_sb) == "(4;120]"] <- "(3;120]"

levels(bin_data$Total_work_experience_in_years_cat_sb)[levels(bin_data$Total_work_experience_in_years_cat_sb) == "(-1;0]"] <- "(-1;9]"
levels(bin_data$Total_work_experience_in_years_cat_sb)[levels(bin_data$Total_work_experience_in_years_cat_sb) == "(0;9]"] <- "(-1;9]"
levels(bin_data$Total_work_experience_in_years_cat_sb)[levels(bin_data$Total_work_experience_in_years_cat_sb) == "(14;16]"] <- "(14;19]"
levels(bin_data$Total_work_experience_in_years_cat_sb)[levels(bin_data$Total_work_experience_in_years_cat_sb) == "(16;19]"] <- "(14;19]"

levels(bin_data$Main_Income_from_client_cat_sb)[levels(bin_data$Main_Income_from_client_cat_sb) == "(799;800]"] <- "(799;9200]"
levels(bin_data$Main_Income_from_client_cat_sb)[levels(bin_data$Main_Income_from_client_cat_sb) == "(800;9200]"] <- "(799;9200]"

levels(bin_data$Main_Income_from_UW_cat_sb)[levels(bin_data$Main_Income_from_UW_cat_sb) == "(799;800]"] <- "(799;9200]"
levels(bin_data$Main_Income_from_UW_cat_sb)[levels(bin_data$Main_Income_from_UW_cat_sb) == "(800;9200]"] <- "(799;9200]"

levels(bin_data$Add_Income_from_client_cat_sb)[levels(bin_data$Add_Income_from_client_cat_sb) == "(-1;0]"] <- "(-1;500]"
levels(bin_data$Add_Income_from_client_cat_sb)[levels(bin_data$Add_Income_from_client_cat_sb) == "(0;500]"] <- "(-1;500]"

levels(bin_data$Add_Income_from_UW_cat_sb)[levels(bin_data$Add_Income_from_UW_cat_sb) == "(-1;0]"] <- "(-1;500]"
levels(bin_data$Add_Income_from_UW_cat_sb)[levels(bin_data$Add_Income_from_UW_cat_sb) == "(0;500]"] <- "(-1;500]"

levels(bin_data$Total_Income_cat_sb)[levels(bin_data$Total_Income_cat_sb) == "(799;800]"] <- "(799;9800]"
levels(bin_data$Total_Income_cat_sb)[levels(bin_data$Total_Income_cat_sb) == "(800;9800]"] <- "(799;9800]"

levels(bin_data$Monthly_charges_cat_sb)[levels(bin_data$Monthly_charges_cat_sb) == "(-1;0]"] <- "(-1;2100]"
levels(bin_data$Monthly_charges_cat_sb)[levels(bin_data$Monthly_charges_cat_sb) == "(0;2100]"] <- "(-1;2100]"

levels(bin_data$di_cat_eq)[levels(bin_data$di_cat_eq) == "[-576300;-576260.25)"] <- "[-576300;2700)"
levels(bin_data$di_cat_eq)[levels(bin_data$di_cat_eq) == "[-576260.25;2700)"] <- "[-576300;2700)"

levels(bin_data$dti_cat_eq)[levels(bin_data$dti_cat_eq) == "[-5;0)"] <- "[-5;0.1)"
levels(bin_data$dti_cat_eq)[levels(bin_data$dti_cat_eq) == "[0;0.1)"] <- "[-5;0.1)"

levels(bin_data$dti_cat_sb)[levels(bin_data$dti_cat_sb) == "(-1;0]"] <- "(-1;0.05]"
levels(bin_data$dti_cat_sb)[levels(bin_data$dti_cat_sb) == "(0;0.05]"] <- "(-1;0.05]"

bin_data$Number_of_dependants_cat_sb<-cut(work_data$Number_of_dependants, c(min(work_data$Number_of_dependants)-1,0,1,2, max(work_data$Number_of_dependants)), right = TRUE)
bin_data$Number_of_dependants_cat_sb <- factor(bin_data$Number_of_dependants_cat_sb, ordered = TRUE, levels = c("(-1,0]", "(0,1]", "(1,2]","(2,20]"))

bin_data$Number_of_dependants_UW_cat_sb <-cut(work_data$Number_of_dependants_UW, c(min(work_data$Number_of_dependants_UW)-1,0,1, max(work_data$Number_of_dependants_UW)), right = TRUE)
bin_data$Number_of_dependants_UW_cat_sb <- factor(bin_data$Number_of_dependants_UW_cat_sb, ordered = TRUE, levels = c("(-1,0]", "(0,1]", "(1,8]"))

bin_data$Age_y_cat_eq<-NULL
bin_data$Number_of_dependants_cat_eq<-NULL
bin_data$Number_of_dependants_UW_cat_sb<-NULL
bin_data$dti_cat_eq<-NULL
bin_data$Total_work_experience_in_years_cat_km<-NULL
bin_data$dti_clear_cat_eq<-NULL
bin_data$dti_clear_cat_km<-NULL

bin_ncol <- ncol(bin_data)

#remove related variables

bin_data$Current_work_experience_in_months_cat_km<-NULL
bin_data$Main_Income_from_UW_cat_sb<-NULL
bin_data$Add_Income_from_UW_cat_sb<-NULL
bin_data$Number_of_dependants_UW_cat_eq<-NULL
bin_data$Family_status_UW <- NULL
bin_data$Spouse_social_status_UW<- NULL
bin_data$Payments_in_banks_from_client_cat_sb<-NULL
bin_data$Payments_in_banks_total_cat_sb<-NULL
bin_data$Total_work_experience_in_months_cat_sb<-NULL

bin_ncol <- ncol(bin_data)

######add new cross columns - combination all columns with each other
names(bin_data)
for(i in variable_fc_bin:(bin_ncol-1))
{
  for (j in (i+1):bin_ncol)
  {
    if (((substr(names(bin_data[i]),1,gregexpr('_cat', names(bin_data[i]), fixed=TRUE)[[1]][1]-1) == "")|(substr(names(bin_data[j]),1,gregexpr('_cat', names(bin_data[j]), fixed=TRUE)[[1]][1]-1) == "")|(substr(names(bin_data[i]),1,gregexpr('_cat', names(bin_data[i]), fixed=TRUE)[[1]][1]-1) != substr(names(bin_data[j]),1,gregexpr('_cat', names(bin_data[j]), fixed=TRUE)[[1]][1]-1) ))) 
    {
      print(paste(i, '_', j, ': ',names(bin_data[i]), ' and ', names(bin_data[j]), ' = ', names(bin_data[i]), '_x_', names(bin_data[j]), sep = ''))
      colname <- paste(names(bin_data[i]), 'x', names(bin_data[j]), sep = '_')
      bin_data[[colname]] <- paste(bin_data[,i], 'x', bin_data[,j], sep = '_')
    }
    else
    {
      print(paste(i, '_', j, ': ',names(bin_data[i]), ' and ', names(bin_data[j]), ' = ...', sep = ''))
    }
  }
}

rm(colname)
rm(i)
rm(j)

bin_ncol <- ncol(bin_data)

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

Total<-length(bin_data$target_for_calc)
Good<-sum(bin_data$target_for_calc)
Bad<-Total-Good

i <- 1

pdf("IV_3.pdf",width = 16, height=14, paper='special')

# table with IV values(descending order)
for (i in seq(1,(bin_ncol-variable_fc_bin), 100)){
  table1 <-  ggtexttable(iv_table[i:min((i+49),nrow(iv_table)),], rows = NULL, theme = ttheme("lBlueWhite"))
  table2 <-  ggtexttable(iv_table[(i+50):min((i+99),nrow(iv_table)),], rows = NULL, theme = ttheme("lBlueWhite"))
  print(ggarrange(table1, table2, 
                  ncol = 2, nrow = 1, heights = c(0.1, 0.1)))
}

dev.off()

rm(table1)
rm(table2)

pdf_name <- 'Statistic_of_variables_3'
pdf_count<-20
k <- 1
# loop, that creates statistics for every column 
for (i in seq(1,(bin_ncol-variable_fc_bin), pdf_count))
{
  print(paste(pdf_name, "_part", ((i-1)/pdf_count)+1, ".pdf", sep = ''))
  pdf(paste(pdf_name, "_part", ((i-1)/pdf_count)+1, ".pdf", sep = ''),width = 16, height=14, paper='special')
  for (j in (variable_fc_bin+i-1):min(variable_fc_bin+i+pdf_count-2,bin_ncol)) {
    
    print(paste(k, j, names(bin_data)[j]))
    
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
    k <- k+1
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
                    ncol = 1, nrow = 5,heights = c(0.08, 0.04, 0.04, 0.3, 0.2)))
    
  }
  
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
rm(i)
rm(ia)
rm(j)
rm(p_value_chisq)
rm(table)
rm(text1)
rm(text2)
rm(text3)
rm(title1)
rm(title2)
rm(title3)
rm(var_for_group)
rm(pdf_name)
rm(pdf_name_init)
rm(pdf_count)

rm(pp)
rm(plot1_hist)
rm(plot2_BR_line)

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
pdf("CorrPlot_4_3.pdf",width = 25,height=25,paper='special')

corrplot(cor_table, method="number",type = "upper",tl.col = "black",diag = FALSE)

corrplot(cor_table, method="color", 
         diag=FALSE, 
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         insig = "blank" 
)

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


sample_score <- as.data.table(dbGetQuery(con_d, "select id_order, target_60max12m, appl_score, bur_score 
                                         from risk_test.dbo.Scoring_test_table where sample_type in ('dev')" ))
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
