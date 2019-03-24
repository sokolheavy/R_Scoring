library(dplyr)

variable_fc_bin <- 2
target_calc_bin <- match("target_for_calc",names(bin_data))
bin_data <- work_data
bin_ncol<-ncol(bin_data)

## IV - statistic table
iv_func <- function(variable_fc_bin, bin_ncol, bin_data){
  iv_table <- arrange(cbind.data.frame(variables = names(bin_data[variable_fc_bin:bin_ncol])
                                       ,IV = sapply(bin_data[variable_fc_bin:bin_ncol], function(x) round(IV(X=x, Y=bin_data$target_for_calc)[1],4)), row.names = NULL),
                      desc(IV))
  
  
  # Add strength of variables
  iv_table$Strength <- ifelse(iv_table$IV>=1, "Suspicious",
                              ifelse(iv_table$IV>=.5, "Very strong",
                                     ifelse(iv_table$IV>=.2, "Strong",
                                            ifelse(iv_table$IV>=.1, "Average",
                                                   ifelse(iv_table$IV>=.02, "Weak", "Wery weak")))))
  iv_table
}







# select only bining data (remove column with 'BR') and sorting columns by IV

data_woe <- select(bin_data, c(target_for_calc, 
                               match(iv_table$variables, names(bin_data)))) %>%
  select(-(grep("BR", names(bin_data))-target_calc_bin+1))

######### doesn't matter
bin_data[,1] <- ifelse(bin_data[,1]==2,1,0)

# create 'woe_table'. It consists of 2 column("WOE" + name_of_variables, WOE_value)
bin_data[,1] <-  as.integer(bin_data[,1])
i <- 2
##########

woe_list = list()
for (i in 2:ncol(data_woe)){
  Good <- sum(data_woe[,1])
  Bad <- nrow(data_woe) - Good
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


###
# create score
###

bin_data <- work_data
measurevar <- "target_for_calc"
model <- glm(as.formula(paste(measurevar,paste(names(bin_data)[-1], collapse=" + "), sep=" ~ ")), bin_data, family = "binomial") 

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
      if (grepl(names(woe_list[[j]][1]), names(model[["coefficients"]])[i]))
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







####### 
# Create sql code for binning data 
####### 

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











