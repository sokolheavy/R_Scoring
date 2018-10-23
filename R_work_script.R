#подключение к ms server
#install.packages("odbc")
library("odbc")
library(data.table)
con <- dbConnect(odbc(),Driver = "SQL Server",Server = "...",Database = "...",trusted_connection=TRUE,Port = 1433)
sample_table1 <- as.data.table(dbGetQuery(con, "select ... "))

#join таблиц
total_sample<-merge(x=sample_table1,y=sample_table2,by=c("dealno","contractsubject"))
rm(sample_table1)
rm(sample_table2)
gc()

total_sample<-as.data.table(total_sample)
setkey(total_sample, dealno, contractsubject)
setkey(sample_table2,dealno,contractsubject)
total_sample<-total_sample[sample_table2, nomatch = 0]
rm(sample_table2)
gc()

#удаление перменных
sample_client_parms[,client_position:=NULL]
total_sample<-as.data.frame(total_sample) 
total_sample$client_position <- NULL
total_sample[81:146]<-NULL

#заменяем NA на значения
total_sample[is.na(total_sample$Client_mobphone_number), 'Client_mobphone_number'] <- -99


n1<- match("Days_from_first_loan_all_fromall",names(total_sample))
n2<- match("Days_from_last_closedloan_C_fromnotbanks",names(total_sample))
for (i in c(n1:n2))
{ total_sample[is.na(total_sample[i]), i] <- -99
}

#уникальные переменные
unique(total_sample$Client_education)

#расчет iv
library(devtools)
install_github("riv","tomasgreif")
library(woe)

options(digits = 2)
iv.mult(as.data.frame(total_sample),"Bad_ff",vars=c("Test_cat_km"))

options(digits = 7)
iv.mult(total_sample[c(13,15:22)],"Bad_ff",TRUE)
iv.mult(total_sample[c(13,15:22)],"Bad_ff")

# Load library and its dataset
library(smbinning) # Load package and its data

# Example: Optimal binning
result=smbinning(df=total_sample,y="Bad_ff",x="Client_age") # Run and save result
result$ivtable # Tabulation and Information Value
result$iv # Information value
result$bands # Bins or bands
result$ctree # Decision tree

smbinning.factor(total_sample,"Bad_ff", "Client_dti_cat")

#категоризация
t<-total_sample$Client_age

total_sample$Client_age_cat1<-cut(t, c(min(t),0,5,10,15,20,25,30, max(t)),include.lowest = TRUE, right = FALSE)



rm(t)

d<-total_sample[c(20)]

library(Hmisc)
table(cut2(t, m = length(t)/4))

library(classInt)
install.packages("classInt")
classIntervals(t, 4, style = 'quantile')

t<-total_sample$Req_cnt_12m_all
eq<-classIntervals(t, 5, style = 'quantile')
km<-classIntervals(t, 4, style = 'kmeans')
sb=smbinning(df=total_sample,y="Bad_ff",x="Req_cnt_12m_all")
total_sample$Test_cat_eq<-as.factor(gsub(",", ";",cut(t, eq$brks,include.lowest = TRUE, right = FALSE)))
total_sample$Test_cat_eq<-as.factor(gsub(",", ";",cut(t, c(min(t)-1,unique(eq$brks)),include.lowest = TRUE, right = FALSE)))
total_sample$Test_cat_km<-as.factor(gsub(",", ";",cut(t, km$brks,include.lowest = TRUE, right = FALSE)))
total_sample$Test_cat_km<-as.factor(gsub(",", ";",cut(t, km$brks[c(1:3,5)],include.lowest = TRUE, right = FALSE)))
total_sample$Test_cat_sb<-as.factor(gsub(",", ";",cut(t, sb$bands,include.lowest = TRUE, right = FALSE)))
total_sample$Test_cat_sb<-as.factor(gsub(",", ";",cut(t, c(min(t)-1,unique(sb$bands)), right = TRUE, left = FALSE)))


#частота и гистограмма
rec_c<-length(total_sample$Bad_ff)
rec_c_proc<-length(total_sample$Bad_ff)/100
table(total_sample$Test_cat_eq)/rec_c_proc
table(total_sample$Test_cat_km)/rec_c_proc
table(total_sample$Test_cat_sb)/rec_c_proc
ggplot(total_sample, aes( x= Test_cat_eq))+geom_bar()
ggplot(total_sample, aes( x= Test_cat_km))+geom_bar()
ggplot(total_sample, aes( x= Test_cat_sb))+geom_bar()

#сохраняем успешные категории
getwd()
setwd("C:/Users/Nachos/Desktop/work_SCORING/rdata")
save(eq, km, sb, file = "Client_workphone_number.rda")
rm(eq)
rm(km)
rm(sb)
load(file = "Client_workphone_number.rda")

#используем сохраненные категории
t<-total_sample$Client_age
setwd("C:/Users/Nachos/Desktop/work_SCORING/rdata")
load(file = "Client_age.rda")
total_sample$Client_age_cat_eq<-as.factor(gsub(",", ";",cut(t, eq$brks,include.lowest = TRUE, right = FALSE)))
total_sample$Client_age_cat_km<-as.factor(gsub(",", ";",cut(t, km$brks,include.lowest = TRUE, right = FALSE)))
total_sample$Client_age_cat_sb<-as.factor(gsub(",", ";",cut(t, sb$bands,include.lowest = TRUE, right = FALSE)))
rm(eq)
rm(km)
rm(sb)
gc()

#просмотреть статистику
for (i in c(43:length(total_sample))) 
  { print(names(total_sample[i]))
  print(table(total_sample[i]))
  print(table(total_sample[i])/rec_c_proc)
  x<-factor(total_sample[,i])
  print(ggplot(total_sample, aes(x))+geom_bar())
  }

#разбивка на категории циклом
for (i in c(81:116)) 
{ 
  print(names(total_sample[i]), quote = FALSE)
  tt<-factor(total_sample[,i])
  t<-total_sample[,i]
  
  print('calculating equal method...', quote = FALSE)
  eq<-classIntervals(t, 5, style = 'quantile')
  total_sample$Test_cat_eq<-cut(t, c(min(t)-1,unique(eq$brks)),include.lowest = TRUE, right = FALSE, ordered = TRUE)
  levels(total_sample$Test_cat_eq)<-gsub(",", ";", levels(total_sample$Test_cat_eq))
  print(table(total_sample$Test_cat_eq)/rec_c_proc)
  iv_stat<-iv.mult(total_sample[c(13,length(total_sample))],"Bad_ff",TRUE)
  #print(ggplot(total_sample, aes( x= Test_cat_eq))+geom_bar()+xlab(gsub('Test', names(total_sample[i]),"Test_cat_eq")))
  print(ggplot(total_sample, aes( x= Test_cat_eq))+geom_bar(aes(y=..count../sum(..count..)), fill = "steelblue4")+ labs(title = paste(gsub('Test', names(total_sample[i]), "Test_cat_eq"), ": ", iv_stat$Strength[iv_stat$Variable == "Test_cat_eq"],", iv = ", round(iv_stat$InformationValue[iv_stat$Variable == "Test_cat_eq"],3)), x = "") + theme_minimal()+ geom_text(aes( label = scales::percent(..count../sum(..count..)), y= ..count../sum(..count..) ), stat= "count", vjust=-.5))
  print('', quote = FALSE)
  
  print('calculating kmeans method...', quote = FALSE)
  interval_count<-6
  repeat{
  print(paste('number of interval -', interval_count), quote = FALSE)
  km<-classIntervals(t, interval_count, style = 'kmeans')
  total_sample$Test_cat_km<-cut(t, km$brks,include.lowest = TRUE, right = FALSE, ordered = TRUE)
  levels(total_sample$Test_cat_km)<-gsub(",", ";",levels(total_sample$Test_cat_km))
  print(table(total_sample$Test_cat_km)/rec_c_proc)
  iv_stat<-iv.mult(total_sample[c(13,length(total_sample))],"Bad_ff",TRUE)
  print('', quote = FALSE)
  interval_count<-interval_count-1
  if (min(table(total_sample$Test_cat_km)/rec_c_proc)>3) {break}
  if (interval_count<2) {break}
  }
  #print(ggplot(total_sample, aes( x= Test_cat_km))+geom_bar()+xlab(gsub('Test', names(total_sample[i]),"Test_cat_km")))
  print(ggplot(total_sample, aes( x= Test_cat_km))+geom_bar(aes(y=..count../sum(..count..)), fill = "steelblue4")+ labs(title = paste(gsub('Test', names(total_sample[i]), "Test_cat_km"), ": ", iv_stat$Strength[iv_stat$Variable == "Test_cat_km"],", iv = ", round(iv_stat$InformationValue[iv_stat$Variable == "Test_cat_km"],3)), x = "") + theme_minimal()+ geom_text(aes( label = scales::percent(..count../sum(..count..)), y= ..count../sum(..count..) ), stat= "count", vjust=-.5))
  
  
  print('calculating smbinning method...', quote = FALSE)
  sb=smbinning(df=total_sample,y="Bad_ff",x=names(total_sample[i]))
  total_sample$Test_cat_sb<-cut(t, c(min(t)-1,unique(sb$bands)), right = TRUE, left = FALSE, ordered = TRUE)
  levels(total_sample$Test_cat_sb)<-gsub(",", ";",levels(total_sample$Test_cat_sb))
  print(table(total_sample$Test_cat_sb)/rec_c_proc)
  iv_stat<-iv.mult(total_sample[c(13,length(total_sample))],"Bad_ff",TRUE)
  #print(ggplot(total_sample, aes( x= Test_cat_sb))+geom_bar()+xlab(gsub('Test', names(total_sample[i]),"Test_cat_sb")))
  print(ggplot(total_sample, aes( x= Test_cat_sb))+geom_bar(aes(y=..count../sum(..count..)), fill = "steelblue4")+ labs(title = paste(gsub('Test', names(total_sample[i]), "Test_cat_sb"), ": ", iv_stat$Strength[iv_stat$Variable == "Test_cat_sb"],", iv = ", round(iv_stat$InformationValue[iv_stat$Variable == "Test_cat_sb"],3)), x = "") + theme_minimal()+ geom_text(aes( label = scales::percent(..count../sum(..count..)), y= ..count../sum(..count..) ), stat= "count", vjust=-.5))
  print('', quote = FALSE)
  
  names(total_sample)[names(total_sample) == "Test_cat_eq"] <- gsub('Test', names(total_sample[i]),"Test_cat_eq")
  names(total_sample)[names(total_sample) == "Test_cat_km"] <- gsub('Test', names(total_sample[i]),"Test_cat_km")
  names(total_sample)[names(total_sample) == "Test_cat_sb"] <- gsub('Test', names(total_sample[i]),"Test_cat_sb")

  setwd("C:/Users/Nachos/Desktop/work_SCORING/rdata")
  save(eq, km, sb, file = paste(names(total_sample[i]),".rda", sep = ""))
  #rm(eq)
  #rm(km)
  #rm(sb)
}

#Используем сохраненные категории циклом
n1<- match("Req_cnt_3m_all",names(total_sample))
n2<- match("Days_from_last_closedloan_C_fromnotbanks",names(total_sample))

setwd("C:/Users/Nachos/Desktop/work_SCORING/rdata")
for (i in c(n1:n2)) 
{ 
  print(names(total_sample[i]), quote = FALSE)
  t<-total_sample[,i]
  
  if (file.exists(paste(names(total_sample[i]),".rda", sep = "")))
  {
    load(file = paste(names(total_sample[i]),".rda", sep = ""))
    
    total_sample$Test_cat_eq<-cut(t, c(min(t)-1,unique(eq$brks)),include.lowest = TRUE, right = FALSE, ordered = TRUE)
    total_sample$Test_cat_km<-cut(t, km$brks,include.lowest = TRUE, right = FALSE, ordered = TRUE)
    total_sample$Test_cat_sb<-cut(t, c(min(t)-1,unique(sb$bands)), right = TRUE, left = FALSE, ordered = TRUE)
    
    levels(total_sample$Test_cat_eq)<-gsub(",", ";", levels(total_sample$Test_cat_eq))
    levels(total_sample$Test_cat_km)<-gsub(",", ";",levels(total_sample$Test_cat_km))
    levels(total_sample$Test_cat_sb)<-gsub(",", ";",levels(total_sample$Test_cat_sb))
  
    rm(eq)
    rm(km)
    rm(sb)
    
    names(total_sample)[names(total_sample) == "Test_cat_eq"] <- gsub('Test', names(total_sample[i]),"Test_cat_eq")
    names(total_sample)[names(total_sample) == "Test_cat_km"] <- gsub('Test', names(total_sample[i]),"Test_cat_km")
    names(total_sample)[names(total_sample) == "Test_cat_sb"] <- gsub('Test', names(total_sample[i]),"Test_cat_sb")
    
    print("...process ok...", quote = FALSE)
  }
  else {print("...file does not exist...", quote = FALSE)}
}

#строим графики и статистику
n2<- length(total_sample)
n1<- match("Days_from_first_loan_C_frombanks_cat_sb",names(total_sample))
n2<-n1
for (i in c(n1:n2)) 
{
  print(names(total_sample[i]), quote = FALSE)
  tt<-factor(total_sample[,i])
  t<-total_sample[,i]
  
  print(table(total_sample[,i])/rec_c_proc)
  iv_stat<-iv.mult(total_sample[c(13,i)],"Bad_ff",TRUE)
   print(ggplot(total_sample, aes( x= t))+geom_bar(aes(y=..count../sum(..count..)), fill = "steelblue4")+ labs(title = paste(names(total_sample[i]), ": ", iv_stat$Strength[iv_stat$Variable == names(total_sample[i])],", iv = ", round(iv_stat$InformationValue[iv_stat$Variable == names(total_sample[i])],3)), x = "") + theme_minimal()+ geom_text(aes( label = scales::percent(..count../sum(..count..)), y= ..count../sum(..count..) ), stat= "count", vjust=-.5))
  print('', quote = FALSE)
  
}


iv_stat<-iv.mult(total_sample[c(13,45:length(total_sample))],"Bad_ff",TRUE)

ggplot(total_sample, aes( x= Req_cnt_3m_all_cat_sb))+geom_bar(aes(y=..count../sum(..count..)), fill = "steelblue4")+ labs(title = paste(gsub('Test', 'Req_cnt_3m_all',"Test_cat_sb"), ": ", iv_stat$Strength[iv_stat$Variable == "Req_cnt_3m_all_cat_sb"],", iv = ", round(iv_stat$InformationValue[iv_stat$Variable == "Req_cnt_3m_all_cat_sb"],3)), x = "") + theme_minimal()+ geom_text(aes( label = scales::percent(..count../sum(..count..)), y= ..count../sum(..count..) ), stat= "count", vjust=-.5)+ geom_line(aes(x=Req_cnt_3m_all_cat_sb, y=sum(Bad_ff)))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        

total_sample[45:47]<-NULL

# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/



#----------------------example for work with sample---------------------

rec_c<-length(total_sample$Bad_ff)
rec_c_proc<-length(total_sample$Bad_ff)/100

#Удаляем пустые переменные
total_sample<-as.data.frame(total_sample) 
total_sample$client_position <- NULL
total_sample$Client_dependantsnumber <- NULL

#заменяем NA на значения
total_sample[is.na(total_sample$Client_mobphone_number), 'Client_mobphone_number'] <- -99
total_sample[is.na(total_sample$Client_workphone_number), 'Client_workphone_number'] <- -99
total_sample[is.na(total_sample$Client_homephone_number), 'Client_homephone_number'] <- -99
total_sample[is.na(total_sample$negative_status), 'negative_status'] <- -99

n1<- match("Days_from_first_loan_all_fromall",names(total_sample))
n2<- match("Days_from_last_closedloan_C_fromnotbanks",names(total_sample))
for (i in c(n1:n2))
{ total_sample[is.na(total_sample[i]), i] <- -99
}


#создаем категории
total_sample$Client_gender_cat <- as.factor(total_sample$Client_gender)
total_sample$Client_residency_cat <- as.factor(total_sample$Client_residency)
total_sample$Client_classification_cat <- as.factor(total_sample$Client_classification)
total_sample$Client_education_cat<-as.factor(total_sample$Client_education)
total_sample$Client_marutalstatus_cat<-as.factor(total_sample$Client_marutalstatus)
total_sample$Client_email_number_cat<-as.factor(total_sample$Client_email_number)
total_sample$negative_status_cat<-as.factor(total_sample$negative_status)

total_sample$Client_workexperience_cat<-as.factor(gsub(",", ";",cut(total_sample$Client_workexperience, c(-10000000,-100,1401))))
total_sample$Client_monthlyincome_cat<-as.factor(gsub(",", ";",cut(total_sample$Client_monthlyincome, c(-10000000,-1,0,10e+08))))

t<-total_sample$Client_age
setwd("C:/Users/Nachos/Desktop/work_SCORING/rdata")
load(file = "Client_age.rda")
total_sample$Client_age_cat_eq<-as.factor(gsub(",", ";",cut(t, eq$brks,include.lowest = TRUE, right = FALSE)))
total_sample$Client_age_cat_km<-as.factor(gsub(",", ";",cut(t, km$brks,include.lowest = TRUE, right = FALSE)))
total_sample$Client_age_cat_sb<-as.factor(gsub(",", ";",cut(t, sb$bands,include.lowest = TRUE, right = FALSE)))
rm(eq)
rm(km)
rm(sb)

total_sample$Client_dti_cat<-as.factor(gsub(",", ";",cut(total_sample$Client_dti, c(-10000000,-1,7160183.4),include.lowest = TRUE, right = FALSE)))

t<-total_sample$Client_mobphone_number
setwd("C:/Users/Nachos/Desktop/work_SCORING/rdata")
load(file = "Client_mobphone_number.rda")
total_sample$Client_mobphone_number_cat_eq<-as.factor(gsub(",", ";",cut(t, eq$brks[c(2:4)],include.lowest = TRUE, right = FALSE)))
total_sample$Client_mobphone_number_cat_km<-as.factor(gsub(",", ";",cut(t, km$brks,include.lowest = TRUE, right = FALSE)))
total_sample$Client_mobphone_number_cat_sb<-as.factor(gsub(",", ";",cut(t, c(-100,sb$bands[2:length(sb$bands)]),include.highest = TRUE, right = TRUE, left = FALSE)))
rm(eq)
rm(km)
rm(sb)
gc()

t<-total_sample$Client_homephone_number
setwd("C:/Users/Nachos/Desktop/work_SCORING/rdata")
load(file = "Client_homephone_number.rda")
total_sample$Client_homephone_number_cat_eq<-as.factor(gsub(",", ";",cut(t, eq$brks[c(2:4)],include.lowest = TRUE, right = FALSE)))
total_sample$Client_homephone_number_cat_km<-as.factor(gsub(",", ";",cut(t, km$brks,include.lowest = TRUE, right = FALSE)))
total_sample$Client_homephone_number_cat_sb<-as.factor(gsub(",", ";",cut(t, c(-100,sb$bands[2:length(sb$bands)]),include.highest = TRUE, right = TRUE, left = FALSE)))
rm(eq)
rm(km)
rm(sb)
gc()

t<-total_sample$Client_workphone_number
setwd("C:/Users/Nachos/Desktop/work_SCORING/rdata")
load(file = "Client_workphone_number.rda")
total_sample$Client_workphone_number_cat_eq<-as.factor(gsub(",", ";",cut(t, eq$brks[c(2:4)],include.lowest = TRUE, right = FALSE)))
total_sample$Client_workphone_number_cat_km<-as.factor(gsub(",", ";",cut(t, km$brks[c(1:3,5)],include.lowest = TRUE, right = FALSE)))
total_sample$Client_workphone_number_cat_sb<-as.factor(gsub(",", ";",cut(t, c(-100,sb$bands[2:length(sb$bands)]),include.highest = TRUE, right = TRUE, left = FALSE)))
rm(eq)
rm(km)
rm(sb)
gc()

n1<- match("Req_cnt_3m_all",names(total_sample))
n2<- match("Days_from_last_closedloan_C_fromnotbanks",names(total_sample))

setwd("C:/Users/Nachos/Desktop/work_SCORING/rdata")
for (i in c(n1:n2)) 
{ 
  print(names(total_sample[i]), quote = FALSE)
  t<-total_sample[,i]
  
  if (file.exists(paste(names(total_sample[i]),".rda", sep = "")))
  {
    load(file = paste(names(total_sample[i]),".rda", sep = ""))
    total_sample$Test_cat_eq<-cut(t, c(min(t)-1,unique(eq$brks)),include.lowest = TRUE, right = FALSE, ordered = TRUE)
    total_sample$Test_cat_km<-cut(t, km$brks,include.lowest = TRUE, right = FALSE, ordered = TRUE)
    total_sample$Test_cat_sb<-cut(t, c(min(t)-1,unique(sb$bands)), right = TRUE, left = FALSE, ordered = TRUE)
    
    levels(total_sample$Test_cat_eq)<-gsub(",", ";", levels(total_sample$Test_cat_eq))
    levels(total_sample$Test_cat_km)<-gsub(",", ";",levels(total_sample$Test_cat_km))
    levels(total_sample$Test_cat_sb)<-gsub(",", ";",levels(total_sample$Test_cat_sb))
    
    
    rm(eq)
    rm(km)
    rm(sb)
    
    names(total_sample)[names(total_sample) == "Test_cat_eq"] <- gsub('Test', names(total_sample[i]),"Test_cat_eq")
    names(total_sample)[names(total_sample) == "Test_cat_km"] <- gsub('Test', names(total_sample[i]),"Test_cat_km")
    names(total_sample)[names(total_sample) == "Test_cat_sb"] <- gsub('Test', names(total_sample[i]),"Test_cat_sb")
    
    print("...process ok...", quote = FALSE)
  }
  else {print("...file does not exist...", quote = FALSE)}
}

#удаляем тестовые колонки
total_sample$Test_cat_eq<- NULL
total_sample$Test_cat_km<- NULL
total_sample$Test_cat_sb<- NULL

#удаляем оригинальные колонки
n1<- match("Bad_ff_def",names(total_sample))+1
n2<- length(names(total_sample))
for (i in c(n2:n1))
{ 
  print(paste(i, names(total_sample[i])), quote = FALSE)
  if (length(grep(paste(names(total_sample[i]), "_cat", sep = ""), names(total_sample))) > 0)
  {total_sample[i] <- NULL
  print("delete", quote = FALSE)
  }
  else
  {print("skip", quote = FALSE)}
}

#удаляем переменные с низким iv
total_sample$Client_residency_cat <- NULL
total_sample$Client_gender_cat <- NULL
total_sample$Client_classification_cat <- NULL
total_sample$Client_workexperience_cat <- NULL

#строим графики и статистику
library(devtools)
library(woe)
library(ggplot2)
n2<- length(total_sample)
n1<- min(grep("_cat", names(total_sample)))
for (i in c(n1:n2)) 
{
  print(names(total_sample[i]), quote = FALSE)
  tt<-factor(total_sample[,i])
  t<-total_sample[,i]
  
  print(table(total_sample[,i])/rec_c_proc)
  iv_stat<-iv.mult(total_sample[c(13,i)],"Bad_ff",TRUE)
  print(ggplot(total_sample, aes( x= t))+geom_bar(aes(y=..count../sum(..count..)), fill = "steelblue4")+ labs(title = paste(names(total_sample[i]), ": ", iv_stat$Strength[iv_stat$Variable == names(total_sample[i])],", iv = ", round(iv_stat$InformationValue[iv_stat$Variable == names(total_sample[i])],3)), x = "") + theme_minimal()+ geom_text(aes( label = scales::percent(..count../sum(..count..)), y= ..count../sum(..count..) ), stat= "count", vjust=-.5))
  print('', quote = FALSE)
  
}

c<-c(grep("Bad_ff", names(total_sample)),grep("_cat", names(total_sample)))
total_sample_cat<-total_sample[c]
iv.mult(total_sample_cat[c(1,3:length(total_sample_cat))],"Bad_ff",TRUE)
