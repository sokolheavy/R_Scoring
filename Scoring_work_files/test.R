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

setwd("S:/PROJECTS/SCORING/2018.10.31 Scoring CC/CC_no_CSF_appl_beh_alfa")
#setwd("H:/Документы/Работа_проекты/Scoring CC_no_CSF/Scoring CC_no_CSF appl beh alfa")
################ Get sample

#setwd("S:/PROJECTS/SCORING/0. Scorecard_Creation/R scripts")
#set data for binning from Csv
#work_data <- read.csv2("cc_no_csf_else_bin.csv")

con_k <- dbConnect(odbc(),Driver = "SQL Server",Server = "khazardbp02\\hazard",Database = "Risk_test",trusted_connection=TRUE)
con_d <- dbConnect(odbc(),Driver = "SQL Server",Server = "dhazardbp01\\hazard",Database = "Risk_test",trusted_connection=TRUE)

sample_base <- as.data.table(dbGetQuery(con_k, "select * from risk_test.dbo.Scoring_DataMart_CC_base where productgroup = 'CC' and sample_type in ('dev') and client_type = 'alfa'"))
sample_base_dev1 <- as.data.table(dbGetQuery(con_k, "select top 20415 * from risk_test.dbo.Scoring_DataMart_CC_base where productgroup = 'CC' and sample_type in ('dev') and client_type = 'alfa'"))
sample_base_dev2 <- as.data.table(dbGetQuery(con_k, "select top 20415 * from risk_test.dbo.Scoring_DataMart_CC_base where productgroup = 'CC' and sample_type in ('dev') and client_type = 'alfa' ORDER BY dealdate DESC"))
sample_base_test <- as.data.table(dbGetQuery(con_k, "select * from risk_test.dbo.Scoring_DataMart_CC_base where productgroup = 'CC' and sample_type in ('test') and client_type = 'alfa'"))
sample_base_random <- as.data.table(dbGetQuery(con_k, "select top 20415 * from risk_test.dbo.Scoring_DataMart_CC_base where productgroup = 'CC' and sample_type in ('dev') and client_type = 'alfa' ORDER BY NEWID()"))

sample_appl <- as.data.table(dbGetQuery(con_d, "select t.* from risk_test.dbo.Scoring_DataMart_CC_appl as t join risk_test.dbo.Scoring_DataMart_CC_base as b on t.id_order = b.id_order where target_60max12m in ('good', 'bad')"))
sample_beh<- as.data.table(dbGetQuery(con_k, "select t.* from risk_test.dbo.Scoring_DataMart_CC_beh as t join risk_test.dbo.Scoring_DataMart_CC_base as b on t.id_order = b.id_order where target_60max12m in ('good', 'bad')"))

#join table - create all sample of data if you need
setkey(sample_base, id_order)
setkey(sample_base_dev1, id_order)
setkey(sample_base_dev2, id_order)
setkey(sample_base_test, id_order)
setkey(sample_base_random, id_order)
setkey(sample_appl, id_order)


total_sample<-sample_base[sample_appl, nomatch = 0]
total_sample_dev1<-sample_base_dev1[sample_appl, nomatch = 0]
total_sample_dev2<-sample_base_dev2[sample_appl, nomatch = 0]
total_sample_test<-sample_base_test[sample_appl, nomatch = 0]
total_sample_random <-sample_base_random[sample_appl, nomatch = 0]


glimpse(sample_base)

setkey(sample_beh, id_order)
total_sample<-total_sample[sample_beh, nomatch = 0]
total_sample_dev1<-total_sample_dev1[sample_beh, nomatch = 0]
total_sample_dev2<-total_sample_dev2[sample_beh, nomatch = 0]
total_sample_test<-total_sample_test[sample_beh, nomatch = 0]
total_sample_random<-total_sample_random[sample_beh, nomatch = 0]

rm(sample_base)
rm(sample_appl)
rm(sample_beh)

save(total_sample, file = "total_sample_3.rda")
load(file = 'total_sample_3.rda')

#for work with specified scoring variables you can choose columns from total_sample or create it in other way
work_data<-as.data.frame(total_sample[, c(1:14, 77:260)])
work_data_dev1<-as.data.frame(total_sample_dev1[, c(1:14, 77:260)])
work_data_dev2<-as.data.frame(total_sample_dev2[, c(1:14, 77:260)])
work_data_test<-as.data.frame(total_sample_test[, c(1:14, 77:260)])
work_data_random<-as.data.frame(total_sample_random[, c(1:14, 77:260)])

# Add 'target_for_calc'(1,0), if it doesn't exist
work_data <- add_column(work_data, 
                        target_for_calc = ifelse(work_data$target_60max12m == "good", 1,0), 
                        .after = "target_60max12m")

work_data_dev1 <- add_column(work_data_dev1, 
                        target_for_calc = ifelse(work_data_dev1$target_60max12m == "good", 1,0), 
                        .after = "target_60max12m")

work_data_dev2 <- add_column(work_data_dev2, 
                        target_for_calc = ifelse(work_data_dev2$target_60max12m == "good", 1,0), 
                        .after = "target_60max12m")

work_data_test <- add_column(work_data_test, 
                        target_for_calc = ifelse(work_data_test$target_60max12m == "good", 1,0), 
                        .after = "target_60max12m")

work_data_random <- add_column(work_data_random, 
                        target_for_calc = ifelse(work_data_random$target_60max12m == "good", 1,0), 
                        .after = "target_60max12m")





work_data <- add_column(work_data, 
                        dti_clear = (work_data$Payments_in_alfa+work_data$Payments_in_banks_total)/work_data$Total_Income, 
                        .after = "dti")


work_data<-work_data[complete.cases(work_data[,142]),]

work_data<-work_data[work_data$status == 1,]

names(work_data)[names(work_data) %like% "#_"] 

names(work_data) <- gsub("#_", "count_", names(work_data))

names(work_data)[names(work_data) %like% "count_"] 


work_data <- select(work_data, c(id_order,
                                  dealdate,
                                  decisiondate,
                                  inn,
                                  subgroup,
                                  target_for_calc,
                                  Min_month_from_date_begin,
                                  sum_amountbegin_all_loans_CSF,
                                  Work_type,
                                  Relationship_with_contact_person,
                                  MaxCC_usage_ever,
                                  Age_y,
                                  Monthly_charges,
                                  Current_work_experience_in_years,
                                  MaxCC_out_12m,
                                  Max_dpd_ever,
                                  Education,
                                  out_to_amountmax,
                                  Max_month_from_date_begin,
                                  Relationship_with_contact_person,
                                  Month_since_del_more_then_60,
                                  max_amountmax_all_loans,
                                  MaxCC_usage_1m,
                                  max_amountbegin_all_loans_CC))

##################### work data dev1 ############################


work_data_dev1[is.na(work_data_dev1$Number_of_dependants), ]
names(work_data_dev1)
i<-43
work_data_dev1<-work_data_dev1[complete.cases(work_data_dev1[,i]),]
print(names(work_data_dev1[i]))
print(table(work_data_dev1[i], useNA = "always"))

work_data_dev1$Time_in_marriage_y <- NULL
work_data_dev1$Time_in_marriage_m <- NULL
work_data_dev1$Registration_term_in_months<-NULL

work_data_dev1[is.na(work_data_dev1$Residing_term_in_months), ]
names(work_data_dev1)
i<-46
work_data_dev1<-work_data_dev1[complete.cases(work_data_dev1[,i]),]
print(names(work_data_dev1[i]))
print(table(work_data_dev1[i], useNA = "always"))

work_data_dev1$Way_of_purchase_of_habitation <- NULL

work_data_dev1[is.na(work_data_dev1$Add_Income_from_UW), ]
names(work_data_dev1)
work_data_dev1[is.na(work_data_dev1$Add_Income_from_UW), 61] <- 0
work_data_dev1[is.na(work_data_dev1$Add_Income_from_UW), ]

work_data_dev1[is.na(work_data_dev1$Payments_in_banks_from_client), ]
names(work_data_dev1)
work_data_dev1[is.na(work_data_dev1$Payments_in_banks_from_client), 65] <- 0
work_data_dev1[is.na(work_data_dev1$Payments_in_banks_from_client), ]

work_data_dev1[is.na(work_data_dev1$Payments_in_banks_UW), ]
work_data_dev1[is.na(work_data_dev1$Payments_in_banks_total), ]
names(work_data_dev1)
work_data_dev1[is.na(work_data_dev1$Payments_in_banks_total), 67] <- 0
work_data_dev1[is.na(work_data_dev1$Payments_in_banks_total), ]

work_data_dev1[is.na(work_data_dev1$Payments_in_alfa), ]
names(work_data_dev1)
work_data_dev1[is.na(work_data_dev1$Payments_in_alfa), 68] <- 0
work_data_dev1[is.na(work_data_dev1$Payments_in_alfa), ]

work_data_dev1$Loan_amount_in_banks_from_client<-NULL
work_data_dev1$Use_of_credits_last_5_years <- NULL

work_data_dev1$Requested_monthly_Payment <- NULL
work_data_dev1$Requested_loan_amount <- NULL
work_data_dev1$Requested_contract_amount <- NULL
work_data_dev1$Requested_loan_Initial_cash_payment<-NULL
work_data_dev1$Requested_loan_term <- NULL

work_data_dev1[is.na(work_data_dev1$di), ]
names(work_data_dev1)
work_data_dev1<-work_data_dev1[complete.cases(work_data_dev1[,69]),]

work_data_dev1[is.na(work_data_dev1$dti), ]

work_data_dev1$i.inn <- NULL

work_data_dev1 <- add_column(work_data_dev1, 
                        dti_clear = (work_data_dev1$Payments_in_alfa+work_data_dev1$Payments_in_banks_total)/work_data_dev1$Total_Income, 
                        .after = "dti")

work_data_dev1[is.na(work_data_dev1$dti_clear), ]

work_data_dev1[is.na(work_data_dev1$sum_amountbegin_close_loans), ]

work_data_dev1[is.na(work_data_dev1$Sum_cur_out), ]

work_data_dev1[is.na(work_data_dev1$Max_dpd_ever), ]
names(work_data_dev1)
work_data_dev1<-work_data_dev1[complete.cases(work_data_dev1[,142]),]
table(work_data_dev1$Max_dpd_ever)

work_data_dev1[(work_data_dev1$Max_dpd_ever == 0) & (work_data_dev1$max_dpd_then_pay_all_payment_ever != 0),]

work_data_dev1<-work_data_dev1[work_data_dev1$status == 1,]

names(work_data_dev1)[names(work_data_dev1)=='Missed_payment_ever,%']<- 'Missed_payment_ever_proc'
names(work_data_dev1)[names(work_data_dev1)=='Missed_payment_12m,%']<- 'Missed_payment_12m_proc'
names(work_data_dev1)[names(work_data_dev1)=='Missed_payment_6m,%']<- 'Missed_payment_6m_proc'
names(work_data_dev1)[names(work_data_dev1)=='Missed_payment_3m,%']<- 'Missed_payment_3m_proc'
names(work_data_dev1)[names(work_data_dev1)=='Missed_payment_1m,%']<- 'Missed_payment_1m_proc'

names(work_data_dev1)[names(work_data_dev1) %like% "#_"] 

names(work_data_dev1) <- gsub("#_", "count_", names(work_data_dev1))

names(work_data_dev1)[names(work_data_dev1) %like% "count_"] 



work_data_dev1 <- select(work_data_dev1, c(id_order,
                                 dealdate,
                                 decisiondate,
                                 inn,
                                 subgroup,
                                 target_for_calc,
                                 Min_month_from_date_begin,
                                 sum_amountbegin_all_loans_CSF,
                                 Work_type,
                                 Relationship_with_contact_person,
                                 MaxCC_usage_ever,
                                 Age_y,
                                 Monthly_charges,
                                 Current_work_experience_in_years,
                                 MaxCC_out_12m,
                                 Max_dpd_ever,
                                 Education,
                                 out_to_amountmax,
                                 Max_month_from_date_begin,
                                 Relationship_with_contact_person,
                                 Month_since_del_more_then_60,
                                 max_amountmax_all_loans,
                                 MaxCC_usage_1m,
                                 max_amountbegin_all_loans_CC))

############## work_data_dev2 ############################



work_data_dev2[is.na(work_data_dev2$Number_of_dependants), ]
names(work_data_dev2)
i<-43
work_data_dev2<-work_data_dev2[complete.cases(work_data_dev2[,i]),]
print(names(work_data_dev2[i]))
print(table(work_data_dev2[i], useNA = "always"))

work_data_dev2$Time_in_marriage_y <- NULL
work_data_dev2$Time_in_marriage_m <- NULL
work_data_dev2$Registration_term_in_months<-NULL

work_data_dev2[is.na(work_data_dev2$Residing_term_in_months), ]
names(work_data_dev2)
i<-46
work_data_dev2<-work_data_dev2[complete.cases(work_data_dev2[,i]),]
print(names(work_data_dev2[i]))
print(table(work_data_dev2[i], useNA = "always"))

work_data_dev2$Way_of_purchase_of_habitation <- NULL

work_data_dev2[is.na(work_data_dev2$Add_Income_from_UW), ]
names(work_data_dev2)
work_data_dev2[is.na(work_data_dev2$Add_Income_from_UW), 61] <- 0
work_data_dev2[is.na(work_data_dev2$Add_Income_from_UW), ]

work_data_dev2[is.na(work_data_dev2$Payments_in_banks_from_client), ]
names(work_data_dev2)
work_data_dev2[is.na(work_data_dev2$Payments_in_banks_from_client), 65] <- 0
work_data_dev2[is.na(work_data_dev2$Payments_in_banks_from_client), ]

work_data_dev2[is.na(work_data_dev2$Payments_in_banks_UW), ]
work_data_dev2[is.na(work_data_dev2$Payments_in_banks_total), ]
names(work_data_dev2)
work_data_dev2[is.na(work_data_dev2$Payments_in_banks_total), 67] <- 0
work_data_dev2[is.na(work_data_dev2$Payments_in_banks_total), ]

work_data_dev2[is.na(work_data_dev2$Payments_in_alfa), ]
names(work_data_dev2)
work_data_dev2[is.na(work_data_dev2$Payments_in_alfa), 68] <- 0
work_data_dev2[is.na(work_data_dev2$Payments_in_alfa), ]

work_data_dev2$Loan_amount_in_banks_from_client<-NULL
work_data_dev2$Use_of_credits_last_5_years <- NULL

work_data_dev2$Requested_monthly_Payment <- NULL
work_data_dev2$Requested_loan_amount <- NULL
work_data_dev2$Requested_contract_amount <- NULL
work_data_dev2$Requested_loan_Initial_cash_payment<-NULL
work_data_dev2$Requested_loan_term <- NULL

work_data_dev2[is.na(work_data_dev2$di), ]
names(work_data_dev2)
work_data_dev2<-work_data_dev2[complete.cases(work_data_dev2[,69]),]

work_data_dev2[is.na(work_data_dev2$dti), ]

work_data_dev2$i.inn <- NULL

work_data_dev2 <- add_column(work_data_dev2, 
                             dti_clear = (work_data_dev2$Payments_in_alfa+work_data_dev2$Payments_in_banks_total)/work_data_dev2$Total_Income, 
                             .after = "dti")

work_data_dev2[is.na(work_data_dev2$dti_clear), ]

work_data_dev2[is.na(work_data_dev2$sum_amountbegin_close_loans), ]

work_data_dev2[is.na(work_data_dev2$Sum_cur_out), ]

work_data_dev2[is.na(work_data_dev2$Max_dpd_ever), ]
names(work_data_dev2)
work_data_dev2<-work_data_dev2[complete.cases(work_data_dev2[,142]),]
table(work_data_dev2$Max_dpd_ever)

work_data_dev2[(work_data_dev2$Max_dpd_ever == 0) & (work_data_dev2$max_dpd_then_pay_all_payment_ever != 0),]

work_data_dev2<-work_data_dev2[work_data_dev2$status == 1,]

names(work_data_dev2)[names(work_data_dev2)=='Missed_payment_ever,%']<- 'Missed_payment_ever_proc'
names(work_data_dev2)[names(work_data_dev2)=='Missed_payment_12m,%']<- 'Missed_payment_12m_proc'
names(work_data_dev2)[names(work_data_dev2)=='Missed_payment_6m,%']<- 'Missed_payment_6m_proc'
names(work_data_dev2)[names(work_data_dev2)=='Missed_payment_3m,%']<- 'Missed_payment_3m_proc'
names(work_data_dev2)[names(work_data_dev2)=='Missed_payment_1m,%']<- 'Missed_payment_1m_proc'

names(work_data_dev2)[names(work_data_dev2) %like% "#_"] 

names(work_data_dev2) <- gsub("#_", "count_", names(work_data_dev2))

names(work_data_dev2)[names(work_data_dev2) %like% "count_"] 



work_data_dev2 <- select(work_data_dev2, c(id_order,
                                           dealdate,
                                           decisiondate,
                                           inn,
                                           subgroup,
                                           target_for_calc,
                                           Min_month_from_date_begin,
                                           sum_amountbegin_all_loans_CSF,
                                           Work_type,
                                           Relationship_with_contact_person,
                                           MaxCC_usage_ever,
                                           Age_y,
                                           Monthly_charges,
                                           Current_work_experience_in_years,
                                           MaxCC_out_12m,
                                           Max_dpd_ever,
                                           Education,
                                           out_to_amountmax,
                                           Max_month_from_date_begin,
                                           Relationship_with_contact_person,
                                           Month_since_del_more_then_60,
                                           max_amountmax_all_loans,
                                           MaxCC_usage_1m,
                                           max_amountbegin_all_loans_CC))

###################### work data test #######################


work_data_test[is.na(work_data_test$Number_of_dependants), ]
names(work_data_test)
i<-43
work_data_test<-work_data_test[complete.cases(work_data_test[,i]),]
print(names(work_data_test[i]))
print(table(work_data_test[i], useNA = "always"))

work_data_test$Time_in_marriage_y <- NULL
work_data_test$Time_in_marriage_m <- NULL
work_data_test$Registration_term_in_months<-NULL

work_data_test[is.na(work_data_test$Residing_term_in_months), ]
names(work_data_test)
i<-46
work_data_test<-work_data_test[complete.cases(work_data_test[,i]),]
print(names(work_data_test[i]))
print(table(work_data_test[i], useNA = "always"))

work_data_test$Way_of_purchase_of_habitation <- NULL

work_data_test[is.na(work_data_test$Add_Income_from_UW), ]
names(work_data_test)
work_data_test[is.na(work_data_test$Add_Income_from_UW), 61] <- 0
work_data_test[is.na(work_data_test$Add_Income_from_UW), ]

work_data_test[is.na(work_data_test$Payments_in_banks_from_client), ]
names(work_data_test)
work_data_test[is.na(work_data_test$Payments_in_banks_from_client), 65] <- 0
work_data_test[is.na(work_data_test$Payments_in_banks_from_client), ]

work_data_test[is.na(work_data_test$Payments_in_banks_UW), ]
work_data_test[is.na(work_data_test$Payments_in_banks_total), ]
names(work_data_test)
work_data_test[is.na(work_data_test$Payments_in_banks_total), 67] <- 0
work_data_test[is.na(work_data_test$Payments_in_banks_total), ]

work_data_test[is.na(work_data_test$Payments_in_alfa), ]
names(work_data_test)
work_data_test[is.na(work_data_test$Payments_in_alfa), 68] <- 0
work_data_test[is.na(work_data_test$Payments_in_alfa), ]

work_data_test$Loan_amount_in_banks_from_client<-NULL
work_data_test$Use_of_credits_last_5_years <- NULL

work_data_test$Requested_monthly_Payment <- NULL
work_data_test$Requested_loan_amount <- NULL
work_data_test$Requested_contract_amount <- NULL
work_data_test$Requested_loan_Initial_cash_payment<-NULL
work_data_test$Requested_loan_term <- NULL

work_data_test[is.na(work_data_test$di), ]
names(work_data_test)
work_data_test<-work_data_test[complete.cases(work_data_test[,69]),]

work_data_test[is.na(work_data_test$dti), ]

work_data_test$i.inn <- NULL

work_data_test <- add_column(work_data_test, 
                             dti_clear = (work_data_test$Payments_in_alfa+work_data_test$Payments_in_banks_total)/work_data_test$Total_Income, 
                             .after = "dti")

work_data_test[is.na(work_data_test$dti_clear), ]

work_data_test[is.na(work_data_test$sum_amountbegin_close_loans), ]

work_data_test[is.na(work_data_test$Sum_cur_out), ]

work_data_test[is.na(work_data_test$Max_dpd_ever), ]
names(work_data_test)
work_data_test<-work_data_test[complete.cases(work_data_test[,142]),]
table(work_data_test$Max_dpd_ever)

work_data_test[(work_data_test$Max_dpd_ever == 0) & (work_data_test$max_dpd_then_pay_all_payment_ever != 0),]

work_data_test<-work_data_test[work_data_test$status == 1,]

names(work_data_test)[names(work_data_test)=='Missed_payment_ever,%']<- 'Missed_payment_ever_proc'
names(work_data_test)[names(work_data_test)=='Missed_payment_12m,%']<- 'Missed_payment_12m_proc'
names(work_data_test)[names(work_data_test)=='Missed_payment_6m,%']<- 'Missed_payment_6m_proc'
names(work_data_test)[names(work_data_test)=='Missed_payment_3m,%']<- 'Missed_payment_3m_proc'
names(work_data_test)[names(work_data_test)=='Missed_payment_1m,%']<- 'Missed_payment_1m_proc'

names(work_data_test)[names(work_data_test) %like% "#_"] 

names(work_data_test) <- gsub("#_", "count_", names(work_data_test))

names(work_data_test)[names(work_data_test) %like% "count_"] 



work_data_test <- select(work_data_test, c(id_order,
                                           dealdate,
                                           decisiondate,
                                           inn,
                                           subgroup,
                                           target_for_calc,
                                           Min_month_from_date_begin,
                                           sum_amountbegin_all_loans_CSF,
                                           Work_type,
                                           Relationship_with_contact_person,
                                           MaxCC_usage_ever,
                                           Age_y,
                                           Monthly_charges,
                                           Current_work_experience_in_years,
                                           MaxCC_out_12m,
                                           Max_dpd_ever,
                                           Education,
                                           out_to_amountmax,
                                           Max_month_from_date_begin,
                                           Relationship_with_contact_person,
                                           Month_since_del_more_then_60,
                                           max_amountmax_all_loans,
                                           MaxCC_usage_1m,
                                           max_amountbegin_all_loans_CC))

############ work data random #################################


work_data_random[is.na(work_data_random$Number_of_dependants), ]
names(work_data_random)
i<-43
work_data_random<-work_data_random[complete.cases(work_data_random[,i]),]
print(names(work_data_random[i]))
print(table(work_data_random[i], useNA = "always"))

work_data_random$Time_in_marriage_y <- NULL
work_data_random$Time_in_marriage_m <- NULL
work_data_random$Registration_term_in_months<-NULL

work_data_random[is.na(work_data_random$Residing_term_in_months), ]
names(work_data_random)
i<-46
work_data_random<-work_data_random[complete.cases(work_data_random[,i]),]
print(names(work_data_random[i]))
print(table(work_data_random[i], useNA = "always"))

work_data_random$Way_of_purchase_of_habitation <- NULL

work_data_random[is.na(work_data_random$Add_Income_from_UW), ]
names(work_data_random)
work_data_random[is.na(work_data_random$Add_Income_from_UW), 61] <- 0
work_data_random[is.na(work_data_random$Add_Income_from_UW), ]

work_data_random[is.na(work_data_random$Payments_in_banks_from_client), ]
names(work_data_random)
work_data_random[is.na(work_data_random$Payments_in_banks_from_client), 65] <- 0
work_data_random[is.na(work_data_random$Payments_in_banks_from_client), ]

work_data_random[is.na(work_data_random$Payments_in_banks_UW), ]
work_data_random[is.na(work_data_random$Payments_in_banks_total), ]
names(work_data_random)
work_data_random[is.na(work_data_random$Payments_in_banks_total), 67] <- 0
work_data_random[is.na(work_data_random$Payments_in_banks_total), ]

work_data_random[is.na(work_data_random$Payments_in_alfa), ]
names(work_data_random)
work_data_random[is.na(work_data_random$Payments_in_alfa), 68] <- 0
work_data_random[is.na(work_data_random$Payments_in_alfa), ]

work_data_random$Loan_amount_in_banks_from_client<-NULL
work_data_random$Use_of_credits_last_5_years <- NULL

work_data_random$Requested_monthly_Payment <- NULL
work_data_random$Requested_loan_amount <- NULL
work_data_random$Requested_contract_amount <- NULL
work_data_random$Requested_loan_Initial_cash_payment<-NULL
work_data_random$Requested_loan_term <- NULL

work_data_random[is.na(work_data_random$di), ]
names(work_data_random)
work_data_random<-work_data_random[complete.cases(work_data_random[,69]),]

work_data_random[is.na(work_data_random$dti), ]

work_data_random$i.inn <- NULL

work_data_random <- add_column(work_data_random, 
                             dti_clear = (work_data_random$Payments_in_alfa+work_data_random$Payments_in_banks_total)/work_data_random$Total_Income, 
                             .after = "dti")

work_data_random[is.na(work_data_random$dti_clear), ]

work_data_random[is.na(work_data_random$sum_amountbegin_close_loans), ]

work_data_random[is.na(work_data_random$Sum_cur_out), ]

work_data_random[is.na(work_data_random$Max_dpd_ever), ]
names(work_data_random)
work_data_random<-work_data_random[complete.cases(work_data_random[,142]),]
table(work_data_random$Max_dpd_ever)

work_data_random[(work_data_random$Max_dpd_ever == 0) & (work_data_random$max_dpd_then_pay_all_payment_ever != 0),]

work_data_random<-work_data_random[work_data_random$status == 1,]

names(work_data_random)[names(work_data_random)=='Missed_payment_ever,%']<- 'Missed_payment_ever_proc'
names(work_data_random)[names(work_data_random)=='Missed_payment_12m,%']<- 'Missed_payment_12m_proc'
names(work_data_random)[names(work_data_random)=='Missed_payment_6m,%']<- 'Missed_payment_6m_proc'
names(work_data_random)[names(work_data_random)=='Missed_payment_3m,%']<- 'Missed_payment_3m_proc'
names(work_data_random)[names(work_data_random)=='Missed_payment_1m,%']<- 'Missed_payment_1m_proc'

names(work_data_random)[names(work_data_random) %like% "#_"] 

names(work_data_random) <- gsub("#_", "count_", names(work_data_random))

names(work_data_random)[names(work_data_random) %like% "count_"] 



work_data_random <- select(work_data_random, c(id_order,
                                           dealdate,
                                           decisiondate,
                                           inn,
                                           subgroup,
                                           target_for_calc,
                                           Min_month_from_date_begin,
                                           sum_amountbegin_all_loans_CSF,
                                           Work_type,
                                           Relationship_with_contact_person,
                                           MaxCC_usage_ever,
                                           Age_y,
                                           Monthly_charges,
                                           Current_work_experience_in_years,
                                           MaxCC_out_12m,
                                           Max_dpd_ever,
                                           Education,
                                           out_to_amountmax,
                                           Max_month_from_date_begin,
                                           Relationship_with_contact_person,
                                           Month_since_del_more_then_60,
                                           max_amountmax_all_loans,
                                           MaxCC_usage_1m,
                                           max_amountbegin_all_loans_CC))


###############################################################################################################################
## binning 

################ Create binning data 

# initial number of columns
begin_ncol <- ncol(work_data)

str(work_data)

#initiate target column and first column with variables
target_ncol <- match("target_60max12m",names(work_data))

variable_fc <- 7 #put number of first variable column

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

sink('structure2.txt')

names(work_data)

for (i in c(5:ncol(work_data)))
{
  print(names(work_data[i]))
  print(table(work_data[i], useNA = "always"))
  # readline(prompt="Press enter to next")
} 

sink()


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
####deleted

setwd("S:/PROJECTS/SCORING/2018.10.31 Scoring CC/CC_no_CSF_appl_beh_alfa/binning2")
#setwd("H:/Документы/Работа_проекты/Scoring CC_no_CSF/Scoring CC_no_CSF appl beh alfa/binning2")

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
  eq$brks[length(eq$brks)]<-eq$brks[length(eq$brks)]+digit[i]
  print(eq$brks)
  # kmeans 
  # Idea: search k 'center' points, that have biggest spreading of points around this 'center'(every interval have his own 'center' point)
  print('calculating kmeans method...', quote = FALSE)
  # k in {6,5,4,3,2}
  interval_count<-6
  repeat{
    km<-classIntervals(floor(work_data[,i]/digit[i])*digit[i], interval_count, style = 'kmeans')
    km$brks[length(km$brks)]<-km$brks[length(km$brks)]+digit[i]
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
    
    # column name for new binning column, that bins with 'equal' method
    colname <- paste(names(work_data)[i], "cat_eq", sep="_")
    
    # set column, that bins with 'equal' method
    work_data[[colname]] <- with(work_data, cut(as.numeric(work_data[,i]), 
                                                c(min(work_data[,i])-5,unique(eq$brks)),include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    
    # for saving ordered factors all repleacements must be done on factor levels, not on work_data(!!!)
    levels(work_data[[colname]])<-gsub(",", ";", levels(work_data[[colname]]))
    
    # kmeans 
    colname <- paste(names(work_data)[i], "cat_km", sep="_")
    work_data[[colname]] <- with(work_data, cut(as.numeric(work_data[,i]), km$brks,include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
    
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






