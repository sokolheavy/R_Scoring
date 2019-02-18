# https://www.edureka.co/blog/clustering-on-bank-data-using-r/
# https://towardsdatascience.com/how-to-cluster-your-customer-data-with-r-code-examples-6c7e4aa6c5b1  

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