# https://www.r-bloggers.com/five-steps-for-missing-data-with-finalfit/
# https://basegroup.ru/community/articles/missing

install.packages("GGally")
install.packages("finalfit") 
install.packages("MissMech") 


library(finalfit) 
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
library(VIM)
library(GGally)
library(MissMech)

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


work_data$duration_month_2  <- as.numeric(work_data$duration_month_2)
work_data$credit_amount_5   <-  as.numeric(work_data$credit_amount_5 )
work_data$instalment_pct_8 <-  as.numeric(work_data$instalment_pct_8)
work_data$present_residence_since_11 <-  as.numeric(work_data$present_residence_since_11)
work_data$age_in_yrs_13        <-  as.numeric(work_data$age_in_yrs_13)
work_data$number_cards_this_bank_16    <-  as.factor(work_data$number_cards_this_bank_16)
work_data$no_people_liable_for_mntnance_18 <-  as.numeric(work_data$no_people_liable_for_mntnance_18)


work_data <- data.frame(target = ifelse(work_data$good_bad_21 == 1, 1,0), work_data)
work_data$good_bad_21 <- NULL  

set.seed(321)
ind_housing_type <- sample(1:1000, 15)
ind_job <- sample(1:1000, 21)
ind_number_cards_this_bank <- sample(1:1000, 23)

work_data[ind_housing_type, 16] <- NA
work_data[ind_job, 18] <- NA
work_data[ind_number_cards_this_bank, 17] <- NA

set.seed(651)
ind_housing_type2 <- sample(1:1000, 21)
ind_job2 <- sample(1:1000, 21)
ind_number_cards_this_bank2 <- sample(1:1000, 33)

work_data[ind_housing_type2, 16] <- NA
work_data[ind_job2, 18] <- NA
work_data[ind_number_cards_this_bank2, 17] <- NA


work_data <-
  work_data %>%
  mutate(housing_type_15 = recode(housing_type_15, 
                          A151 = "RENT",
                          A152 = "OWN",
                          A153 = "RELATIVES_PROPERTY"))

work_data <-
  work_data %>%
  mutate(job_17 = recode(job_17, 
                         A171 = "PENSIONER",
                         A172 = "OWN_BUSINESS",
                         A173 = "HIRED_EMPLOYEE",
                         A174 = "NOT_OFFICIAL"))


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

str(work_data)


options(repr.plot.width=8, repr.plot.height=5)
aggr_plot = aggr(work_data[,-1], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
                 labels=names(work_data[,-1]), cex.axis=.7, gap=1,
                 ylab=c("Histogram of missing data","Pattern"))

explanatory = c("housing_type_15", "chk_ac_status_1", "job_17")
dependent = "target"
work_data %>% 
  missing_pairs(dependent, explanatory)


work_data %>%
  missing_plot()




