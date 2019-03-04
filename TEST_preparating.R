work_data<-read.table("https://raw.githubusercontent.com/Srisai85/GermanCredit/master/german.data", h=F, sep="")
colnames(work_data) <- c("chk_ac_status_1",
                         "duration_month_2", "credit_history_3", "purpose_4",
                         "credit_amount_5","savings_ac_bond_6","p_employment_since_7", 
                         "instalment_pct_8", "personal_status_9","other_debtors_or_grantors_10", 
                         "present_residence_since_11","property_type_12","age_in_yrs_13",
                         "other_instalment_type_14", "housing_type_15", 
                         "number_cards_this_bank_16","job_17","no_people_liable_for_mntnance_18",
                         "telephone_19", "foreign_worker_20", 
                         "good_bad_21")

work_data <- data.frame(target_for_calc = ifelse(work_data$good_bad_21 == 1, 1,0), work_data)
work_data$good_bad_21 <- NULL  

work_data <- work_data[,c(1,2,4,5)]

## Create loop for BR (add columns with BR for every binning variable)
variable_fc_bin <- 2
bin_data <- work_data
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

work_data <- bin_data


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
library(corrplot)
library(tibble)


## Create loop for BR (add columns with BR for every binning variable)
variable_fc_bin <- 2
target_calc_bin <- match("target_for_calc",names(bin_data))
bin_data <- work_data
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

## IV - statistic table
iv_table <- arrange(cbind.data.frame(variables = names(bin_data[variable_fc_bin:bin_ncol])
                                     ,IV = sapply(bin_data[variable_fc_bin:bin_ncol], function(x) round(IV(X=x, Y=bin_data$target_for_calc)[1],4)), row.names = NULL),
                    desc(IV))

# Add strength of variables
iv_table$Strength <- ifelse(iv_table$IV>=1, "Suspicious",
                            ifelse(iv_table$IV>=.5, "Very strong",
                                   ifelse(iv_table$IV>=.2, "Strong",
                                          ifelse(iv_table$IV>=.1, "Average",
                                                 ifelse(iv_table$IV>=.02, "Weak", "Wery weak")))))



############### Create HTML-file with statistics for all bining variables
#install.packages("R2HTML")
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
j <- 2

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
                  ncol = 1, nrow = 2,heights = c(0.1, 0.04, 0.04, 0.3, 0.2)))
  
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