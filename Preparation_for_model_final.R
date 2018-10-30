library(woe)
library(smbinning) 
library(classInt)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(lazyeval)

data <- read.csv2("cc_no_csf_else_bin.csv")

begin_ncol <- ncol(data)
for (i in c(2:begin_ncol)) 
{ 
  options("scipen"=100, "digits"=4)
  
  #equal
  eq<-classIntervals(data[,i], 5, style = 'quantile')
  colname <- paste(names(data)[i], "eq", sep="_")
  data[[colname]] <- with(data, cut(data[,i], 
                                    c(min(data[,i])-1,unique(eq$brks)),include.lowest = TRUE, right = FALSE, ordered = TRUE))
  levels(data[[colname]])<-gsub(",", ";", levels(data[[colname]]))
  
  #kmeans
  interval_count<-6
  repeat{
    km<-classIntervals(data[,i], interval_count, style = 'kmeans')
    colname <- paste(names(data)[i], "km",interval_count, sep="_")
    data[[colname]] <- with(data, cut(data[,i], km$brks,include.lowest = TRUE, right = FALSE, ordered = TRUE))
    levels(data[[colname]])<-gsub(",", ";",levels(data[[colname]]))
    interval_count<-interval_count-1
    if (min(table(data[[colname]])/nrow(data))>3) {break}
    if (interval_count<2) {break}
  }
  
  #smbinning
  sb=smbinning(data,y=names(data)[1],x=names(data)[i])
  colname <- paste(names(data)[i], "sb", sep="_")
  data[[colname]] <- with(data, cut(data[,i], c(min(data[,i])-1,unique(sb$bands)), right = TRUE, left = FALSE, ordered = TRUE))
  levels(data[[colname]])<-gsub(",", ";",levels(data[[colname]]))
  
  #save(eq, km, sb, file = paste(names(data[i]),".rda", sep = ""))
  #rm(eq)
  #rm(km)
  #rm(sb)
}

data <- data[,c(1,(begin_ncol+1):ncol(data))]

file<-data
# create loop for BR
begin_ncol<-ncol(file)
for (i in 2:begin_ncol){
  
  var_for_group <- names(file)[i]
  column_br <- paste("BR", names(file)[i] , sep="_")
  br_table <- file %>%
    select(c(i,1)) %>%
    group_by_(.dots = var_for_group) %>%
    summarise_all(funs(!!column_br := (n() - sum(.))/n()))
  
  file <- left_join(file,br_table,by=names(file)[i])
}

## Go to pdf

data <- file

Total<-length(data$target_for_calc)
Good<-sum(data$target_for_calc)
Bad<-Total-Good
pdf("var_plonew9.pdf",width = 9,height=10,paper='special')
for (j in 2:begin_ncol) {
  plot1_hist<-ggplot(data, aes(data[,j])) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    scale_y_continuous(labels=scales::percent)+ 
    geom_text(aes( y = ((..count..)/sum(..count..)),label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
    theme(axis.text.x = element_text(angle=10, vjust=0.9),
          plot.margin = unit(c(1,1,1,1), "cm") ) + 
    labs( y = "Class", x = "")
  
  plot2_line<-ggplot(data, aes(x=data[,j],y=data[,j+begin_ncol-1],group=1)) + 
    geom_line(color="dodgerblue4",size=1)+
    geom_point() +
    theme(axis.text.x = element_text(angle=10, vjust=0.9),
          plot.margin = unit(c(1,1,1,1), "cm") ) + 
    scale_y_continuous(limits=c(0, 0.3),breaks=c(0.05,0.1,0.15, 0.2, 0.25, 0.3), labels = function(x) paste0(x*100, "%"))+
    labs( y = "BR", x = "")
  
  # extract gtable
  g1 <- ggplot_gtable(ggplot_build(plot1_hist))
  g2 <- ggplot_gtable(ggplot_build(plot2_line))
  
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
  
  
  aggregate_table<-aggregate(. ~ data[,j], data = data[c(names(data)[1],names(data)[j])],
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
  names(aggregate_table)<-c(names(data)[j],"good, #","bad, #","total, #","good, %","bad, %","total, %","BR, %","WOE")
  
  IV=round(sum((aggregate_table[,5]-aggregate_table[,6])/100*aggregate_table[,9]),4)
  
  table <- ggtexttable(aggregate_table, rows = NULL, theme = ttheme("mBlue"))
  
  
  
  text <- paste("
              ",names(data)[j],"     ","IV =",IV, sep = " ")
  title <- ggparagraph(text = text, face = "italic", size = 25, color = "black")
  print(ggarrange(title,g, table , 
                  ncol = 1, nrow = 3,heights = c(0.055,0.3, 0.3, 0.2)))
  
}
dev.off()
## Remove unused columns

data$Age_of_passport_month_km_4 <- NULL

## Calc_WOE

data_woe <- select(data, -grep("BR", names(data)))

for (i in 2:ncol(data_woe)){
  var_for_group <- names(data_woe)[i]
  column_woe <- paste("WOE", names(data_woe)[i] , sep="_")
  woe_table <- data_woe %>%
    select(c(i,1)) %>%
    group_by_(.dots = var_for_group) %>%
    summarise_all(funs(!!column_woe := log((sum(.)/Good)/((length(.)-sum(.))/Bad))))
                       
  data_woe <- left_join(data_woe,woe_table,by=names(data_woe)[i])
} 

data_log_regr<- select(data_woe, c(1, grep("WOE", names(data_woe))))
data_cor <- select(data_woe, grep("WOE", names(data_woe))) 


data("mtcars")
my_data <- mtcars[, c(1,3,4,5,6,7)]
# print the first 6 rows
head(my_data)

#delete NA
res <- cor(my_data, use="na.or.complete")
round(res, 2)

cor_table <- cor( data_cor[!is.infinite(rowSums(data_cor)),])


install.packages("corrplot")
library(corrplot)
#install.packages("corrplot")

# type:“upper”, “lower”, “full” - method of visualization
#order = "hclust",
#tl.col - text label color
#tl.srt - text label string rotation(angle)

#data for "corrplot" must be matrix with correlation 
View(cor_table)
corrplot(cor_table, method="number")
corrplot(res, method="number",type = "lower",tl.col = "black")
corrplot(res, method="number",type = "lower",tl.col = "black", tl.srt = 45)
corrplot(res, method="circle",type = "upper",tl.col = "black", 
         order = "hclust", tl.srt = 45,diag = FALSE)

#if u want to view all methods(or another subfunc of 'corrplot') write value with mistake
corrplot(res, method="circle",type = "upper",tl.col = "black", 
         order = "dddddd", tl.srt = 45,diag = FALSE)


##full description of corr
#install.packages("PerformanceAnalytics")
#good for 10 variables,not bigger(takes a lot of time)

library("PerformanceAnalytics")
my_data <- mtcars[, c(1,3,4,5,6,7)]
#data for "chart.Correlation" must be simple data,this func build all by yourself
chart.Correlation(my_data, histogram=TRUE, pch=10)


#install.packages("Hmisc")
library("Hmisc")
#rcorr() - to compute the significance levels for pearson and spearman correlations
res2 <- rcorr(as.matrix(my_data))
res2

#only corr
res2$r

#only p-value
res2$P


rm(col)
# corr : matrix of the correlation coefficients
# pval : matrix of the correlation p-values
tableCorrMatrix <- function(corr, pval) {
  ut <- upper.tri(corr)
  data.frame(
    row = rownames(corr)[row(corr)[ut]],
    column = rownames(corr)[col(corr)[ut]],
    cor  =(corr)[ut],
    p = pval[ut]
  )
}

corrtable<-tableCorrMatrix(res2$r, res2$P)

#try to choose value that have significant corr
resultcorr<-subset(corrtable,abs(cor)>0.5)

#order by 1,2
resultcorr<-resultcorr[order(resultcorr[,1],resultcorr[,2]),] 

#select data,that have hight corr
#primary type of (resultcorr$column,resultcorr$row) is "factor","unique" func returns numbers
#unique(c(resultcorr$column,resultcorr$row))
new_data<-my_data[,unique(c(as.character(resultcorr$column),as.character(resultcorr$row)))]

#in this case that all data,so create actificial data,that include less columns
#new_data<-new_data[,c(2,3,4)]
new_data<-cor(new_data)


corrplot(new_data, type = "upper", order = "alphabet", method="number", 
         tl.col = "black", tl.srt = 45,diag = FALSE)

new_data<-my_data[unique(resultcorr$column)]
#new_data<-new_data[,c(3,4)]
chart.Correlation(new_data, histogram=TRUE, pch=10)



##------------------Data from buro------------------------------##
example<-read.csv2("WOE.csv", sep=";",colClasses="numeric",dec=".")

#take away first column("target")
example<-example[,-1]

#reviewing data
str(example)

res2 <- cor(example)
round(res2, 2)

#build plot in pdf
pdf("CorrPlot2.pdf",width = 25,height=25,paper='special')
corrplot(cor_table, method="circle",type = "upper",tl.col = "black",diag = FALSE)
corrplot(cor_table, method="number",type = "upper",tl.col = "black",diag = FALSE)
chart.Correlation(res, histogram=TRUE, pch=10)
dev.off()




corrplot(res, method="number",type = "upper",tl.col = "black",diag = FALSE)




res3 <- rcorr(as.matrix(example))
res3$r
res3$P
round(res3$P,2)
tableCorrMatrix <- function(corr, pval) {
  ut <- upper.tri(corr)
  data.frame(
    row = rownames(corr)[row(corr)[ut]],
    column = rownames(corr)[col(corr)[ut]],
    cor  =(corr)[ut],
    p = pval[ut]
  )
}

corrtable<-tableCorrMatrix(res3$r, res3$P)

#try to choose value that have significant corr
resultCORR<-subset(corrtable,abs(cor)>0.5)

#try to choose value that have significant corr
resultPVAL<-subset(corrtable,abs(p)>0.05)

#order by 1,2
resultCORR<-resultCORR[order(resultCORR[,1],resultCORR[,2]),] 

resultPVAL<-resultPVAL[order(resultPVAL[,1],resultPVAL[,2]),] 

#select data,that have hight corr
CORR_buro<-example[,unique(c(as.character(resultCORR$column),as.character(resultCORR$row)))]

PVAL_buro<-example[,unique(c(as.character(resultPVAL$column),as.character(resultPVAL$row)))]

chart.Correlation(CORR_buro, histogram=TRUE, pch=10)

CorrforPlot <- rcorr(as.matrix(PVAL_buro))
CORR_buro_forPlot<-cor(CORR_buro)
PVAL_buro_forPlot<-CorrforPlot$P

pdf("HightCorrPlot_4.pdf",width = 15,height=15,paper='special')
corrplot(CORR_buro_forPlot, method="number",type = "upper",tl.col = "black",diag = FALSE)
corrplot(PVAL_buro_forPlot, method="number",type = "upper",tl.col = "black",diag = FALSE)
dev.off()

#build plot with discription 
names(PVAL_buro)
chart.Correlation(PVAL_buro[,c("C_Gender_inn","C_d_begin_ft","C_al_max12dpdAmt_x_Amt","C_cr_Out_x_OpAmt")], histogram=TRUE, pch=10)








