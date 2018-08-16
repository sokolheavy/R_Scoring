data("mtcars")
my_data <- mtcars[, c(1,3,4,5,6,7)]
# print the first 6 rows
head(my_data)

#delete NA
res <- cor(my_data, use = "complete.obs")
round(res, 2)

library(corrplot)
#install.packages("corrplot")

# type:“upper”, “lower”, “full” - method of visualization
#order = "hclust",
#tl.col - text label color
#tl.srt - text label string rotation(angle)

#data for "corrplot" must be matrix with correlation 
corrplot(res, method="number")
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



##------------------example in buro data------------------##
example<-read.csv2("WOE.csv", sep=";",colClasses="numeric",dec=".")

#take away first column("target")
example<-example[,-1]

#reviewing data
str(example)

res2 <- cor(example)
round(res2, 2)

#build plot in pdf
pdf("CorrPlot.pdf",width = 15,height=15,paper='special')
corrplot(res2, method="circle",type = "upper",tl.col = "black",diag = FALSE)
corrplot(res2, method="number",type = "upper",tl.col = "black",diag = FALSE)
dev.off()



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











