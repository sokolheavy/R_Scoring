library(corrplot)
#install.packages("corrplot")

data("mtcars")
my_data <- mtcars[, c(1,3,4,5,6,7)]
# print the first 6 rows
head(my_data)

res <- cor(my_data)
round(res, 2)

#delete NA
cor(my_data, use = "complete.obs")


# type:“upper”, “lower”, “full” - method of visualization
#order = "hclust",
#tl.col - text label color
#tl.srt - text label string rotation(angle)

corrplot(res, method="number")
corrplot(res, method="number",type = "lower",tl.col = "black")
corrplot(res, method="number",type = "lower",tl.col = "black", tl.srt = 45)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

##full description of corr
#install.packages("PerformanceAnalytics")

  library("PerformanceAnalytics")
my_data <- mtcars[, c(1,3,4,5,6,7)]
chart.Correlation(my_data, histogram=TRUE, pch=10)


##for a big list of variables 
#Correlation matrix with significance levels (p-value)
library(Hmisc)
#install.packages("Hmisc")
#rcorr() - to compute the significance levels for pearson and spearman correlations
library("Hmisc")
res2 <- rcorr(as.matrix(my_data))
res2

#only corr
res2$r

#only p-value
res2$P
res2$P[is.na(res2$P)]<-1


#if u have a lit of column, it's convenient to have table with correlation
#Column 1 : row names (variable 1 for the correlation test)
#Column 2 : column names (variable 2 for the correlation test)
#Column 3 : the correlation coefficients
#Column 4 : the p-values of the correlations

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
resultcorr[,order()]


