######################smbinning####################
#install.packages("smbinning")
library(smbinning) # Load package and its data

# Example: Optimal binning
result=smbinning(df=file_bin,y="fgood",x="cbs1") # Run and save result
result$ivtable # Tabulation and Information Value
result$iv # Information value
result$bands # Bins or bands
result$ctree # Decision tree


###############func for bining#######################
#install.packages("sas7bdat")
#install.packages("Hmisc")
library(sas7bdat)
library(Hmisc)
data <- read.csv2("cc_no_csf_incall_bin.csv")
str(data)
attach(data)



setwd("S:/PROJECTS/! Individual/LSokol/!Rstudio/script")
n<-10
bin <- function(x, y){
  n <- min(50, length(unique(x)))
  repeat {
    n   <- n - 1
    d1  <- data.frame(x, y, bin = cut2(x, g = n)) 
    d2  <- aggregate(d1[-3], d1[3], mean)
    cor <- cor(d2[-1], method = "spearman")
    if(abs(cor[1, 2]) == 1) break
  }
  d2[2] <- NULL
  colnames(d2) <- c('LEVEL', 'RATE')
  head <- paste(toupper(substitute(y)), " RATE by ", toupper(substitute(x)), sep = '')
  cat("+-", rep("-", nchar(head)), "-+\n", sep = '')
  cat("| ", head, ' |\n', sep = '')
  cat("+-", rep("-", nchar(head)), "-+\n", sep = '')
  print(d2)
  cat("\n")
}

n<-10
v<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)
cttt<-cut2(v,g=15)
bin <- function(x, y){
  n <- min(50, length(unique(data$Additional_income)))
  repeat {
    n   <- n - 1
    d1  <- data.frame(data$Additional_income, data$target_for_calc, bin = cut2(data$Additional_income, g = n)) 
    d2  <- aggregate(d1[-3], d1[3], mean)
    cor <- cor(d2[-1], method = "spearman")
    n
    cor[1, 2]
    if(abs(cor[1, 2]) == 1) break
  }
  d2[2] <- NULL
  colnames(d2) <- c('LEVEL', 'RATE')
  head <- paste(toupper(substitute(y)), " RATE by ", toupper(substitute(x)), sep = '')
  cat("+-", rep("-", nchar(head)), "-+\n", sep = '')
  cat("| ", head, ' |\n', sep = '')
  cat("+-", rep("-", nchar(head)), "-+\n", sep = '')
  print(d2)
  cat("\n")
}
names(data)
bin(Age,target_for_calc)



# Load library and its dataset
install.packages("smbinning")
library(smbinning) # Load package and its data

names(data)
# Example: Optimal binning
result=smbinning(df=data,y="target_for_calc",x="Additional_income") # Run and save result
result$ivtable # Tabulation and Information Value
result$iv # Information value
result$bands # Bins or bands
result$ctree # Decision tree


data2<-read.csv2("cc_no_csf_incall_bin.csv")
data2$Additional_income<-as.factor(data2$Additional_income)
str(data2)


bin1<-smbinning.factor(df=data2,y="target_for_calc",x="Total_payments_in_other_banks", maxcat = 5)

## Binning

install.packages("classInt")
library(classInt)
install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')
library(spDataLarge)

x <- classIntervals(data$Age, 4, style = 'equal')
x
y <- classIntervals(data$Age, 4, style = 'quantile')
y

install.packages("smbinning")
library(smbinning)
length(unique(data$Additional_income))
result=smbinning(df=data, y="target_for_calc", x="Total_payments_in_other_banks", p=0.05) 
View(result)
result$ivtable

install.packages("woeBinning")
library("woeBinning")

names(data)
woe.binning(data, 'target_for_calc','Total_payments_in_other_banks',min.perc.total= 0.01, min.perc.class = 0.05, stop.limit=0.1)
w<-woe.binning(data, 'target_for_calc','Additional_income',min.perc.total= 0.01, min.perc.class = 0.05, stop.limit=0.1)


w1<-as.data.frame(w[2])
rownames(w1)
typeof(w1)
names(data)
length(unique(data$Additional_income))

Total_payments_in_other_banks_bin<-ifelse(data$Total_payments_in_other_banks <=273, "tp<=273", ifelse(data$Total_payments_in_other_banks <=477, "tp<=477", "tp>477"))
data<-cbind(data,Total_payments_in_other_banks_bin)




str(data)
