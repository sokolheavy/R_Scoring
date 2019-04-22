## Binning

#install.packages("classInt")
library(classInt)

#install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')
library(spDataLarge)

x <- classIntervals(data$Age, 4, style = 'equal')
x
y <- classIntervals(data$Age, 4, style = 'quantile')
y

#install.packages("woeBinning")
library("woeBinning")

woe.binning(data, 'target_for_calc','Total_payments_in_other_banks',min.perc.total= 0.01, min.perc.class = 0.05, stop.limit=0.1)
w<-woe.binning(data, 'target_for_calc','Additional_income',min.perc.total= 0.01, min.perc.class = 0.05, stop.limit=0.1)


#install.packages("smbinning")
#install.packages("dplyr")
library(dplyr)
library(smbinning) # Load package and its data

# Example: Optimal binning
result=smbinning(df=data,y="target_for_calc",x=names(data)[2]) # Run and save result
result$ivtable # Tabulation and Information Value
result$iv # Information value
result$bands # Bins or bands
result$ctree # Decision tree


################ func for writting bining value #################################
  data<-read.csv2("cc_no_csf_incall_bin.csv",na.strings = c("NA"))
  for (i in 2:ncol(data)){
    data_bin=smbinning(df=data,y=names(data)[1],x=names(data)[i])
    
    replacement<-ceiling(max(data[,names(data)[i]][!is.na(data[,names(data)[i]])]))
    cuts<-c(data_bin$cuts,replacement)
    
    #search 'NA' value for replasement to another class "Missimg"
    NA_data<-which(is.na(data[,i]))
    
    k<-1
    for (j in cuts){
     
    data[,i][data[,i] <=j]<-replacement+k
    k<-k+1
    }
    
  k<-k-1
  data[NA_data,i]<-"Missing"
  
  while (k!=0){
    data[,names(data)[i]][data[,names(data)[i]]==(replacement+k)]<-paste0(k,".",names(data)[i],"<=",cuts[k])
    k<-k-1
  }
  
  data[,names(data)[i]]<-as.factor(data[,names(data)[i]])
  }
