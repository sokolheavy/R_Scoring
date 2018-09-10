#load data for calc
#file<-read.csv2("cc_no_csf_incall_bin.csv")

#recall column with "target",take universall name(target can be 'target_for_calc','target_for_calc_30' and so on) 
names(file)[grep("target", names(file))]<-"target"

#generate "file" with first column "target" and another columns are sorted alphabetically
data<-cbind(file$target,(file[ , sort(names(file[,-which( colnames(file)=="target")]))]))

#recall first column,cauth automatically programe calls it "data$target"
names(file)[1]<-"target"


Total<-length(file$target)
Good<-sum(file$target)
Bad<-Total-Good
begin_ncol<-ncol(file)

##create loop for calc WOE
for (i in 2:begin_ncol){
  #t1-aggregate file for create BR
  count_woe<-aggregate(. ~ file[,i], data = file[c(names(file)[1],names(file)[i])],
                       FUN = function(x) c(WOE=round(log((sum(x)/Good)/((length(x)-sum(x))/Bad)),4)))[,c(1,2)]
  
  #log(x) will produce NaN any time x is less than zero(calculating 'length(x)-sum(x)' we have '-' func 'log' see that and returns error
  
  #all aggregate data from "aggregate" func is matrix, convert to data.frame
  count_woe<-cbind(count_woe[,1],data.frame(count_woe[,2]))
  
  
  #we must have united column with common name
  #WOE columns begins with "WW_"
  #(all variables sorted by alphabet,we split table by 2 part,'value column' in the left,  'woe column' in the right(names begin with WW_") )
  names(count_woe)<-c(names(file)[i],paste0("WW_",names(file)[i]))
  
  #add  new column to our "file" table
  file<-merge(file,count_woe,by.x=names(file)[i],by.y=names(count_woe)[1])
  
  #again sort column,cauth 'woe column' goes to the end,but joined column goes to the begining,totaly bad column)
  file<-cbind(file$target,(file[ , sort(names(file[,-which( colnames(file)=="target")]))]))
  
  #recall first column,cauth automatically programe calls it "file$target"
  names(file)[1]<-"target"
}

#rewrite for log regresion
data<-file[,c(1,ceiling(ncol(file)/2):ncol(file))]
names(data)<-names(file)[1:ceiling(ncol(file)/2)]
str(data) 
