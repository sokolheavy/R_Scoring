##read.delim - read.table with "\" sep 
read.delim("rr.txt",header = TRUE,sep="/")
t<-read.delim("name.txt",fill=TRUE,sep="/")
#header=TRUE by defolt,u can don`t write with dich

##mode()&typeof()&class()
#mode() The 'atomic' modes are numeric, complex, character and logical
#type() 'avarage' type 
#class() - 'global' type of object
x <- 1
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- letters
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- TRUE
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- cars
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- cars[1]
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- cars[[1]]
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- matrix(cars)
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- new.env()
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- expression(1 + 1)
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- quote(y <- 1 + 1)
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- ls
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

#dates
dates <- c("15-9-2009", "16-07-2008", "17 12-2007", "29-02-2011")
as.POSIXct(dates, format = "%d-%m-%Y")
'%a Abbreviated weekday name in the current locale. Mon
%A Full weekday name in the current locale. Monday
%b Abbreviated month name in the current locale. Sep
%B Full month name in the current locale. September
%m Month number (01-12) 09
%d Day of the month as decimal number (01-31). 28
%y Year without century (00-99) 13
%Y Year including century. 2013'

#String normalization
library(stringr)
str_trim(" hello world ")
## [1] "hello world"
str_trim(" hello world ", side = "left")
## [1] "hello world "
str_trim(" hello world ", side = "right")
## [1] " hello world"

#grep and grepl functions:
grep("[a-z]", letters) #index
grepl("[a-z]", letters) #TRUE/FALSE

gender <- c("M", "male ", "Female", "fem.")
grepl("m", gender, ignore.case = TRUE) #remove sensitive to caps lock

v <- factor(c("2", "3", "5", "7", "11"))

vv<-as.numeric(v)
typeof(vv)
mode(vv)
class(vv)

txt<-readLines("example0.txt")
#
i<-grepl("^%", txt)
dat<-txt[!i]
fieldList <- strsplit(dat, split = ";")

#input and assigns the values in the right order.
x<-strsplit(dat, split = ";")
assignFields <- function(x){
  out <- character(3)
  # get names
  i <- grepl("[[:alpha:]]",x[[1]][])
  out[1] <- x[i]
  # get birth date (if any)
  i <- which(as.numeric(x) < 1890)
  out[2] <- ifelse(length(i)>0, x[i], NA)
  # get death date (if any)
  i <- which(as.numeric(x) > 1890)
  out[3] <- ifelse(length(i)>0, x[i], NA)
  out
}



txt<-readLines("example.txt")
#
i<-grepl("^//", txt)
dat<-txt[!i]
fieldList <- strsplit(dat, split = ";")
class(fieldList)
#input and assigns the values in the right order.
x<-strsplit(dat, split = ";")
x<-x[1]
assignFields <- function(x){
  out <- character(3)
  # get names
  i <- x[[1]][grep("[[:alpha:]]",x)]
  out[1] <- i
  # get birth date (if any)
  i <- which(as.numeric(x) < 1890)
  i <- x[x < 1890]
  out[2] <- ifelse(length(i)>0, x[i], NA)
  # get death date (if any)
  i <- which(as.numeric(x) > 1890)
  out[3] <- ifelse(length(i)>0, x[i], NA)
  out
}
x<-as.vector(x)
x[,lapply(x,is.numeric)]
lapply(l,sum )

typeof(x)
x[as.numeric(x)]
as.vector(x)
x[as.numeric(x)]

class(x[[1]][3])
class(81.3)

v<-c(1,2,3)
class(v)
typeof(v)
is.numeric("v")
#https://raw.github.com/edwindj/datacleaning/master/data/dirty_iris.csv
data<-read.csv("dirty_iris.csv",header = TRUE,sep=";",stringsAsFactors=FALSE)
str(data)
View(data)
is.finite(c(1, Inf, NaN, NA))
X<-data[3,]


#only for row
is.special <- function(x){
  if (is.numeric(x)) is.finite(x)=NULL else x
}
predata<-sapply(data, is.special)
n<-names(data)
t<-data.frame(names(n))
c<-c(1,1,1,1,1)
rbind(t,c)
x<-data[1,]
x[x==TRUE]




rm_badval<-function(x){
  i<-1
  data1<-data.frame()
  while(i<=nrow(x))
  {
    a<-x[i,]
    if (is.numeric(a)) !is.finite(a) else !is.na(a)
    if (length(a[a==TRUE])>0) data1 else data1<-rbind(data1,a)
    i<-i+1
  }
  data1
}

rm_badval(data)


#install.packages("functional")
library(functional)


rm_badval<-function(x){
  i<-1
  df<-data.frame()
  while(i<=ncol(x))
  {
    a<-x[,i]
    if (is.numeric(a)) t[is.infinite(t) | is.nan(t)] <- NA else a
    i<-i+1
  }
  na.omit(x)
}
rm_badval(data)
View(data)
data[1:2,2]<-NaN
t<-data[,2]
t[is.infinite(t) | is.nan(t)] <- NA




# gsub (remove unuseles symbol)

"hello".gsub(/[aeiou]/, '*')                  #=> "h*ll*"
"hello".gsub(/([aeiou])/, '<\1>')             #=> "h<e>ll<o>"
"hello".gsub(/./) {|s| s.ord.to_s + ' '}      #=> "104 101 108 108 111 "
"hello".gsub(/(?<foo>[aeiou])/, '{\k<foo>}')  #=> "h{e}ll{o}"
'hello'.gsub(/[eo]/, 'e' => 3, 'o' => '*')    #=> "h3ll*"

# replace /ll/ with itself
'hello'.gsub(/ll/, '\0') # returns 'hello'
'hello'.gsub(/ll/, "\0") # returns 'he\000o'

v = "Foo Bar!"  # Target: Foo\ Bar\!
# Resulting strings will not be quoted to decrease
# the amount of backslashes. Compare \\! to "\\\\!"

v.gsub(/\W/, '\0') #=> Foo Bar!

# \\ escapes to a literal \, which next to the 0 becomes \0
v.gsub(/\W/, '\\0') #=> Foo Bar!

# \\\0, means "\ \0", or "escaped \0"
v.gsub(/\W/, '\\\0') #=> Foo\0Bar\0

# Same mechanism as before. \\ → \
v.gsub(/\W/, '\\\\0') #=> Foo\0Bar\0

# Finally! We have now an escaped \ before \0 and
# we get the results we want.
v.gsub(/\W/, '\\\\\0') #=> Foo\ Bar\!

# It's very tempting to just write it like this now, right?
v.gsub(/\W/) { |m| "\\#{m}" } #=> Foo\ Bar\!
# It might not be shorter, but anyone can understand it.

