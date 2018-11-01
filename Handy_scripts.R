########################### conection ms server
# install.packages("odbc")
# install.packages("data.table")
library("odbc")
library(data.table)
con <- dbConnect(odbc(),Driver = "SQL Server",Server = "...",Database = "...",trusted_connection=TRUE,Port = 1433)
sample_table1 <- as.data.table(dbGetQuery(con, "select ... "))


########################### SQL 'language'
# install.packages("dplyr")
# install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# select - columns
# diamonds - table from R standart collection; (cut, price, color) - columns in table diamonds
select(diamonds, cut, price, color)

# this select is wrang and generate mistake
select(diamonds,-3:4)

#this select is ok
select(diamonds, 1, 2, 3)
select(diamonds, -cut)

select(diamonds, contains("t"))
select(diamonds, ends_with("t"))
select(diamonds, starts_with("c"))

# slice - select rows that specified in vector
slice(diamonds, c(1, 4, 5))

# filter observations - set condition
filter(diamonds, cut %in% c("Good","Premium"))
filter(diamonds, carat > 0.3 | color == "J")

# arrange - sorting (sql - order by)
arrange(diamonds, desc(price))

########################### operator %>% - for pretty code

# for example such function (iris - table from R standart collection)
select(arrange(filter(iris, Petal.Length > 1.7), Sepal.Length), 
       Sepal.Length, Sepal.Width)
# or by steps
filtered_iris <- filter(iris, Petal.Length > 1.7)
arranged_iris <- arrange(filtered_iris, Sepal.Length)
selected_iris <- select(arranged_iris, Sepal.Length, Sepal.Width)

#operator %>% - simplify code by union separate steps
iris %>% 
  filter(Petal.Length > 1.7) %>% 
  arrange(Sepal.Length) %>% 
  select(Sepal.Length, Sepal.Width)

#one more example
mtcars %>%     
  select(mpg, am, vs, hp) %>%     
  arrange(-mpg) %>%     
  filter(mpg > 14, hp > 100) %>%     
  slice(1:10) %>%     
  rename('Miles per gallon' = mpg,'Gross horsepower' = hp)

#remarks for this operator
x%>%y = f(x)
x%>%f(y) = f(x,y)
x%>% f(y, param=.) = f(y, param=x)

########################### Big story about joins:

# join
inner_join(x, y, by = NULL, copy = FALSE)
inner_join(diamonds[1:2000,], diamonds[2000:4000,], by = c("cut","color"))

#left_join()
#right_join()
#full_join()

#totaly selfish join(select first table)
semi_join(x, y, by = NULL, copy = FALSE, ...)
anti_join(x, y, by = NULL, copy = FALSE, ...)

#join tables
total_sample<-merge(x=sample_table1,y=sample_table2,by=c("dealno","contractsubject"))
rm(sample_table1)
rm(sample_table2)
gc()

## join BIG TABLES
#nomatch = 0 - for inner join
total_sample<-as.data.table(total_sample)
setkey(total_sample, dealno, contractsubject)
setkey(sample_table2,dealno,contractsubject)
total_sample<-total_sample[sample_table2, nomatch = 0]
gc()

########################### removing and changing data
# rename - only rename colums
# mutate - change content in colums
rename(diamonds, new_cut = cut,new_price = price)
m <- mutate(diamonds, 
            sqrt_price = sqrt(price), 
            log_carat = log(carat))

mutate(mtcars, am = factor(am), vs = factor(vs))

## rm data - remove objects (variables, vectors, dataframes, etc) from environment
rm(total_sample) 
gc() #- garbage collector, clear operative memory after removing objects

## delete columns
sample_client_parms[,client_position:=NULL]
total_sample<-as.data.frame(total_sample) 
total_sample$client_position <- NULL
total_sample[81:146]<-NULL

## Replace NA
# one column
total_sample[is.na(total_sample$Client_mobphone_number), 'Client_mobphone_number'] <- -99

# all data
total_sample_without_NA <- data.frame(sapply(select(total_sample, ColumnName1:ColumnName2),
                                            function(x) replace(x,which(is.na(x)),-99)))

#from column to column by cicle
n1<- match("Days_from_first_loan_all_fromall",names(total_sample))
n2<- match("Days_from_last_closedloan_C_fromnotbanks",names(total_sample))
for (i in c(n1:n2))
{ total_sample[is.na(total_sample[i]), i] <- -99
}

# all data without NA
total_sample[complete.cases(total_sample),]

########################### select unique value
unique(total_sample$Client_education)

########################### String normalization
#delete spaces
library(stringr)
str_trim(" hello world ")
#"hello world"
str_trim(" hello world ", side = "left")
#"hello world "
str_trim(" hello world ", side = "right")
#" hello world"

#print w/o ""
print(str_trim(" hello world "))
print(str_trim(" hello world "), quote = FALSE)

########################### grep and grepl functions - search in strings

grep("[a-z]", letters) #result - index
grepl("[a-z]", letters) #result - TRUE/FALSE

gender <- c("M", "male ", "Female", "fem.")
grepl("m", gender)
grepl("m", gender, ignore.case = TRUE) #become not sensitive to caps lock

########################### Type of variables
# class() - - The 'atomic' modes are numeric, complex, character and logical
# typeof() - The "type" of object from R's point of view
x <- 1
print(c(class(x),  typeof(x)))
x <- cars #cars - table from R standart collection
print(c(class(x), typeof(x)))

########################### IV
library(devtools)
install_github("riv","tomasgreif") # for install from Github use manual from S:\0.Learning\R\Github_packages
library(woe)

options(digits = 2)
iv.mult(as.data.frame(total_sample),"Bad_ff",vars=c("Test_cat_km"))

options(digits = 7)
iv.mult(total_sample[c(13,15:22)],"Bad_ff",TRUE)
iv.mult(total_sample[c(13,15:22)],"Bad_ff")

########################### Binning
# Load library and its dataset
install.packages(smbinning)
library(smbinning) # Load package and its data

# Example: Optimal binning
result=smbinning(df=total_sample,y="Bad_ff",x="Client_age") # Run and save result
result$ivtable # Tabulation and Information Value
result$iv # Information value
result$bands # Bins or bands
result$ctree # Decision tree

smbinning.factor(total_sample,"Bad_ff", "Client_dti_cat")

#create category
#with manual cutoff
t<-total_sample$Client_age 
total_sample$Client_age_cat1<-cut(t, c(min(t),0,5,10,15,20,25,30, max(t)),include.lowest = TRUE, right = FALSE)
rm(t)

#by equal length
library(Hmisc)
table(cut2(t, m = length(t)/4))

#by equal count in category
library(classInt)
install.packages("classInt")
classIntervals(t, 4, style = 'quantile')

eq<-classIntervals(t, 5, style = 'quantile')


#save good cutoffs
getwd()
setwd("C:/Users/Nachos/Desktop/work_SCORING/rdata")
save(eq, km, sb, file = "Client_workphone_number.rda")
rm(eq)
rm(km)
rm(sb)
load(file = "Client_workphone_number.rda")

#use cutoffs from file
t<-total_sample$Client_age
setwd("C:/Users/Nachos/Desktop/work_SCORING/rdata")
load(file = "Client_age.rda")
total_sample$Client_age_cat_eq<-as.factor(gsub(",", ";",cut(t, eq$brks,include.lowest = TRUE, right = FALSE)))
total_sample$Client_age_cat_km<-as.factor(gsub(",", ";",cut(t, km$brks,include.lowest = TRUE, right = FALSE)))
total_sample$Client_age_cat_sb<-as.factor(gsub(",", ";",cut(t, sb$bands,include.lowest = TRUE, right = FALSE)))
rm(eq)
rm(km)
rm(sb)
gc()









