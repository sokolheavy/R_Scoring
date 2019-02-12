https://coursehunters.net/course/data-science-i-mashinnoe-obuchenie-s-r


Ìåòîä ê-ãî áëèæàéøåãî ñîñåäà (knn)
+ ïðîñòåéøàÿ âàëèäàöè

setwd("F:/scoring/KNN/R_çàíÿòèå_8_1_knn")
sales <- read.table("DISCRIM_0_new_R.txt", header=T, sep=";")

summary(sales)
sales[1:5, ]

#  Íå ðåêîìåíäóåòñÿ
#  head(sales)
#  tail(sales)


dim(sales)
class(sales[ , 6])


#  Ñëó÷àéíàÿ ïîäâûáîðêà
set.seed(14)
test.num <- sample(1:nrow(sales), 50, replace = FALSE)

#  Òåñòîâàÿ âûáîðêà
test <- sales[test.num, 2:5]

#  Îáó÷àþùàÿ âûáîðêà
train <- sales[-test.num, 2:5]

#  Êîä êëàññà äëÿ îáó÷àþùåé âûáîðêè
cl <- sales[-test.num, 6]

#  Ïðèñîåäèíÿþ áèáëèîòåêó class
library(class)

#  Ðàñïîçíàþ êëàññ îáúåêòîâ èç òåñòîâîé âûáîðêè
zzz1 <- knn(train, test, cl, k = 3)

#  çà÷åì ìîæíî çàäàâàòü prob=TRUE


#  Ïðîâåðÿþ ñîîòâåòñòâèå èñòèííîãî è ðàñïîçíàííîãî êëàññîâ
table(zzz1, sales[test.num, 6])



#------------------------------
#  Êàê îïðåäåëÿòü çíà÷åíèå k?
#  Êðîññ-âàëèäàöèÿ - ïðîñòåéøèé âàðèàíò
#  ñìîòðè òàêæå ïàêåò caret

a <-rep(0,15)

for (i in 1:15)
{
  zzz <- knn(train, test, cl, k = i)
  a[i] <- summ(zzz != sales[test.num, 6])
}

a

##testing

#  Ñëó÷àéíàÿ ïîäâûáîðêà
set.seed(3476)
test.num <- sample(1:nrow(data), 2000, replace = FALSE)

#  Òåñòîâàÿ âûáîðêà
test_knn <- data[test.num, 1:12]

#  Îáó÷àþùàÿ âûáîðêà
train_knn <- data[-test.num, 1:12]

#  Êîä êëàññà äëÿ îáó÷àþùåé âûáîðêè
cl <- data[-test.num, 1]

#  Ïðèñîåäèíÿþ áèáëèîòåêó class
library(class)

#  Ðàñïîçíàþ êëàññ îáúåêòîâ èç òåñòîâîé âûáîðêè
zzz1 <- knn(train_knn, test_knn, cl, k = 3)

#  çà÷åì ìîæíî çàäàâàòü prob=TRUE


#  Ïðîâåðÿþ ñîîòâåòñòâèå èñòèííîãî è ðàñïîçíàííîãî êëàññîâ
table(zzz1, test$target_for_calc)









