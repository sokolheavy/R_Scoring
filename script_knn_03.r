
Метод к-го ближайшего соседа (knn)
+ простейшая валидаци

setwd("F:/scoring/KNN/R_занятие_8_1_knn")
sales <- read.table("DISCRIM_0_new_R.txt", header=T, sep=";")

summary(sales)
sales[1:5, ]

#  Не рекомендуется
#  head(sales)
#  tail(sales)


dim(sales)
class(sales[ , 6])


#  Случайная подвыборка
set.seed(14)
test.num <- sample(1:nrow(sales), 50, replace = FALSE)

#  Тестовая выборка
test <- sales[test.num, 2:5]

#  Обучающая выборка
train <- sales[-test.num, 2:5]

#  Код класса для обучающей выборки
cl <- sales[-test.num, 6]

#  Присоединяю библиотеку class
library(class)

#  Распознаю класс объектов из тестовой выборки
zzz1 <- knn(train, test, cl, k = 3)

#  зачем можно задавать prob=TRUE


#  Проверяю соответствие истинного и распознанного классов
table(zzz1, sales[test.num, 6])



#------------------------------
#  Как определять значение k?
#  Кросс-валидация - простейший вариант
#  смотри также пакет caret

a <-rep(0,15)

for (i in 1:15)
{
  zzz <- knn(train, test, cl, k = i)
  a[i] <- summ(zzz != sales[test.num, 6])
}

a

##testing

#  Случайная подвыборка
set.seed(3476)
test.num <- sample(1:nrow(data), 2000, replace = FALSE)

#  Тестовая выборка
test_knn <- data[test.num, 1:12]

#  Обучающая выборка
train_knn <- data[-test.num, 1:12]

#  Код класса для обучающей выборки
cl <- data[-test.num, 1]

#  Присоединяю библиотеку class
library(class)

#  Распознаю класс объектов из тестовой выборки
zzz1 <- knn(train_knn, test_knn, cl, k = 3)

#  зачем можно задавать prob=TRUE


#  Проверяю соответствие истинного и распознанного классов
table(zzz1, test$target_for_calc)









