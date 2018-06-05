#загружаем данные из файла:
sample<-read.csv2("data_new.csv")

#смотрим на структуру файла:
head(sample)
str(sample)

#гистограмма по отдельным категориям:
#install.packages("ggplot2")
library(ggplot2)
ggplot(sample, aes(x = Family_status_ukr)) + geom_bar()
summary(sample$Family_status_ukr)

#гистограмма по численным переменным (с возможностью разбивки на определенное количество категорий)
hist(sample$Age_y)
hist(sample$Age_y,breaks = 20)

#статистика по категориальным
summary(sample$Family_status_ukr)
#статистика по численным
summary(sample$Age_y)

#удалить записи с определенным значением (через переменную)
delete_position <- sample$Gender != "Gender:_F"
sample2 <- sample[delete_position,]
table(sample$Gender)
table(sample2$Gender)

#удалить записи с определенным значением (короткая)


sample3<-head(sample[,1:10])

#изменить запись - численные
sample3$target_for_calc[sample3$target_for_calc == 1] <- 0

#изменить запись - категориальные
levels(sample3$Gender)[levels(sample3$Gender) == "Gender:_M"] <- "Male"

#переименовать категории
levels(sample3$Gender)
levels(sample3$Gender)<- c("Female", "Male")
levels(sample3$Gender)

#удалить столбец


#Построение WoE-таблицы
library(plyr)
Gender_stat<-data.frame(count(sample[sample$target_60max12m == "good",], c("Gender") ),count(sample[sample$target_60max12m == "bad",], c("Gender") )[,2], count(sample, c("Gender"))[,2])
colnames(Gender_stat)<-c(colnames(Gender_stat)[1], "good","bad","total")
Total_stat<-summary(sample$target_60max12m)
Gender_stat <- data.frame(Gender_stat,"share of good" = Gender_stat$good/Total_stat["good"], "share of bad" = Gender_stat$bad/Total_stat["bad"], "share of total" = Gender_stat$total/sum(Total_stat),"BR" = Gender_stat$bad/Gender_stat$total)
Gender_stat <- data.frame(Gender_stat,"Woe" = log(Gender_stat$share.of.good/Gender_stat$share.of.bad))
Gender_stat

#Расчет IV
IV <- data.frame("char_name" = colnames(Gender_stat)[1], "IV" = sum((Gender_stat$share.of.good-Gender_stat$share.of.bad)*Gender_stat$Woe))
IV