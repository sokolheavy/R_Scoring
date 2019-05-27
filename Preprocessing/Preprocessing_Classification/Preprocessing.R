https://www.kaggle.com/erikbruin/titanic-2nd-degree-families-and-majority-voting

https://www.kaggle.com/vincentlugat/titanic-data-analysis-rf-prediction-0-81818



# 1) Exploratory Analysis
1. Age

2. Sex

3. Age vs Sex

4. Pclass vs Sex

5. Pclass vs Sex vs Age

6. Fare vs Pclass


#  2). Data Processing and Exploratory Analysis 2
1. New Variable : Title (From Name)

2. New Variable : Family Size (From Name, SibSp and Parch)

3. Processing Embarked (Replace missing values by most common value = S)

4. Processing Fare (Replace missing value by Pclass = 3 ''s median)
                      
5. Processing Age (Replace missing values by Title''s median)

6. New Variable : Child (From Age)

7. Correlogram Matrix


#_____________________________Package + Data___________________________________
#______________________________________________________________________________

# Package
suppressMessages(library('ggplot2'))
suppressMessages(library('ggthemes')) 
suppressMessages(library('scales')) 
suppressMessages(library('dplyr'))
suppressMessages(library('randomForest'))
suppressMessages(library('corrplot'))
suppressMessages(library('plyr'))
suppressMessages(library(InformationValue))
suppressMessages(library(cooccur))

#Loading Data

train <- read.csv('https://raw.githubusercontent.com/Sokolheavy/R_Scoring/master/Preprocessing/Preprocessing_Classification/train.csv', stringsAsFactors = F)
test  <- read.csv('https://raw.githubusercontent.com/Sokolheavy/R_Scoring/master/Preprocessing/Preprocessing_Classification/test.csv', stringsAsFactors = F)

full  <- bind_rows(train, test) # test + train

full$Sex <- as.factor(full$Sex)
full$Survived <- as.factor(full$Survived)
full$Pclass <- as.ordered(full$Pclass)

sum(duplicated(train))
sum(duplicated(test))
options( warn = -1 )

str(full)

summary(full)
# dev.off

#______________________________________________________________________________
#_____________________________Function for Analysis_____________________________
#______________________________________________________________________________

## IV - statistic table
iv <- function(column, target){
iv_table <- data.frame(IV=IV(X=column, Y=target)[1])
column <- as.factor(column)
target <- as.factor(target)

# Add strength of variables
iv_table$Strength <- ifelse(iv_table$IV>=1, "Suspicious",
                            ifelse(iv_table$IV>=.5, "Very strong",
                                   ifelse(iv_table$IV>=.2, "Strong",
                                          ifelse(iv_table$IV>=.1, "Average",
                                                 ifelse(iv_table$IV>=.02, "Weak", "Very weak")))))

iv_table
}

prop.table(table(full$Fsize))*100

prop.1 <- with(full, table(Survived, Fsize)) %>% 
  prop.table(margin = 1)

prop.2 <- with(full, table(Survived, Fsize)) %>% 
  prop.table(margin = 2)

#______________________________________________________________________________
#_____________________________Exploratory Analysis_____________________________
#______________________________________________________________________________

## 2.1

# Age vs Survived
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + 
  theme_few() +
  xlab("Age") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Age vs Survived")

## 2.2

# Sex vs Survived
ggplot(full[1:891,], aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = 'dodge')+
  theme_few() +
  xlab("Sex") +
  ylab("Count") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Sex vs Survived")

## 2.3. Age vs Sex

#Sex vs Survived vs Age 
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + 
  theme_few() +
  xlab("Age") +
  ylab("Count") +
  facet_grid(.~Sex)+
  scale_fill_discrete(name = "Survived") + 
  theme_few()+
  ggtitle("Age vs Sex vs Survived")


## 2.4. Pclass vs Sex

# Pclass vs Survived
ggplot(full[1:891,], aes(Pclass, fill = factor(Survived))) + 
  geom_bar(stat = "count")+
  theme_few() +
  xlab("Pclass") +
  facet_grid(.~Sex)+
  ylab("Count") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Pclass vs Sex vs Survived")


## 2.5. Pclass vs Sex vs Age

ggplot(full[1:891,], aes(x = Age, y = Sex)) + 
  geom_jitter(aes(colour = factor(Survived))) + 
  theme_few()+
  theme(legend.title = element_blank())+
  facet_wrap(~Pclass) + 
  labs(x = "Age", y = "Sex", title = "Pclass vs Sex vs Age vs Survived")+
  scale_fill_discrete(name = "Survived") + 
  scale_x_continuous(name="Age",limits=c(0, 81))


## 2.6. Fare vs Pclass

#Fare
ggplot(full[1:891,], aes(x = Fare, y = Pclass)) + 
  geom_jitter(aes(colour = factor(Survived))) + 
  theme_few()+
  theme(legend.title = element_blank())+
  labs(x = "Age", y = "Pclass", title = "Fare vs Pclass")+
  scale_fill_discrete(name = "Survived") + 
  scale_x_continuous(name="Fare", limits=c(0, 270), breaks=c(0, 40, 80, 120, 160, 200, 240, 280))







#______________________________________________________________________________
#______________________Data processing and ___________________________________
#_________________________exploratory analysis 2______________________________

## 3.1. New Variable : Title (From Name)


#__________________________________Title_______________________________________
# Extract titles
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Titles by Sex
table(full$Sex, full$Title)

# Reassign rare titles
officer <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
royalty <- c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')

# Reassign mlle, ms, and mme, and rare
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% royalty]  <- 'Royalty'
full$Title[full$Title %in% officer]  <- 'Officer'

full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])


#graph title
ggplot(full[1:891,], aes(Title,fill = factor(Survived))) +
  geom_bar(stat = "count")+
  xlab('Title') +
  ylab("Count") +
  scale_fill_discrete(name = " Survived") + 
  ggtitle("Title vs Survived")+
  theme_few()


#____________________________Family Size________________________________
full$Pclass <- as.factor(full$Pclass)
iv(train$Pclass, train$Survived)

train$Fsize <- train$SibSp + train$Parch + 1

#Family
# Family Size
full$Fsize <- full$SibSp + full$Parch + 1

ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  xlab('Family Size') +
  ylab("Count") +
  theme_few()+
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Family Size vs Survived")

# FsizeD
full$FsizeD[full$Fsize == 1] <- 'Alone'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'Small'
full$FsizeD[full$Fsize > 4] <- 'Big'

mosaicplot(table(full$FsizeD, full$Survived), main='FsizeD vs Survived', ylab="Survived",xlab="FsizeD",col = hcl(c(50, 120)),)

# FsizeD
train$FsizeD[train$Fsize == 1] <- 'Alone'
train$FsizeD[train$Fsize < 5 & train$Fsize > 1] <- 'Small'
train$FsizeD[train$Fsize > 4] <- 'Big'

train$isalone <- ifelse(train$Fsize==1,1,0)

iv(as.factor(train$isalone), train$Survived)
table(train$isalone, train$Survived)


## 3.3. Processing Embarked (Replace missing values by most common value = S)

#________________________________Embarked______________________________________
# 2 missing datas : input S
full[c(62, 830), 'Embarked']

full$Embarked[c(62, 830)] <- 'S'

ggplot(full[1:891,], aes(Pclass, fill = factor(Survived))) + 
  geom_bar(stat = "count")+
  theme_few() +
  xlab("Pclass") +
  ylab("Count") +
  facet_wrap(~Embarked) + 
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Embarked vs Pclass vs Survived")





#_________________________________Fare_________________________________________ 

# 3.4. Processing Fare (Replace missing value by Pclass = 3 s median)

full[1044, ]

ggplot(full[full$Pclass == '3', ], 
       aes(x = Fare)) +
  geom_density(fill = 'lightgrey', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='darkred', linetype='dashed', lwd=1) +
  xlab('Fare') +
  ggtitle("Pclass = 3")+
  ylab("Density") +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

full$Fare[1044] <- median(full[full$Pclass == '3', ]$Fare, na.rm = TRUE)


ggplot(full, 
       aes(x = log(Fare))) +
  geom_density(fill = 'lightgrey', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='darkred', linetype='dashed', lwd=1) +
  xlab('Fare') +
  ggtitle("Pclass = 3")+
  ylab("Density") +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

#__________________________________Child__________________________________________

# 3.6. New Variable : Child (From Age)

full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

ggplot(full[1:891,][full[1:891,]$Child == 'Child', ], aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count") + 
  xlab("Sex") +
  ylab("Count") +
  facet_wrap(~Pclass)+
  scale_fill_discrete(name = "Survived") +
  ggtitle("Child vs Sex vs Pclass vs Survived")+
  theme_few()

table(full$Child, full$Survived)


#__________________________________ Pclass vs Sex__________________________________________

# 3.7. New Variable : Child (From Age)

p3 <- ggplot(train, aes(x = Pclass, fill = Pclass)) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Pclass, train data') + geom_label(stat='count', aes(label=..count..)) +
  theme(legend.position="none") + theme_grey()     
p4 <- ggplot(train[!is.na(train$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='dodge') + labs(x = 'Training data only') +
  theme(legend.position="none") + theme_grey()
p5 <- ggplot(train[!is.na(train$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'Training data only', y= "Count") + facet_grid(.~Sex) +
  theme(legend.position="none") + theme_grey()
p6 <- ggplot(train[!is.na(train$Survived),], aes(x = Pclass, fill = Survived)) +
  geom_bar(stat='count', position='fill') +
  labs(x = 'Training data only', y= "Percent") + facet_grid(.~Sex) +
  theme(legend.position="none") + theme_grey()

grid.arrange(p3, p4, p5, p6, ncol=2)


all$PclassSex[all$Pclass=='1' & all$Sex=='male'] <- 'P1Male'
all$PclassSex[all$Pclass=='2' & all$Sex=='male'] <- 'P2Male'
all$PclassSex[all$Pclass=='3' & all$Sex=='male'] <- 'P3Male'
all$PclassSex[all$Pclass=='1' & all$Sex=='female'] <- 'P1Female'
all$PclassSex[all$Pclass=='2' & all$Sex=='female'] <- 'P2Female'
all$PclassSex[all$Pclass=='3' & all$Sex=='female'] <- 'P3Female'
all$PclassSex <- as.factor(all$PclassSex)


