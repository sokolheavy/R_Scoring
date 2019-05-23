# 1) Exploratory Analysis
# 1. Age

# 2. Sex

# 3. Age vs Sex

# 4. Pclass vs Sex

# 5. Pclass vs Sex vs Age

# 6. Fare vs Pclass


#  2). Data Processing and Exploratory Analysis 2
# 1. New Variable : Title (From Name)

# 2. New Variable : Family Size (From Name, SibSp and Parch)

# 3. Processing Embarked (Replace missing values by most common value = S)

# 4. Processing Fare (Replace missing value by Pclass = 3 ''s median)
                      
# 5. Processing Age (Replace missing values by Title''s median)

# 6. New Variable : Child (From Age)

# 7. Correlogram Matrix


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

#Loading Data

train <- read.csv('https://raw.githubusercontent.com/Sokolheavy/R_Scoring/master/Preprocessing/Preprocessing_Classification/train.csv', stringsAsFactors = F)
test  <- read.csv('https://raw.githubusercontent.com/Sokolheavy/R_Scoring/master/Preprocessing/Preprocessing_Classification/test.csv', stringsAsFactors = F)

full  <- bind_rows(train, test) # test + train

options( warn = -1 )

str(full)

summary(full)
# dev.off



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
