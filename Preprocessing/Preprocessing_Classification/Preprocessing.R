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
