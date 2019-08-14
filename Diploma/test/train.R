setwd('D:/moneyveo')
#detach(package:plyr) 
# Data manipulation
library(dplyr)
library(tibble)
library(tidyr)
library(tibble)
library(tidyverse)
library(knitr)
library(data.table)

# Visualization
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(gtable)
library(gridExtra)
library(corrplot)
library(grid)

# Modeling
library(caret)
library(caretEnsemble)
library(mice)
library(classInt)
library(RColorBrewer)
library(party)
library(rpart)
library(rpart.utils)
library(rpart.plot)
library(C50)
library(randomForest)
library(lattice)
library(ape)
library(glmnet)
library(caroline)
library(ROCR)
library(MLmetrics)
library(VIM)
library(GGally)
library(MissMech)
library(InformationValue)
library(smbinning)
library(xgboost)
library(parallel)
library(woeBinning)
library(agricolae) # kruskal
library(corrplot)


work_data <- fread("Train_Sample.csv", sep=";", na.strings=getOption(" ","NA"), dec = ".")
test <- fread("Test_Sample.csv", sep=";", na.strings=getOption(" ","NA"), dec = ".")

#####
# Exploratory Analysis & EDA
#####

glimpse(work_data)

### Try to know Bad Rate of the data
ggplot(work_data, aes(Status)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
  scale_y_continuous(labels=scales::percent) + 
  geom_text(aes( y = ((..count..)/sum(..count..)),
                 label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.001) +
  theme(axis.text.x = element_text(vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
  labs( y = "", x = "BR")

# The data a little bit skewed, it's a normal situation for credit scoring

### Check missing data
# [%,#] missing
options(scipen = 99)
kable(data.frame(obs = nrow(work_data),
                 qty_NA = colSums(sapply(work_data, is.na)),
                 prop_NA = colSums(sapply(work_data, is.na))/nrow(work_data)), digits = 3)

# data doesn`t have missing values


### Analysis of charater variables

work_data <- work_data %>% 
  mutate_if(is.character, as.factor) 

char_varnames <- work_data %>% 
  select_if(is.factor) %>%
  names()

plop1 <- ggplot(work_data, aes(x = work_data[ ,char_varnames[1]], fill = as.factor(Status))) +
  geom_bar(stat='count', position='stack') +
  labs(x = char_varnames[1]) +
  theme(legend.position = "none")    

plop2 <- ggplot(work_data, aes(x = work_data[ ,char_varnames[2]], fill = as.factor(Status))) +
  geom_bar(stat='count', position='stack') +
  labs(x = char_varnames[2]) +
  theme(legend.position = "none")

plop3 <- ggplot(work_data, aes(x = work_data[ ,char_varnames[3]], fill = as.factor(Status))) +
  geom_bar(stat='count', position='stack') +
  labs(x = char_varnames[3]) +
  theme(legend.position = "none")

grid.arrange(plop1, plop2, plop3, ncol=2)


## try to union category in the variables based by variable 'Status' 

# par53
# 
# of course, distribution variable 'Status' of every category unnormal, 
# so to compare this distributions we must use non-parameteric test(Kruskal-Wallis)
kruskal_test = kruskal(work_data$Status, work_data$par53,console = TRUE)

work_data = work_data %>%
  mutate(par53_new1 = ifelse(par53 %in% c('Unknown','Mozilla','Firefox','Opera'), 'Mozilla+',
                             ifelse(par53 %in% c('Chrome','InternetExplorer'), 'Chrome+', 'Safari+')))

# IV = 0.01167     IV(as.factor(work_data[,'par53_new2']), as.factor(work_data[,'Status']), valueOfGood = 1)[1]
# 
# IV is not amazing) let`s try somethig else
woebin1 = data.frame(woe.binning(work_data, 'Status', 'par53', 
                                     min.perc.total=0.05, min.perc.class=0, stop.limit=0.1,
                                     abbrev.fact.levels=50, event.class=1)[[2]][1:2])
names(woebin1) <- c('par53_new', 'par53')
woebin1 = woebin1 %>%
  mutate(par53_new = ifelse(par53_new == 'Firefox + misc. level neg.', 'Firefox+',
                            ifelse(par53_new == 'misc. level pos. + Safari', 'Safari+', 'Chrome')))
work_data[,'par53'] <- as.character(work_data[,'par53'])
work_data[,'par53_new'] <- inner_join(work_data, woebin1, by = 'par53')['par53_new']
work_data[,'par53_new'] <- as.factor(work_data[,'par53_new'])

# it's a little better:
# IV = 0.013 IV(as.factor(work_data[,'par53']), as.factor(work_data[,'Status']), valueOfGood = 1)[1]

plop1 <- ggplot(work_data, aes(x = par53_new, fill = as.factor(Status))) +
  geom_bar(stat='count', position='stack') +
  theme(legend.position = "none") 

work_data %>%
  group_by(par53_new2) %>%
  summarise(proc = n()/nrow(work_data),
            BR = sum(Status)/n(),
            neg_qty = sum(Status)/nrow(work_data))

# 'Safari+' has a little amount of 'neg_qty',
# so in the future a big chance, that no one 'bad' go to this category => model will lose stability
# => union 'Safari+' with other categories
work_data = work_data %>%
  mutate(par53_new = ifelse(par53_new %in% c('Mozilla+'), 'Mozilla+','Chrome'))

test = test %>%
  mutate(par53_new = ifelse(par53 %in% c('Unknown','Mozilla','Firefox','Opera'), 'Mozilla+','Chrome'))

# par61
woebin2 = as.data.frame(woe.binning(work_data, 'Status', 'par61',
                                    min.perc.total=0.05, min.perc.class=0, stop.limit=0.1,
                                    abbrev.fact.levels=50, event.class=1)[[2]][1:2])
names(woebin2) <- c('par61_new', 'par61')
woebin2 = woebin2 %>%
  mutate(par61_new = ifelse(par61_new == 'misc. level neg.', 'rare',
                            ifelse(par61_new == 'ukr.net + misc. level pos. + gmail.com', 'gmail+ukr.net', 'mail.ru+yandex.ru')))
work_data[,'par61'] <- as.character(work_data[,'par61'])
work_data[,'par61_new'] <- inner_join(work_data, woebin2, by = 'par61')['par61_new']
work_data[,'par61_new'] <- as.factor(work_data[,'par61_new'])

# IV = 0.0136 IV(as.factor(work_data[,'par61_new']), as.factor(work_data[,'Status']), valueOfGood = 1)[1]
test[,'par61_new'] <- inner_join(test, woebin3, by = 'par61')['par61_new']

# par71
woebin3 = as.data.frame(woe.binning(work_data, 'Status', 'par71',
                                    min.perc.total=0.05, min.perc.class=0, stop.limit=0.1,
                                    abbrev.fact.levels=50, event.class=1)[[2]][1:2])
names(woebin3) <- c('par71_new', 'par71')
woebin3 = woebin3 %>%
  mutate(par71_new = ifelse(par71_new == 'misc.levelpos. ggl.cm. yndx. mnyv. rdr.slsdblr.cm.', 'moneyveo+',
                            ifelse(par71_new == 'misc. level neg.', 'Not_available', 'else')))

work_data[,'par71'] <- as.character(work_data[,'par71'])
work_data[,'par71_new'] <- inner_join(work_data, woebin3, by = 'par71')['par71_new']
work_data[,'par71_new'] <- as.factor(work_data[,'par71_new'])

# IV = 0.0247 IV(as.factor(work_data[,'par71_new']), as.factor(work_data[,'Status']), valueOfGood = 1)[1]

test[,'par71_new'] <- inner_join(test, woebin3, by = 'par71')['par71_new']



plop1 <- ggplot(work_data, aes(x = par53_new, fill = as.factor(Status))) +
  geom_bar(stat = 'count', position = 'stack') +
  theme(legend.position = "none")    

plop2 <- ggplot(work_data, aes(x = par61_new, fill = as.factor(Status))) +
  geom_bar(stat='count', position='stack') +
  theme(legend.position = "none")

plop3 <- ggplot(work_data, aes(x = par71_new, fill = as.factor(Status))) +
  geom_bar(stat='count', position='stack') +
  theme(legend.position = "none")

grid.arrange(plop1, plop2, plop3, ncol=2)

# cleaning)
work_data <- select(work_data, -c(par53, par61, par71))
test <- select(test, -c(par53, par61, par71))
rm(woebin1, woebin2, woebin3, plop1, plop2, plop3)
gc()
#save(work_data, file = "work_data_before_var_selection.rda"), save(test, file = "test_before_var_selection.rda")
#load(file = "work_data_before_var_selection.rda"), load(file = "test_before_var_selection.rda")










### Analysis of int/num variables

# Correlation Matrix
cat("There are",length(work_data[ ,-(221:223)]),"numeric variables")
ncorII <- cor(work_data[ ,-(221:223)] ,use = "pairwise.complete.obs")
round(ncorII,2)
ncorII_sorted <- as.matrix(sort(ncorII[,"Status"], decreasing=TRUE))
ncorH <- names(which(apply(ncorII_sorted,1,function(x)abs(x) > .1)))
ncorIII <- ncorII[ncorH,ncorH]
corrplot.mixed(ncorIII,tl.col="black",tl.pos="lt",tl.cex=.7,cl.cex=.7,number.cex=.5)

# There're correlation in the data

# 



train_rF <- work_data
train_rF$Status <- as.factor(train_rF$Status)

set.seed(1)
rF1 <- randomForest(Status~.,  data = work_data, importance=TRUE)

rF1Imp <- rF1$importance 
dt_Imp <- data.table(Name = rownames(rF1Imp), IncMSE = rF1Imp[,1])
dt_Imp <- dt_Imp[order(-IncMSE),,]
ggplot(dt_Imp[1:20,], aes(reorder(x = Name, IncMSE), y=IncMSE, fill=IncMSE)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + theme(legend.position = "none")







# Libraries
library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

# Data
data("Sonar")
str(Sonar)

# Feature Selection
set.seed(111)
boruta <- Boruta(Class ~ ., data = Sonar, doTrace = 2, maxRuns = 500)
print(boruta)
plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)

# Tentative Fix
bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)

# Data Partition
set.seed(222)
ind <- sample(2, nrow(Sonar), replace = T, prob = c(0.6, 0.4))
train <- Sonar[ind==1,]
test <- Sonar[ind==2,]

# Random Forest Model
set.seed(333)
rf60 <- randomForest(Class~., data = train)

# Prediction & Confusion Matrix - Test
p <- predict(rf60, test)
confusionMatrix(p, test$Class)
                           
                           
                           
                           
                           
                           
                           
                           
   ## Stratified Random Sampling(test, train)
set.seed(123)
div_part_1 <- createDataPartition(y = work_data$Status, p = 0.1, list = F)
test1 <- work_data[div_part_1,]

set.seed(456)
div_part_2 <- createDataPartition(y = work_data$Status[-div_part_1], p = 0.111, list = F)
test2 <- work_data[div_part_2,]

set.seed(789)
div_part_3 <- createDataPartition(y = work_data$Status[-c(div_part_1,div_part_2)], p = 0.124, list = F)
test3 <- work_data[div_part_3,]

prop.table(table(test1$Status))
prop.table(table(test2$Status))
prop.table(table(test3$Status))
                           
                           
                           
                           

library(Boruta)

set.seed(123)
div_part_1 <- createDataPartition(y = work_data$Status, p = 0.1, list = F)
test1 <- work_data[div_part_1,]

set.seed(456)
div_part_2 <- createDataPartition(y = work_data$Status[-div_part_1], p = 0.111, list = F)
test2 <- work_data[div_part_2,]

set.seed(789)
div_part_3 <- createDataPartition(y = work_data$Status[-c(div_part_1,div_part_2)], p = 0.124, list = F)
test3 <- work_data[div_part_3,]

prop.table(table(test1$Status))
prop.table(table(test2$Status))
prop.table(table(test3$Status))


# Feature Selection
set.seed(111)
boruta <- Boruta(Status ~ ., data = test1, doTrace = 2)
save(boruta, file = 'boruta.rda')
print(boruta)


norm_100  <- function(x){
  round((x-min(x))/(max(x)-min(x))*100, 2)
  } 
  

Boruta_selection <- data.frame(variale = rownames(attStats(boruta))[attStats(boruta)['decision']!='Rejected'],
                               decision = attStats(boruta)['decision'][attStats(boruta)['decision']!='Rejected'],
                               empirity_imp = attStats(boruta)['normHits'][attStats(boruta)['decision']!='Rejected'],
                               norm_imp = norm_100(attStats(boruta)['normHits'][attStats(boruta)['decision']!='Rejected']))
                           
                           
                           

                        

