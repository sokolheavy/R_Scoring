setwd('D:/moneyveo')
#detach(package:plyr) 
# Data manipulation
library(dplyr)
library(tibble)
library(tidyr)
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
library(classInt)
library(RColorBrewer)
library(randomForest)
library(ape)
library(glmnet)
library(caroline)
library(ROCR)
library(MLmetrics)
library(VIM)
library(GGally)
library(InformationValue)
library(smbinning)
library(xgboost)
library(parallel)
library(woeBinning)
library(agricolae)
library(corrplot)
library(Boruta)

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
### 
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
work_data[,'par53_new'] <- inner_join(work_data, woebin1, by = 'par53')['par53_new']

# it's a little better:
# IV = 0.013 IV(as.factor(work_data[,'par53']), as.factor(work_data[,'Status']), valueOfGood = 1)[1]

plop1 <- ggplot(work_data, aes(x = par53_new, fill = as.factor(Status))) +
  geom_bar(stat='count', position='stack') +
  theme(legend.position = "none") 

work_data %>%
  group_by(par53_new) %>%
  summarise(proc = n()/nrow(work_data),
            BR = sum(Status)/n(),
            neg_qty = sum(Status)/nrow(work_data))

# 'Safari+' has a little amount of 'neg_qty',
# so in the future a big chance, that no one 'bad' go to this category => model will lose stability
# => union 'Safari+' with other categories with similar Bad Rate
work_data = work_data %>%
  mutate(par53_new = ifelse(par53_new %in% c('Firefox+'), 'Firefox+','Chrome'))
work_data[,'par53_new'] <- as.factor(work_data[,'par53_new'])

test = test %>%
  mutate(par53_new = ifelse(par53 %in% c('Unknown','Mozilla','Firefox','Opera'), 'Firefox+','Chrome'))
test[,'par53_new'] <- as.factor(test[,'par53_new'])


# par61
woebin2 = as.data.frame(woe.binning(work_data, 'Status', 'par61',
                                    min.perc.total=0.05, min.perc.class=0, stop.limit=0.1,
                                    abbrev.fact.levels=50, event.class=1)[[2]][1:2])
names(woebin2) <- c('par61_new', 'par61')
woebin2 = woebin2 %>%
  mutate(par61_new = ifelse(par61_new == 'misc. level neg.', 'rare',
                            ifelse(par61_new == 'ukr.net + misc. level pos. + gmail.com', 'gmail+ukr.net', 'mail.ru+yandex.ru')))

work_data[,'par61_new'] <- inner_join(work_data, woebin2, by = 'par61')['par61_new']
work_data[,'par61_new'] <- as.factor(work_data[,'par61_new'])

# IV = 0.0136 IV(as.factor(work_data[,'par61_new']), as.factor(work_data[,'Status']), valueOfGood = 1)[1]
test[,'par61_new'] <- inner_join(test, woebin2, by = 'par61')['par61_new']
test[,'par61_new'] <- as.factor(test[,'par61_new'])

# par71
woebin3 = as.data.frame(woe.binning(work_data, 'Status', 'par71',
                                    min.perc.total=0.05, min.perc.class=0, stop.limit=0.1,
                                    abbrev.fact.levels=50, event.class=1)[[2]][1:2])
names(woebin3) <- c('par71_new', 'par71')
woebin3 = woebin3 %>%
  mutate(par71_new = ifelse(par71_new == 'misc.levelpos. ggl.cm. yndx. mnyv. rdr.slsdblr.cm.', 'moneyveo+',
                            ifelse(par71_new == 'misc. level neg.', 'Not_available', 'else')))

work_data[,'par71_new'] <- inner_join(work_data, woebin3, by = 'par71')['par71_new']
work_data[,'par71_new'] <- as.factor(work_data[,'par71_new'])

# IV = 0.0247 IV(as.factor(work_data[,'par71_new']), as.factor(work_data[,'Status']), valueOfGood = 1)[1]

test[,'par71_new'] <- inner_join(test, woebin3, by = 'par71')['par71_new']
test[,'par71_new'] <- as.factor(test[,'par71_new'])


plop1 <- ggplot(work_data, aes(x = par53_new, fill = as.factor(Status))) +
  geom_bar(stat = 'count', position = 'stack') +
  labs(x = 'par53_new') +
  theme(legend.position = "none")    

plop2 <- ggplot(work_data, aes(x = par61_new, fill = as.factor(Status))) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'par61_new') +
  theme(legend.position = "none")

plop3 <- ggplot(work_data, aes(x = par71_new, fill = as.factor(Status))) +
  geom_bar(stat='count', position='stack') +
  labs(x = 'par71_new') +
  theme(legend.position = "none")

grid.arrange(plop1, plop2, plop3, ncol = 2)


## Analysis of logical variables
## 
logical_varnames <- work_data %>% 
  select_if(is.logical) %>%
  names()

work_data <- work_data  %>% 
  mutate_if(is.logical, as.factor) 
test <- test  %>% 
  mutate_if(is.logical, as.factor)


plot4 <- ggplot(work_data, aes(x = work_data[ ,logical_varnames[1]], fill = as.factor(Status))) +
              geom_bar(stat='count', position='stack') +
              labs(x = logical_varnames[1]) +
              theme(legend.position = "none")

plot5 <- ggplot(work_data, aes(x = work_data[ ,logical_varnames[2]], fill = as.factor(Status))) +
              geom_bar(stat='count', position='stack') +
              labs(x = logical_varnames[2]) +
              theme(legend.position = "none")

plot6 <- ggplot(work_data, aes(x = work_data[ ,logical_varnames[3]], fill = as.factor(Status))) +
              geom_bar(stat='count', position='stack') +
              labs(x = logical_varnames[3]) +
              theme(legend.position = "none")

grid.arrange(plot4, plot5, plot6, ncol = 2)
# This variables likely to be not informative

# cleaning)
work_data <- select(work_data, -c(par53, par61, par71))
test <- select(test, -c(par53, par61, par71))
rm(woebin1, woebin2, woebin3, plop1, plop2, plop3, plo4, plo5, plo6)
gc()

#save(work_data, file = "work_data_before_var_selection.rda"), save(test, file = "test_before_var_selection.rda")
#load(file = "work_data_before_var_selection.rda"), load(file = "test_before_var_selection.rda")


## Analysis of int/num variables
## 
Num_varnames <- work_data[ ,-1] %>% 
  select_if(is.numeric) %>%
  names()

# Correlation Matrix
cat("There are",length(work_data[ ,Num_varnames]),"numeric variables")
ncorII <- cor(work_data[ ,Num_varnames] ,use = "pairwise.complete.obs")
round(ncorII,2)
ncorII_sorted <- as.matrix(sort(ncorII[,"Status"], decreasing=TRUE))
ncorH <- names(which(apply(ncorII_sorted,1,function(x)abs(x) > .1)))
ncorIII <- ncorII[ncorH,ncorH]
corrplot.mixed(ncorIII,tl.col="black",tl.pos="lt",tl.cex=.7,cl.cex=.7,number.cex=.5)

# There're correlation in the data



work_data_with_dumies <- createDummyFeatures(work_data, cols = c('par53', 'par61', 'par71'))
work_data_dummy <- dummyVars(~ par53_new + par61_new + par71_new, work_data)
work_data_with_dumies <- as.data.frame(predict(work_data_dummy, work_data))
work_data_with_dumies <- get.dummy(work_data, 'par53')
work_data_with_dumies <- dummy.data.frame(work_data,  names = c('par53', 'par61', 'par71'),  sep = ".")
## Stratified Random Sampling(test, train)
set.seed(123)
div_part_1 <- createDataPartition(y = work_data$Status, p = 0.01, list = F)
test1 <- work_data[div_part_1,]

set.seed(456)
div_part_2 <- createDataPartition(y = work_data_with_dumies$Status[-div_part_1], p = 0.111, list = F)
test2 <- work_data[div_part_2,]

set.seed(789)
div_part_3 <- createDataPartition(y = work_data_with_dumies$Status[-c(div_part_1,div_part_2)], p = 0.124, list = F)
test3 <- work_data[div_part_3,]

prop.table(table(test1$Status))
prop.table(table(test2$Status))
prop.table(table(test3$Status))

glimpse(work_data)

data_fac=data_char %>% mutate_if(is.character, as.factor)
train_rF <- work_data
train_rF$Status <- as.factor(work_data$Status)

set.seed(1)
rF1 <- randomForest(Status ~.,  data = train_rF, importance=TRUE)
#save(rF1, file = 'RF_var_selection.rda')

rF1Imp <- rF1$importance 
dt_Imp <- data.table(Name = rownames(rF1Imp), IncMSE = rF1Imp[,1])
dt_Imp <- dt_Imp[order(-IncMSE),,]

ggplot(dt_Imp[1:190,], aes(reorder(x = Name, IncMSE), y = IncMSE, fill = IncMSE)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + theme(legend.position = "none")


### Boruta
set.seed(111)
boruta <- Boruta(Status ~ ., data = test1, doTrace = 2)
#save(boruta, file = 'boruta.rda')
print(boruta)

plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)

# Tentative Fix
bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)


norm_100  <- function(x){
  round((x - min(x))/(max(x) - min(x))*100, 2)
} 

quantile(norm_100(attStats(boruta)['normHits'][attStats(boruta)['decision'] !='Rejected']), 
         prob = seq(0,1,.25))

quantile(norm_100(RF_select[,3]), 
         prob = seq(0,1,.25))

Bor_select <- data.frame(variale = rownames(attStats(boruta))[attStats(boruta)['decision'] !='Rejected'],
                   decision = attStats(boruta)['decision'][attStats(boruta)['decision'] !='Rejected'],
                   empirity_imp = attStats(boruta)['normHits'][attStats(boruta)['decision'] !='Rejected'],
                   norm_imp = norm_100(attStats(boruta)['normHits'][attStats(boruta)['decision'] !='Rejected']))
Bor_select['quantile'] <- ifelse(Bor_select[,4]>=100, 1,
                                 ifelse(Bor_select[,4]>=92.54, 2,
                                        ifelse(Bor_select[,4]>=50, 3, 4)))
  

RF_select <- data.frame(variale = dt_Imp[1:150][,1],
                        rf_empirity_imp = dt_Imp[1:150][,2],
                        rf_norm_imp = norm_100(dt_Imp[1:150][,2]))
names(RF_select) <- c('variale', 'empirity_imp', 'norm_imp')


RF_select['rf_quantile'] <- ifelse(RF_select[,3] >= 7.7, 1,
                                 ifelse(RF_select[,3] >= 2.67, 2,
                                        ifelse(RF_select[,3] >= 1, 3, 4)))

Bor_select <- Bor_select[Bor_select['quantile'] <= 3, ]
RF_select <- RF_select[RF_select['rf_quantile'] <= 3, ]
 
var_to_select <- full_join(Bor_select, RF_select, by='variale')[-1,1]

# data after variables selection:
work_data_vs <- (work_data[ ,c('Status', var_to_select)])
test_vs <- test[ ,var_to_select]


## Binning numerical variables

# Bin work_data and then 'bins' from work_data data go to the test
# !!! time consuming process
# *(func 'binning' from the 'helps_function.R' file)
binning(work_data_vs, test_vs)




#####
# Feature engineering & variable selection 
#####
# number of column of first binned variable 
variable_fc_bin <- 2
bin_ncol <- ncol(bin_data)

iv_table <- cbind.data.frame( variables = names(bin_data[variable_fc_bin:bin_ncol])
                              , IV = sapply(bin_data[variable_fc_bin:bin_ncol], function(x) round(IV(X=x, Y=bin_data$Status)[1],4))
                              , row.names = NULL) %>%
                              arrange(desc(IV)) %>%
                              transmute(row_number = row_number(), !!!.)

# Add strength of variables
iv_table$Strength <- ifelse(iv_table$IV>=1, "Suspicious",
                            ifelse(iv_table$IV>=.5, "Very strong",
                                   ifelse(iv_table$IV>=.2, "Strong",
                                          ifelse(iv_table$IV>=.1, "Average",
                                                 ifelse(iv_table$IV>=.02, "Weak", "Wery weak")))))


# Firstly delete variables with "Wery weak" IV,
# very seldom this variables can contribute significant influence into model
norm_IV_var <-  iv_table[iv_table$Strength != "Wery weak", ][ ,1]

bin_data <- bin_data[ ,norm_IV_var]
bin_test <- bin_test[ ,norm_IV_var]

# save(bin_data, file = 'bin_data.rda'), save(bin_test, file = 'bin_test.rda')
# load('bin_data.rda'), load('bin_test.rda')






install.packages('ROCR')
library(ROCR)

set.seed(123)
div_part_1 <- createDataPartition(y = work_data_vs$Status, p = 0.7, list = F)
train <- work_data_vs[div_part_1,] # 70% 
test <- work_data_vs[-div_part_1,] # 30% 

mat1 <- model.matrix(Status ~ . , data = train  ) # convert to numeric matrix
mat2 <- model.matrix(Status ~ . , data = test  )  # convert to numeric matrix

mod2 <-cv.glmnet(mat1, as.numeric(train$Status), alpha=1, family="binomial", type.measure = 'auc')


# Apply model to testing dataset
test$mod2_score <- predict(mod2, type='response', newx=mat2, s = 'lambda.min')
mod2_pred <- prediction(test$mod2_score, test$Status)
mod2_perf <- performance(mod2_pred,"tpr","fpr")

ev_df_m2 <- data.frame(Gini = round(((slot(performance(mod2_pred, measure = "auc"),"y.values")[[1]])*2 - 1)*100, 2),
                       AUC = round(performance(mod2_pred, measure = "auc")@y.values[[1]]*100, 2))

ev_df_m2 <- ggtexttable(ev_df_m2, rows = NULL, theme = ttheme(colnames.style = colnames_style(color = "white", fill = "#39568CFF"), base_size = 10))

acc2 <- 63.09
Gini2 <- 81.54
gini_plot <- ggplot(setNames(data.frame(mod2_perf@x.values, mod2_perf@y.values), c('x_val', 'y_val')), 
                    aes(x = x_val, y = y_val), color=sort_criterion) + 
  geom_line(aes(group=1), colour="#000099", size=1) + 
  geom_abline(color="gray") +
  ggtitle(paste("Gini=", round(Gini2,4), sep="")) +
  xlab("False Positive Rate") +
  ylab("True Positive Rate") +
  theme_bw(base_size = 20) +
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  theme(legend.position ="none")

png(filename="LASSO.png", res=150, width = 1000, height = 1000)
plot(mod2)
dev.off()


log(mod2$lambda.min)

coef(mod2, s=mod2$lambda.min)





save(mod2, file = 'mod2.rda')

#install.packages("Hmisc")
library("Hmisc")
#rcorr() - to compute the significance levels for pearson and spearman correlations

res <- rcorr(as.matrix(work_data_vs))

tableCorrMatrix <- function(corr, pval) {
  ut <- upper.tri(corr)
  data.frame(
    row = rownames(corr)[row(corr)[ut]],
    column = rownames(corr)[col(corr)[ut]],
    cor  =(corr)[ut],
    p = pval[ut]
  )
}

corrtable<-tableCorrMatrix(res$r, res$P)

#try to choose value that have significant corr
resultCORR<-subset(corrtable,abs(cor)>0.8)
corr_var <- unique(resultCORR[,1], resultCORR[,2])




set.seed(123)
div_part_1 <- createDataPartition(y = work_data_vs$Status, p = 0.3, list = F)
test_VIF <- work_data_vs[div_part_1,]

install.packages('fmsb')
library(fmsb)
library(caret)
vif_analysis <- vif_func(test_VIF, corr_var, thresh=10, trace=T)












