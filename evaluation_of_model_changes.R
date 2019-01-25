#setwd("E:/scoring")

getwd()

library(ROCR)
library(ggplot2)
library(MLmetrics)
library(smbinning)
library(dplyr)
#data_log_regr <- read.csv2("WOE_CC_no_CSF_else.csv")

#recall column with "target",take universall name(target can be 'target_for_calc','target_for_calc_30' and so on) 
names(data_log_regr)[grep("target", names(data_log_regr))]<-"target"


#generate "data_log_regr" with first column "target" and another columns are sorted alphabetically
data_log_regr<-cbind(data_log_regr$target,(data_log_regr[ , sort(names(data_log_regr[,-which( colnames(data_log_regr)=="target")]))]))

#recall first column,cauth automatically programe calls it "data$target"
names(data_log_regr)[1]<-"target"

# if exists rows with Inf value - delete them 
#data_log_regr <- data_log_regr[!is.infinite(rowSums(data_log_regr)),]

# target name:
measurevar <- "target"




train <- data_log_regr
############### create cross validation

V <- 10
T <- 3
TrControl <- trainControl(method = "repeatedcv",
                          number = V,
                          repeats = T)

Grid <- expand.grid(train[,-grep("target", names(train))])

seed <- 123

# func to compute an accuracy
#postResample can be used to estimate the root mean squared error (RMSE)
ErrsCaret <- function(Model, Name) {
  Errs <- data.frame(t(postResample(predict(Model, newdata = train), train[["target"]])),
                     Resample = "None", model = Name)
  rbind(Errs, data.frame(Model$resample, model = Name))
}

Errs <- data.frame()
CaretLearnAndDisplay <- function (Errs, Name, Formula, Method, ...) {
  set.seed(seed)
  Model <- train(as.formula(Formula), data = train, method = Method, trControl = TrControl, ...)
  Pred <- predict(Model, newdata = Grid)
  Errs <- rbind(Errs, ErrsCaret(Model, Name))
}






























#the end 

## Result
ErrCaretAccuracy <- function(Errs) {
  Errs <- group_by(Errs, model)
  cbind(dplyr::summarize(Errs, mAccuracy = mean(Accuracy, na.rm = TRUE), mKappa = mean(Kappa, na.rm = TRUE),
                         sdAccuracy = sd(Accuracy, na.rm = TRUE), sdKappa = sd(Kappa, na.rm = TRUE)))
}

ErrAndPlotErrs <- function(Errs) {
  ErrCV <- ErrCaretAccuracy(dplyr::filter(Errs, !(Resample == "None")))
  ErrCV <- transmute(ErrCV, AccuracyCV = mAccuracy, AccuracyCVInf = mAccuracy - 2 * sdAccuracy/sqrt(T * V),
                     AccuracyCVPAC = mAccuracy - sdAccuracy,
                     model = model)
  ErrEmp <- dplyr::select(dplyr::filter(Errs, (Resample == "None")), Accuracy, model)
  Err <- dplyr::left_join(ErrCV, ErrEmp, by = "model")
  print(ggplot(data = reshape2::melt(Err, "model"), 
               aes(x = model, y = value, color = variable)) + 
          geom_point(size = 5) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                plot.margin = grid::unit(c(1, 1, .5, 1.5), "lines")))
  Err
}

Err <- ErrAndPlotErrs(Errs)
