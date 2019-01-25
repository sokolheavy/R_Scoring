library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(h2o)
library(doFutur)
library(caret)
library(AppliedPredictiveModeling)
library(klaR)
library(shiny)
library(h2o)
library(rpart)
library(rpart.plot)
library(magrittr)
library(ggpubr)
library(Cubist)
library(C50)

data(twoClassData)

registerDoFuture()
plan(multiprocess)

twoClass=cbind(as.data.frame(predictors),classes)


# predictors distrubution 
ggplot(data = twoClass,aes(x = PredictorA, y = PredictorB)) + 
  geom_point(aes(color = classes), size = 6, alpha = .5) +
  scale_colour_manual(name = 'classes', values = c("red","blue")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))

# prepare graphics for predictive data
nbp <- 250;
PredA <- seq(min(twoClass$PredictorA), max(twoClass$PredictorA), length = nbp)
PredB <- seq(min(twoClass$PredictorB), max(twoClass$PredictorB), length = nbp)
Grid <- expand.grid(PredictorA = PredA, PredictorB = PredB)

PlotGrid <- function(pred,title) {
  surf <- (ggplot(data = twoClass, aes(x = PredictorA, y = PredictorB, 
                                       color = classes)) +
             geom_tile(data = cbind(Grid, classes = pred), aes(fill = classes)) +
             scale_fill_manual(name = 'classes', values = c("goldenrod2","grey20")) +
             ggtitle("Decision region") + theme(legend.text = element_text(size = 10)) +
             scale_colour_manual(name = 'classes', values = c("goldenrod2","grey20"))) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  pts <- (ggplot(data = twoClass, aes(x = PredictorA, y = PredictorB,  
                                      color = classes)) +
            geom_contour(data = cbind(Grid, classes = pred), aes(z = as.numeric(classes)), 
                         color = "red", breaks = c(1.5)) +
            geom_point(size = 4, alpha = .5) + 
            ggtitle("Decision boundary") +
            theme(legend.text = element_text(size = 10)) +
            scale_colour_manual(name = 'classes', values = c("goldenrod2","grey20"))) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  grid.arrange(surf, pts, top = textGrob(title, gp = gpar(fontsize = 20)), ncol = 2)
}


# create cross validation
V <- 10
T <- 3
TrControl <- trainControl(method = "repeatedcv",
                          number = V,
                          repeats = T)

seed <- 123

# func to compute an accuracy
#postResample can be used to estimate the root mean squared error (RMSE)
ErrsCaret <- function(Model, Name) {
  Errs <- data.frame(t(postResample(predict(Model, newdata = twoClass), twoClass[["classes"]])),
                     Resample = "None", model = Name)
  rbind(Errs, data.frame(Model$resample, model = Name))
}

Errs <- data.frame()
CaretLearnAndDisplay <- function (Errs, Name, Formula, Method, ...) {
  set.seed(seed)
  Model <- train(as.formula(Formula), data = twoClass, method = Method, trControl = TrControl, ...)
  Pred <- predict(Model, newdata = Grid)
  PlotGrid(Pred, Name)
  Errs <- rbind(Errs, ErrsCaret(Model, Name))
}

# Accurany analysis
Acc_analysis1 <- function(Errs) {
  cbind(dplyr::summarize(Errs, mAccuracy = mean(Accuracy, na.rm = TRUE), mKappa = mean(Kappa, na.rm = TRUE),
                         sdAccuracy = sd(Accuracy, na.rm = TRUE), sdKappa = sd(Kappa, na.rm = TRUE)))
}

Acc_analysis2 <- function(Errs) {
  ErrCV <- Acc_analysis1(dplyr::filter(Errs, !(Resample == "None")))
  ErrCV <- mutate(ErrCV, AccuracyCV = round(mAccuracy,3), AccuracyCVInf = round((mAccuracy - 2 * sdAccuracy/sqrt(T * V)),3),
                  AccuracyCVPAC = round((mAccuracy - sdAccuracy),3))
  ErrEmp <- round(dplyr::select(dplyr::filter(Errs, (Resample == "None")), Accuracy),3)
  Err <- dplyr::select(cbind(ErrCV, ErrEmp), Accuracy, AccuracyCV, AccuracyCVInf, AccuracyCVPAC)  
  Err
}


# Naive Bayes
Errs  <- CaretLearnAndDisplay(Errs, "Naive Bayes with Gaussian model", "classes ~ .", "nb",
                             tuneGrid = data.frame(usekernel = c(FALSE), fL = c(0), adjust = c(1)))

ggtexttable(Acc_analysis2(Errs[(nrow(Errs)-30):nrow(Errs),]),
            rows = NULL, theme = ttheme("lBlack"))

# Logistic
Errs  <- CaretLearnAndDisplay(Errs, "Logistic", "classes ~ .", "glm")

ggtexttable(Acc_analysis2(Errs[(nrow(Errs)-30):nrow(Errs),]),
            rows = NULL, theme = ttheme("lBlack"))

# C4.5
Errs <- CaretLearnAndDisplay(Errs, "C4.5", "classes ~ .", "C5.0")

ggtexttable(Acc_analysis2(Errs[(nrow(Errs)-30):nrow(Errs),]),
            rows = NULL, theme = ttheme("lBlack"))


# CART
Errs <- CaretLearnAndDisplay(Errs, "CART", "classes ~ .", "rpart", 
                             control = rpart::rpart.control(minsplit = 5, cp = 0), tuneGrid = data.frame(cp = .02))

Tree <- train(classes ~ ., data = twoClass, method = "rpart", control = rpart::rpart.control(minsplit = 5, cp = 0),
              tuneGrid = data.frame(cp = .02), trControl = TrControl)

rpart.plot(Tree$finalModel)

ggtexttable(Acc_analysis2(Errs[(nrow(Errs)-30):nrow(Errs),]),
            rows = NULL, theme = ttheme("lBlack"))


# Random Forest
Errs <- CaretLearnAndDisplay(Errs, "Random Forest", "classes ~ .", "rf",
                             tuneLength = 1, 
                             control = rpart.control(minsplit = 5))

ggtexttable(Acc_analysis2(Errs[(nrow(Errs)-30):nrow(Errs),]),
            rows = NULL, theme = ttheme("lBlack"))


# SVM
Errs <- CaretLearnAndDisplay(Errs, "Support Vector Machine", "classes ~ .", "svmLinear")

ggtexttable(Acc_analysis2(Errs[(nrow(Errs)-10):nrow(Errs),]),
            rows = NULL, theme = ttheme("lBlack"))


# KNN
KNNKS <- c(2, 3, 5, 7)
for (k in KNNKS) {
  Errs <- CaretLearnAndDisplay(Errs, sprintf("k-NN with k=%i", k), 
                                  "classes ~ .","knn", tuneGrid = data.frame(k = c(k)))
  
}

ggtexttable(Acc_analysis2(Errs[(nrow(Errs)-30):nrow(Errs),]),
            rows = NULL, theme = ttheme("lBlack"))


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


# find best model

FindBestErr <- function(Err) {
  for (name in names(Err)[!(names(Err) == "model")]) {
    ind <- which.max(Err[, name])
    writeLines(strwrap(paste("Best method according to", name, ":", Err[ind, "model"])))
  }
}

FindBestErr(Err)

PlotParallels <- function(Errs) {
  pl <- ggplot(data = dplyr::filter(Errs, Resample != "None"), 
               aes(x = model, y = Accuracy, color = Resample)) + 
    geom_line(aes(x = as.numeric(model))) +
    scale_x_discrete(labels = unique(Errs$model)) +
    scale_color_grey(guide = FALSE)
  Err <- dplyr::summarize(group_by(dplyr::filter(Errs, Resample != "None"), model),
                          Accuracy = mean(Accuracy), Resample = "CV")
  pl <- pl + geom_line(data = Err, aes(x = as.numeric(model)), size = 3) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
          plot.margin = grid::unit(c(1, 1, .5, 1.5), "lines"))
  print(pl)
}

PlotParallels(Errs)


https://docplayer.net/64849521-Spervised-classification-an-exploration-with-r-erwan-le-pennec-and-ana-karina-fermin-fall-2015.html


