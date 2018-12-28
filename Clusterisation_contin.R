test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")


h <- test_data
hept_pca <- prcomp(h, scale = TRUE)
prcomp(h, center = TRUE, scale = TRUE)
prcomp(h, scale = TRUE)
str(prcomp(h))

data.frame(test_data,prcomp(h)$x[,c(1,2)])

plot(hept_pca)
exp_percent <- summary(hept_pca)$importance[2, ]
qplot(y = exp_percent, x = names(exp_percent)) + 
  geom_bar(stat = 'identity') +
  labs(x = "Главные компоненты",
       y = "Доли диспесии",
       title = "Доли дисперсии, объясняемые главными компонентами \n (без предварительной стандартизации)")

# 4
# func, that add column with pca, that takes more than 90% information

test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")

# first variant
get_pca2 <- function(test_data){
fit <- prcomp(test_data)
pca_analysis <- summary(fit)$importance[3,]
n_col <- length(pca_analysis[sapply(pca_analysis, function(x) x<0.9)]) + 1
data.frame(test_data, fit$x[,1:n_col])
}

# second variant
get_pca2 <- function(test_data){    
  fit <- prcomp(test_data)    
  cum_prop <- summary(fit)$importance['Cumulative Proportion',]    
  test_data <- cbind(test_data, fit$x[,1:min(which(cum_prop>0.9))])    
  return(test_data)    
}

# 5
# find multicol columns
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_3.csv")
test_data <- read.csv("https://stepic.org/media/attachments/course/524/Norris_1.csv")
test_data <- as.data.frame(list(V1 = c(25, 17, 17, 15, 9), V2 = c(10, 4, 1, 16, 4), V3 = c(14, -11, 6, 1, 11), V4 = c(6, 13, 14, 9, 0), V5 = c(2, 2, 10, -3, 18), V6 = c(20, -5, 12, 7, 17), V7 = c(2, 2, -6, 7, -14)))
is_multicol <- function(test_data){
corr <- cor(test_data)
ut <- upper.tri(corr)
corr_table <- data.frame(
          row = rownames(corr)[row(corr)[ut]],
          column = rownames(corr)[col(corr)[ut]],
          cor = (corr)[ut])

v_corr <- sort(as.vector(unlist(c(subset(corr_table,cor==1|cor==-1)[,-3]))))

if (length(x)==0){
  print ('There is no collinearity in the data')
} else {
  print (v_corr)
}

}

sapply(corr, function(x) x==1)


is_multicol(test_data)

