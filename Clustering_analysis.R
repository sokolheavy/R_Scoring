library(ggplot2)
library(dplyr)
library(Hmisc)

# 1
# func that involve data/frame and qty of clusters and return data.frame with column of this clusters

test_data <- read.csv("https://stepic.org/media/attachments/course/524/test_data_hclust.csv")
str(test_data)

cluster_number <- 3

smart_hclust<-  function(test_data, cluster_number){
  dist_matrix <- dist(test_data, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
  fit <- hclust(dist_matrix) 
  cluster <- cutree(fit, cluster_number)
  data.frame(test_data, cluster=as.factor(cluster))
  
}  

smart_hclust <- function(test_data, n_cluster){    
  d <- dist(test_data)    
  fit <- hclust(d)    
  test_data$cluster <- factor(cutree(fit, k = n_cluster))    
  return(test_data)    
}

smart_hclust(test_data, 3)

df[df$cluster == 1, ]

df %>% 
  group_by(cluster)  %>%
  summarise(median(X1), mean(X2))

# 2
# func involve data.frame and qty of clusters, return columns that take more ifluence to buil this cluster

test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")

ggplot(test_data, aes(V1, V2)) +
  geom_point()

# we can see, that more information to build clusters take 'V2'


# my dull func, but it works and I like it
get_difference <- function(test_data, n_cluster){
  #use func from previous task
  test_cluster_col <- data.frame(smart_hclust(test_data, n_cluster))
  
  zz <- apply(test_cluster_col[, colnames(test_cluster_col)!="cluster"], 2,
              function(x) sapply(data.frame(summary(aov(x ~ cluster, test_cluster_col))[[1]])[1,5],
                                 function(y) y<0.05))
  
  colnames(test_cluster_col[, colnames(test_cluster_col)!="cluster"])[zz]
  
}

# more pretty, more intelligence...not my
get_difference <- function(test_data, n_cluster){    
  dist_matrix <- dist(test_data)    
  fit <- hclust(dist_matrix)    
  test_data$cluster <- as.factor(cutree(fit, n_cluster))    
  p_val <- sapply(test_data[,-ncol(test_data)],    
                  function(x) {summary(aov(x~cluster, test_data))[[1]][1,'Pr(>F)']})    
  return(names(p_val)[p_val < 0.05])    
}

get_difference(test_data, 2)

# PCA, add 2 column(p1, pc2) to data frame

test_data <- read.csv("https://stepic.org/media/attachments/course/524/pca_test.csv")

get_pc <- function(d){data.frame(d,prcomp(d)$x[,c(1,2)])
}

## cheking initial data
summary(test_data)
# Mean,  Min.,Max. aren`t different from eah other




