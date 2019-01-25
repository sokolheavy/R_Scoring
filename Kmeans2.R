names(data)

# Manually scaling - (x - mean(x)) / sd(x)
df.stand <- as.data.frame(scale(data[,2]))
set.seed(5)
c(kmeans(df.stand, centers = 5, nstart = 1)$tot.withinss,
  kmeans(df.stand, centers = 5, nstart = 25)$tot.withinss)

k.max <- 15
wss <- sapply(1:k.max, function(k){
  kmeans(df.stand, k, nstart = 10)$tot.withinss
})

names(data)[2]

# bild plot and choose number  of clusters:
plot(1:k.max, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Число кластеров K", 
     ylab = "Общая внутригрупповая сумма квадратов")

# the same with fviz_nbclust(), but better:
library(factoextra)
library(ggplot2)
fviz_nbclust(df.stand, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

set.seed(123)
gap_stat <- clusGap(df.stand, FUN = kmeans, nstart = 10, K.max = 6, B = 50)
# Печать и визуализация результатов
print(gap_stat, method = "firstmax")

k.pam <- pam(df.stand, k = 4)





