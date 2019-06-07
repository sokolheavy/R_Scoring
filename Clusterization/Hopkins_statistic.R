# First of all we should check whether datasets are clusterable at all. 
# Hopkins statistic- measuring the cluster tendency. 
# 1- data is highly clustered, random data will tend to 0.5, and uniformly distributed data will tend to result close to 0.

data_h <- hopkins(data_cluster, (nrow(data_cluster)-1), header = F)


cat("All dataset: Hopkin's statistic is equal to:",data_h$H)
