# convert to h2o frame 
h2o.init(nthreads=-1, max_mem_size="4G")
h2o.removeAll()    # clean slate - just in case the cluster was already running
h2o.no_progress()  # Don't show progress bars in RMarkdown output

h2o.df <- as.h2o(data_cluster)
h2o_kmeans <- h2o.kmeans(training_frame = h2o.df, x = names(data_cluster)[-1],k = 5, estimate_k = FALSE, seed = 1234)
summary(h2o_kmeans)

# run prediction, exclude actual cluster info
p <- h2o.predict(h2o_kmeans, h2o.df[ ,-1])

unique(as.vector(p)) # h2o

# decode H2O cluster id
h2o_cluster_id <- as.vector(p)
h2o_cluster <- ifelse(h2o_cluster_id==0, 5, h2o_cluster_id)

h2o_kmeans
h2o_kmeans@model$centers       # The centers for each cluster


gap_stat <- h2o.gapStatistic(data = h2o_kmeans, K = 10, B = 100, boot_frac = .1)

final <- data.frame(cluster = h2o_cluster, data_cluster)



prop.table(table(final$cluster))
