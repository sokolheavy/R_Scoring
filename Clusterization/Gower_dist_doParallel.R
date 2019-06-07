
set.seed(123)
train<-data_cluster[sort(sample(nrow(data_cluster), nrow(data_cluster)*.7)),] # 70% here
test<-data_cluster[-sort(sample(nrow(data_cluster), nrow(data_cluster)*.7)),] # rest of the 30% data goes here

install.packages('testthat')

install.packages('StatMatch')
library("data.table")
library("StatMatch")
library("testthat")
library("doParallel")

# Function to parallize the computation of distance-matrices
computeDistance <- function(dt1, dt2, nThreads = 4) {
  # Determine chunk-size to be processed by different threads
  s <- floor(nrow(dt1) / nThreads)
  
  # Setup multi-threading
  modelRunner <- makeCluster(nThreads)
  registerDoParallel(modelRunner)
  
  # For numeric variables, build ranges (max-min) to be used in gower-distance.
  # Ensure that the ranges is computed on the overall data and not on
  # the chunks in the parallel threads. Also, note that function 'gower.dist()'
  # seems to be buggy in regards to missing values (NA), which can be fixed by
  # providing ranges for all numeric variables in the function-call
  dt <- rbind(dt1, dt2)
  rngs <- rep(NA, ncol(dt))
  for (i in 1:ncol(dt)) {
    col <- dt[[i]]
    if (is.numeric(col)) {
      rngs[i] <- max(col, na.rm = T) - min(col, na.rm = T)
    }
  }
  
  # Compute distance in parallel threads; note that you have to include packages
  # which must be available in the different threads
  distanceMatrix <-
    foreach(
      i = 1:nThreads, .packages = c("StatMatch"), .combine = "rbind",
      .export = "computeDistance", .inorder = TRUE
    ) %dopar% {
      # Compute chunks
      from <- (i - 1) * s + 1
      to <- i * s
      if (i == nThreads) {
        to <- nrow(dt1)
      }
      
      # Compute distance-matrix for each chunk
      distanceMatrix <- gower.dist(dt1[from:to,], dt2, rngs = rngs)
    }
  
  # Clean-up
  stopCluster(modelRunner)
  return(distanceMatrix)
}

main <- function() {
  
  #Load data (NOTE: data is of class data.table, which behaves slightly different
  # than data.frame)
  couponListTrain <- train
  couponListTest <- test
  
  numberOfTrainCoupons <- nrow(couponListTrain)
  numberOfTestCoupons <- nrow(couponListTest)
  
  # ----- Compute sequentially -----
  print("*** Compute sequentially ***")
  
  # Note that function 'gower.dist()' seems to be buggy in regards to missing 
  # values (NA), which can be fixed by providing ranges for all numeric 
  # variables in the function-call
  dt <- rbind(couponListTrain, couponListTest)
  rngs <- rep(NA, ncol(dt))
  for (i in 1:ncol(dt)) {
    col <- dt[[i]]
    if (is.numeric(col)) {
      rngs[i] <- max(col, na.rm = T) - min(col, na.rm = T)
    }
  }
  
  print(system.time(dist1 <- gower.dist(couponListTrain, couponListTest, rngs)))
  expect_that(nrow(dist1), equals(numberOfTrainCoupons))
  expect_that(ncol(dist1), equals(numberOfTestCoupons))
  
  # ----- Compute in parallel -----
  print("*** Compute in parallel ***")
  print(system.time(dist2 <- computeDistance(couponListTrain, couponListTest)))
  expect_that(nrow(dist2), equals(numberOfTrainCoupons))
  expect_that(ncol(dist2), equals(numberOfTestCoupons))
  
  print("Check that both distance-matrices are the same")
  print(all.equal(dist1, dist2))
}

main()
