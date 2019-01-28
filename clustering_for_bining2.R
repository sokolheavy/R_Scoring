glimpse(sample_appl)
total_sample<-join(sample_appl,sample_base,  by="id_order")
train <- select(total_sample, target_60max12m, 
                             Age_y, 
                             Total_work_experience_in_months, 
                             Add_Income_from_client)

train <- add_column(train, 
                        target_for_calc = ifelse(train$target_60max12m == "good", 1,0), 
                        .after = "target_60max12m")


str(train)
summary(train)


work_data <- train
work_data <- as.data.frame(work_data)
bin_col <- 3:5

i <- 3
i <- 4
i <- 5



##########################km

for (i in bin_col) 
{ 
   interval_count<-6
  repeat{
    km<-classIntervals(work_data[,i], interval_count, style = 'kmeans')
    colname <- paste(names(work_data)[i], "cat_km",interval_count,  sep="_")
    work_data[[colname]] <- with(work_data, cut(work_data[,i], km$brks,include.lowest = TRUE, 
                                                right = FALSE, ordered = TRUE,dig.lab = 10))
    levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
    interval_count<-interval_count-1
    
    # any class have to be bigger than 3% of all work_data
    #if (min(table(work_data[[colname]])/(nrow(work_data)/100)>3)) {break}
    
    # interval_count - {6,5,4,3,2}, if interval_count<2 - end
    if (interval_count<2) {break}
  }
}

##########################smbinning
target_calc <- 2
for (i in bin_col) 
{ 
sb_data<-work_data[c(target_calc,i)]
sb<-try(smbinning(sb_data,y=names(work_data)[target_calc],x=names(work_data)[i]), FALSE)
if (length(sb) > 1) ##check this condition !!!!
 {colname <- paste(names(work_data)[i], "cat_sb", sep="_")
work_data[[colname]] <- with(work_data, cut(work_data[,i], c(min(work_data[,i])-1,unique(sb$bands)),
                                            right = TRUE, left = FALSE, ordered = TRUE,dig.lab = 10))
levels(work_data[[colname]])<-gsub(",", ";",levels(work_data[[colname]]))
 }
}


#if original columns and categorical are arranged in other order or not all original columns have categorical 
#you can use this one
#remove original columns
bin_data<-work_data
n1<- 3
n2<- length(names(bin_data))

for (i in c(n2:n1))
{ 
  print(paste(i, names(bin_data[i])), quote = FALSE)
  if (length(grep(paste(names(bin_data[i]), "_cat", sep = ""), names(bin_data))) > 0)
  {bin_data[i] <- NULL
  print("delete", quote = FALSE)
  }
  else
  {print("skip", quote = FALSE)}
}

rm(n1)
rm(n2)

target_ncol_bin <-  ncol(bin_data)
target_calc_bin <- match("target_for_calc",names(bin_data))
variable_fc_bin <- target_calc_bin + 1
bin_ncol <- ncol(bin_data)


## All variables should have 'factor' type, so convert variables, that not is a 'factor', to 'factor' 

# select variables that shouldn`t conver to the 'fator'
factor_vars <- c(names(bin_data)[1:(variable_fc_bin-1)]
                 ,names(which(sapply(bin_data[,variable_fc_bin:ncol(bin_data)], is.factor))))

# convert "unfactor" variables to 'factor'
bin_data[setdiff(names(bin_data),factor_vars)] <- data.frame(
  sapply(
    select(bin_data, -factor_vars), as.factor))

rm(factor_vars)

## IV - statistic table
iv_table <- arrange(cbind.data.frame(variables = names(bin_data[variable_fc_bin:bin_ncol])
                                     ,IV = sapply(bin_data[variable_fc_bin:bin_ncol], function(x) round(IV(X=x, Y=bin_data$target_for_calc)[1],4)), row.names = NULL),
                    desc(IV))

# Add strength of variables
iv_table$Strength <- ifelse(iv_table$IV>=1, "Suspicious",
                            ifelse(iv_table$IV>=.5, "Very strong",
                                   ifelse(iv_table$IV>=.2, "Strong",
                                          ifelse(iv_table$IV>=.1, "Average",
                                                 ifelse(iv_table$IV>=.02, "Weak", "Wery weak")))))



rm(sb_data)
#################################################################################################################
#new type of clustering

#install.packages("fpc")

library(ggplot2)
library(fpc)
library(reshape2)

#function for calc

# PICKING THE NUMBER OF CLUSTERS
#Function to calculate squared distance between two vectors.
sqr_edist <- function(x, y) {
  sum((x-y)^2)
}

# Function to calculate the WSS for a single cluster, which is represented as a matrix(one row for every point)
wss.cluster <- function(clustermat) {
  c0 <- apply(clustermat, 2, FUN=mean)
  sum(apply(clustermat, 1, FUN=function(row){sqr_edist(row,c0)}))
}

# Function to compute the total WSS from a set of data points and cluster labels.
wss.total <- function(dmatrix, labels) {
  wsstot <- 0
  k <- length(unique(labels))
  for(i in 1:k)
    wsstot <- wsstot + wss.cluster(subset(dmatrix, labels==i))
  wsstot
}

# Convenience function to calculate the total sum of squares
totss <- function(dmatrix) {
  grandmean <- apply(dmatrix, 2, FUN=mean)
  sum(apply(dmatrix, 1, FUN=function(row){sqr_edist(row, grandmean)}))
}


# CH(k)=[B(k)/W(k)]×[(n−k)/(k−1)], where n = # data points k = # clusters W(k) = within cluster variation B(k) = between cluster variation.
# A function to calculate the CH index for a number of clusters from 1 to kmax

ch_criterion <- function(dmatrix, kmax, method="kmeans") {
  if(!(method %in% c("kmeans", "hclust"))) {
    stop("method must be one of c('kmeans', 'hclust')")
  }
  
  npts <- dim(dmatrix)[1] # number of rows.
  totss <- totss(dmatrix) # The total sum of squares is independent of the clustering.
  wss <- numeric(kmax) 
  crit <- numeric(kmax)
  wss[1] <- (npts-1)*sum(apply(dmatrix, 2, var)) # Calculate WSS for k=1 (which is really just total sum of squares).
  
  #Calculate WSS for k from 2 to kmax. kmeans() returns the total WSS as one of its outputs.
  for(k in 2:kmax) {  
    if(method=="kmeans") {
      clustering<-kmeans(dmatrix, k, nstart=10, iter.max=100)
      wss[k] <- clustering$tot.withinss
    }else { # hclust
      d <- dist(dmatrix, method="euclidean")
      pfit <- hclust(d, method="ward.D")
      labels <- cutree(pfit, k=k)
      wss[k] <- wss.total(dmatrix, labels)
    }
  }
  bss <- totss - wss
  crit.num <- bss/(0:(kmax-1)) #Normalize BSS by k-1.
  crit.denom <- wss/(npts - 1:kmax) #Normalize WSS by npts - k.
  list(2:kmax, crit = crit.num/crit.denom, wss = wss, totss = totss) 
}

#######test result

vars.to.use <- colnames(protein)[-1]
pmatrix <- scale(protein[,vars.to.use])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")
plot(pfit, labels=protein$Country)

rect.hclust(pfit, k=5)

groups <- cutree(pfit, k=5)
print_clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(protein[labels==i,c("Country","RedMeat","Fish","Fr.Veg")])
  }
}

print_clusters(groups, 5)



vars.to.use <- colnames(train)[3]
pmatrix <- scale(train[,vars.to.use])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")

d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward.D")
plot(pfit, labels=train$target_for_calc)


## "hclust" choose best k
library(reshape2) #for 'melt' func
clustcrit <- ch_criterion(pmatrix, 10, method="hclust")
critframe <- data.frame(k=1:10, ch=scale(clustcrit$crit),
                        wss=scale(clustcrit$wss))

#prepare data for graph
critframe <- melt(critframe, id.vars=c("k"),
                  variable.name="measure",
                  value.name="score")
# choose bst k
ggplot(critframe, aes(x=k, y=score, color=measure)) +
  geom_point(aes(shape=measure)) + geom_line(aes(linetype=measure)) +
  scale_x_continuous(breaks=1:10, labels=1:10)



cluster_number <- 3

smart_hclust<-  function(test_data, n_cluster){
  dist_matrix <- dist(test_data, method = "euclidean", diag = FALSE, upper = FALSE)
  fit <- hclust(dist_matrix) 
  factor(cutree(fit, k = n_cluster)) 
}  

train <- as.data.frame(train)
train <- train[,1:5]
train$cluster_6 <- smart_hclust(train[,3], 6)

iv_table_cluster <- arrange(cbind.data.frame(variables = names(train[,6:12])
                                     ,IV = sapply(train[,6:12], function(x) round(IV(X=x, Y=train$target_for_calc)[1],4)), row.names = NULL),
                    desc(IV))



plot1_hist <- ggplot(train, aes(train[,7])) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "steelblue4") +
  scale_y_continuous(labels=scales::percent)+ 
  geom_text(aes( y = ((..count..)/sum(..count..)),
                 label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
  theme(axis.text.x = element_text(angle=10, vjust=0.9),
        plot.margin = unit(c(1,1,1,1), "cm") ) + 
  labs( y = "Class", x = "")









## "kmeans" choose best k
# default: 100 random starts and 100 min iteration 
clustering.ch <- kmeansruns(pmatrix,krange = 1:10,criterion = "ch")
# choose best k for 'CH'
clustering.ch$bestk  

# Avarage silhoutte width
clustering.asw<-kmeansruns(pmatrix,krange = 1:10,criterion = "asw") 
# choose best k for 'ANS'
clustering.asw$bestk

critframe<-data.frame(k=1:10,ch=scale(clustering.ch$crit),asw=scale(clustering.asw$crit)) 
critframe<- melt(critframe, id.vars=c("k"),
                 variable.name = "measure",
                 value.name = "score")
ggplot(critframe,aes(x=k,y=score,color=measure))+
  geom_point(aes(shape=measure))+
  geom_line(aes(linetype=measure))+
  scale_x_continuous(breaks=1:10,labels = 1:10)



train$cluster_km_2 <- as.factor(kmeans(train[,3], 2)[[1]])
train$cluster_km_3 <- as.factor(kmeans(train[,3], 3)[[1]])
train$cluster_km_4 <- as.factor(kmeans(train[,3], 4)[[1]])
train$cluster_km_5 <- as.factor(kmeans(train[,3], 5)[[1]])
train$cluster_km_6 <- as.factor(kmeans(train[,3], 6)[[1]])



