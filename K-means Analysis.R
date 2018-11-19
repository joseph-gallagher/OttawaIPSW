#Clustering analysis of daily WiFi patterns.


#A function to measure average best-case pairwise distance
#between two equal-size lists of vectors in Euclidean space.

mean_dist <- function(a,b){
  
}


#Stability function for clustering w/k clusters:

clust_dev <- function(data, k, n=10, p=10){
  
  #Repeat the clustering algorithm n times and store clusters
  experiments <- list()
  for(i in 1:n){
    cc <- kmeans(data, centers=k)
    experiments[[i]] <- cc$centers
  }
  
  #Choose p many output pairs and measure average pair distance
  #of clusters
  clust_dist <- c()
  for(i in 1:p){
    s <- base::sample(1:n, 2)
    p1 <- experiments[[s[1]]]
    p2 <- experiments[[s[2]]]
    
    clust_dist <- append(clust_dist, mean_dist(p1, p2))
    
  }
  
  #Return average of the distance
  mean(clust_dist)
  
}

#Plot the deviance as a function of k for a fixed
#data set.

plot_dev_in_k <- function(data, m){
  
  dev <- c()
  for(i in 1:m){
    dev <- append(dev, clust_dev(data, i))
  }
  plot(dev)
  
}

#Alternatively, make an elbow plot.
elbow_plot <- function(data, m){
  
  ssq <- c()
  for(i in 2:m){
    cc <- kmeans(data, centers=i)
    ssq <- append(ssq, cc$tot.withinss)
  }
  plot(ssq)
  length(ssq)
}

#If the day carries its cluster, we can compute
avg_to_wrong_clust <- function(day, center){
  
  cor_clust <- day["cluster"]
  d <- c()
  just_day <- day[1:(length(day)-1)]
  for(i in 1:nrow(center)){
    
    if(i != cor_clust){
      d <- append(d, dist(rbind(center[i,],just_day))^2)
    }
    
  }
  mean(d)
}

#How much do we improve accuracy by binning?
improve <- function(data, clustering, t){
  
  #Find the average error in prediction w/o clustering:
  no_clust <- sqrt(clustering$totss/(nrow(data)*ncol(data)))
  
  #Find the accuracy with clustering, predicting at time t:
  acc <- slice_accuracy(data, clustering, t)
  error_bin_correct <- sqrt(clustering$tot.withinss/(nrow(data)*ncol(data)))
  
  dt <- cbind(data, "cluster" = clustering$cluster)
  error_bin_wrong <- sqrt(sum(apply(dt, 1, 
                                    avg_to_wrong_clust,
                                    clustering$centers))/(nrow(data)*ncol(data)))
  
  w_clust <- acc*error_bin_correct+(1-acc)*error_bin_wrong
  
  result <- c(w_clust,no_clust)
  result
    
}

#Plotting cluster centers as "typical days", assuming c_f1 is a clustering of
#floor 1. 

library(hms)
library(tidyr)

df <- data.frame("Time"=names(f1_pr), 
                 "Typical Weekday" = c_f1$centers[1,], 
                 "Atypical Weekday" = c_f1$centers[2,], 
                 "Weekend"=c_f1$centers[3,])
df$Time <- parse_hms(df$Time)

df <- df %>%
  gather(key = "Day_Type", value = "Wifi_Devices", -Time)

ggplot(df, aes(x = Time, y = Wifi_Devices)) + 
  geom_line(aes(color = Day_Type), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()



