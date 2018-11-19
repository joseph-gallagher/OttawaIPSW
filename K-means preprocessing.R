#A script to cut the long .xlsx file into days
#for clustering.


library(readxl)
library(dplyr)

# ---- ONE MAGIC FUNCTION 
second_floor <- read_excel("WiFi/Floor2.xlsx",
                           col_names = c("Time", "S1", "S2", "S3", "S4", "S5"),
                           sheet = 1)

#A function to split a given floor's data into day observations.
split_days <- function(floor){
  
  #Rename the first index to be time:
  names(floor)[1] <- c("Time")
  
  #Sum the sensors and extract just this information:
  floor <- data.frame("Time" = floor$Time,
                      "sum" = rowSums(select(floor, -Time)))
  
  #Find the indexes where the new days start
  new_days <- c(1)
  day_names <- c("01-23")
  
  days <- format(floor$Time, "%d")
  months <- format(floor$Time, "%m")
  for(i in 1:(nrow(floor)-1)){
    
    current_slice_day = days[i]
    next_slice_day = days[i+1]
    
    if(current_slice_day != next_slice_day){
      new_days <- append(new_days, i+1)
      day_names <- append(day_names, 
                          paste0(months[i+1], "-", days[i+1]))
    }
    
  }
  
  #Make a list of vectors by extracting the relevant chunks
  time_slices <- format(floor$Time, "%H:%M:%S")
  day_profiles <- data.frame()
  
  for(i in 1:(length(new_days)-2)){
    start = new_days[i]
    end = new_days[i+1]-1
    day_profiles <- rbind(day_profiles, name = floor$sum[start:end])
  }
  
  #Rename the rows and columns
  names(day_profiles) <- time_slices[1:287]
  row.names(day_profiles) <- day_names[1:119]
  
  #Return cleaned data
  day_profiles
  
}
#----- END MAGIC FUNCTION

#Perform k-means clustering
# clust <- kmeans(day_profiles, centers = 3)

#How well does the timeslice from 6:00 AM to 6:00 AM + 5*t determine
#which day type you are in?

predict_type <- function(day,centers,t){
  
  #Take the slice from 6:00 onwards
  day_slice <- day[72:(72+t)]
  
  #Measure the distance from each slice to the corresponding 
  #slice of the clusters
  d <- c()
  for(i in 1:nrow(centers)){
    d <- append(d, dist(rbind(centers[i,][72:(72+t)], day_slice)))
  }
  which.min(d)
}

slice_accuracy <- function(data, clustering, t){
  
  preds <- apply(data, 1, predict_type, 
        centers = clustering$centers,
        t = t)
  
  acc <- sum(preds == clustering$cluster)/length(preds)
  
  acc
  
}

plot_time_acc <- function(data, clustering){
  
  acc <- c()
  for(t in 1:110){
    acc_now <- slice_accuracy(data, clustering, t)
    acc <- append(acc, acc_now)
  }
  plot(acc, type = "l")
  
}

