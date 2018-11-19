#A script for reading and experimenting with building occupancy data.

library(xlsx)
library(dplyr)

fifth_floor <- read.xlsx("DataWithGroundtruth.xlsx",1)
sixth_floor <- read.xlsx("DataWithGroundtruth.xlsx",2)
seventh_floor <- read.xlsx("DataWithGroundtruth.xlsx",3)

#Select out the wifi from the fifth floor and compute sums.
#How well does total WiFi count predict Groundtruth?

fifth_wifi <- fifth_floor %>%
	select(contains("AP1")) %>%
	mutate("total" = rowSums(.)) %>%
	mutate("Groundtruth" = fifth_floor$Groundtruth)

#Repeat with CO2.

fifth_CO2 <- fifth_floor %>%
	select(contains("CO2")) %>%
	mutate("total" = rowSums(.)) %>%
	mutate("Groundtruth" = fifth_floor$Groundtruth) %>%
	na.omit()

#Repeat with total power.

fifth_power <- fifth_floor %>%
	select(contains("AA5")) %>%
	mutate("total" = rowSums(.)) %>%
	mutate("Groundtruth" = fifth_floor$Groundtruth) %>%
	na.omit() 

#Repeat with motion detectors.

fifth_md <- fifth_floor %>%
	select(contains("MD")) %>%
	mutate("total" = rowSums(.)) %>%
	mutate("Groundtruth" = fifth_floor$Groundtruth) %>%
	na.omit()

#Building various models for the fifth floor:
wifi_vars = names(fifth_floor)[grep("CA",names(fifth_floor))]
md_vars = names(fifth_floor)[grep("MD",names(fifth_floor))]
power_vars = names(fifth_floor)[grep("AA5",names(fifth_floor))]
CO2_vars = names(fifth_floor)[grep("CO2",names(fifth_floor))]

fm1 = paste("Groundtruth ~",paste(wifi_vars,collapse="+"))
model1 = lm(fm1, data=fifth_floor)

wifi_power_vars = append(wifi_vars,power_vars)
fm2 = paste("Groundtruth ~",paste(wifi_power_vars,collapse="+"))
model2 = lm(fm2, data=fifth_floor)


#To Do:

# --- GOAL --- : Classify various model quality using
#only linear models.

  #Write a function taking floor + producing all models

simple <- function(floor){
  
  #Get the floor's various sensor readings
  floor_wifi <- dplyr::select(floor, contains("AP"))
  floor_md <- dplyr::select(floor, contains("MD"))
  floor_CO2 <- dplyr::select(floor, contains("CO2"))
  floor_power <- dplyr::select(floor, contains("Power"))
  
  #Compute summary statistics for each sensor and
  #collect into a data frame
  sum_wifi <- rowSums(floor_wifi)
  sum_md <- rowSums(floor_md)
  sum_CO2 <- rowSums(floor_CO2)
  sum_power <- rowSums(floor_power)
  
  floor_summary <- cbind.data.frame("wifi" = sum_wifi,
                                    "power" = sum_power,
                                    "CO2" = sum_CO2,
                                    "md" = sum_md,
                                    "Groundtruth" = floor$Groundtruth)
  floor_summary <- na.omit(floor_summary)
  floor_summary
  
}



  #Find a way to compare models with various "types"
  #For linear, ANOVA. Does this persist with glm, nn, rf?

# --- GOAL --- : Find best models using all data/generally
#available data, not nec. linear.

  #Add time lagged data for poor predictors.

  #Try random-forest/gradient boosting

# --- GOAL --- : Study robustness of resulting algorithms
#across buildings, using floors as proxy.

  #???








	
	

	



