# Set the working directory
setwd("/Users/amelchristy/Amazon_R")
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the data
amazon <- read.csv("updated.csv")
table(amazon$City)
colnames(amazon)
amazon$Time_Order_picked
amazon$Time_Orderd
# EDA
str(amazon)
hist(amazon$Time_taken.min.)
hist(amazon$Vehicle_condition)

# Calculate the mean and median
mean_time <- mean(amazon$Time_taken.min., na.rm = TRUE)
median_time <- median(amazon$Time_taken.min., na.rm = TRUE)


# Preprocess data
amazon <- amazon %>%
  filter(Weatherconditions != "7", Road_traffic_density != "4", Vehicle_condition != 3) # Ensure these are the correct factor levels

###FEATURE ENGINEERING
#DISTANCE
# Define function to calculate haversine distance
haversine <- function(lat1, lon1, lat2, lon2) {
  # Convert degrees to radians
  rad <- pi / 180
  lat1 <- lat1 * rad
  lon1 <- lon1 * rad
  lat2 <- lat2 * rad
  lon2 <- lon2 * rad
  
  # Haversine formula
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  
  # Earth's radius in kilometers
  R <- 6371
  d <- R * c
  
  return(d)  # returns distance in kilometers
}

# Adding distance calculation to the dataset
amazon <- amazon %>%
  mutate(distance = haversine(Restaurant_latitude, Restaurant_longitude,
                              Delivery_location_latitude, Delivery_location_longitude))

colnames(amazon)

amazon
# Classify peak times
# Load necessary libraries
library(dplyr)

###Calculate Peak time
# Convert Time_Order_picked to just the hour component, handling unusual and empty values
amazon$Time_Order_picked <- sapply(amazon$Time_Order_picked, function(x) {
  # Split the string on ":" and take the first part (hour)
  hour_part <- ifelse(nchar(x) > 0, as.numeric(strsplit(x, ":")[[1]][1]), NA)
  # Replace potential invalid hours like '24' or '25', etc., with NA
  ifelse(hour_part >= 0 & hour_part <= 23, hour_part, NA)
})

# Calculate order counts per hour
order_counts_per_hour <- amazon |>
  filter(!is.na(Time_Order_picked)) |>
  group_by(Time_Order_picked) |>
  summarise(Order_Count = n(), .groups = 'drop') |>
  arrange(desc(Order_Count))

# Determine the threshold for peak times using the 60th percentile
threshold <- quantile(order_counts_per_hour$Order_Count, 0.6, na.rm = TRUE)

# Identify hours that are considered peak times
peak_hours <- order_counts_per_hour %>%
  filter(Order_Count >= threshold) %>%
  pull(Time_Order_picked)

# Classify times in the original dataset as 'Peak' or 'Off-Peak'
amazon$Peak_time_Flag <- ifelse(amazon$Time_Order_picked %in% peak_hours, "Peak", "Off-Peak")

# Optionally, verify the distribution of peak time flags
table(amazon$Peak_time_Flag)
nrow(amazon)


colnames(amazon)

library(dplyr)
#Remove unnecessary columns
amazon <- amazon %>%
  select(-c(X, ID, Delivery_person_ID, Restaurant_latitude, Restaurant_longitude, 
            Delivery_location_latitude, Delivery_location_longitude, 
            Time_Order_picked, Time_Orderd))

colnames(amazon)

####AS FACTOR
library(dplyr)

amazon <- amazon |>
  mutate(across(c(Weatherconditions,
                  Road_traffic_density, Vehicle_condition, Type_of_order,
                  Type_of_vehicle, multiple_deliveries, Festival, City,
                    Peak_time_Flag), as.factor))

amazon$Peak_time_Flag = ifelse(amazon$Peak_time_Flag == "Peak", 1, 0)


str(amazon)
amazon <- na.omit(amazon)




###DATA PARTITION
set.seed(123)

train.index <- sample(seq_len(nrow(amazon)), 0.6 * nrow(amazon))
train.df <- amazon[train.index, ]
valid.df <- amazon[-train.index, ]



####RF 
colnames(amazon)
library(randomForest)
set.seed(123)
# Train the random forest model
rf <- randomForest(Time_taken.min. ~ ., data = train.df, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)
# Make predictions on the validation dataset
rf.pred <- predict(rf, valid.df)

# Calculate RMSE and R-squared using caret
rmse <- RMSE(rf.pred, valid.df$Time_taken.min.)
r_squared <- R2(pred = rf.pred, obs = valid.df$Time_taken.min.)

# Print the performance metrics
print(paste("RMSE:", rmse))
print(paste("R-squared:", r_squared))

# Plot variable importance
varImpPlot(rf, type = 1)

# Get variable importance
importance_values <- importance(rf, type = 1)
print(importance_values) 


