setwd("C:/Users/Chanel/OneDrive/文件/Lehigh/BUAN452- modelling/amazon")
setwd("/Users/amelchristy/Amazon_R")
library(dplyr)
# Load the dataset
delivery_data <- read.csv("cleaned_test.csv")
table(delivery_data$City)
str(delivery_data)
# Drop unnecessary columns
delivery_data_cleaned <- delivery_data |>
  select(-c(Restaurant_latitude, Restaurant_longitude,
            Delivery_location_latitude, Delivery_location_longitude, Name.))

#Remove NaNs
delivery_data_cleaned$Weather <- ifelse(delivery_data_cleaned$Weather == "NaN", NA, delivery_data_cleaned$Weather)
delivery_data_cleaned$Road_traffic_density <- ifelse(delivery_data_cleaned$Road_traffic_density == "NaN", NA, delivery_data_cleaned$Road_traffic_density)
delivery_data_cleaned$City <- ifelse(delivery_data_cleaned$City == "NaN", NA, delivery_data_cleaned$City)
delivery_data_cleaned$Festival <- ifelse(delivery_data_cleaned$Festival == "NaN", NA, delivery_data_cleaned$Festival)
delivery_data_cleaned <- na.omit(delivery_data_cleaned)

table(delivery_data$multiple_deliveries)

#nan
na_counts <- sapply(delivery_data, function(x) sum(is.na(x)))

# To display the number of NAs in each column
na_counts

na_counts[na_counts > 0]
#no of rows
nrow(delivery_data)
hist(delivery_data$Delivery_person_Age)
# Handling missing values for median age 
median_age <- median(delivery_data_cleaned$Delivery_person_Age, na.rm = TRUE)
median_age

delivery_data_cleaned$Delivery_person_Age <- ifelse(is.na(delivery_data_cleaned$Delivery_person_Age),
                                                    median_age, delivery_data_cleaned$Delivery_person_Age)
delivery_data_cleaned <- delivery_data_cleaned |> filter(!is.na(Delivery_person_Ratings))
##FEATURE ENGINEERING
# Convert time columns to POSIXct format
delivery_data_cleaned$Time_Orderd <- as.POSIXct(delivery_data_cleaned$Time_Orderd, format="%H:%M")
delivery_data_cleaned$Time_Order_picked <- as.POSIXct(delivery_data_cleaned$Time_Order_picked, format="%H:%M")

# Calculate pickup time taken in minutes
delivery_data_cleaned$Pickup_Time_Taken <- as.numeric(difftime(delivery_data_cleaned$Time_Order_picked,
                                                               delivery_data_cleaned$Time_Orderd, units="mins"))

# Handle NA values in Pickup_Time_Taken
mean_pickup_time <- mean(delivery_data_cleaned$Pickup_Time_Taken, na.rm = TRUE)
delivery_data_cleaned$Pickup_Time_Taken[is.na(delivery_data_cleaned$Pickup_Time_Taken)] <- mean_pickup_time

# Filter and select relevant columns for the model
df <- delivery_data_cleaned |> 
  select(-c(ID, Delivery_person_ID, Order_Date, Time_Orderd, Time_Order_picked))

#No na in df
df <- na.omit(df)
#
colnames(df)

# Convert the categorical variables to factors
df <- df |>
  mutate(across(c(Weather, Road_traffic_density, Vehicle_condition, Type_of_order,
                  Type_of_vehicle, multiple_deliveries, Festival, City, Pickup_Time_Taken), factor))
df$Pickup_Time_Taken


colnames()
###VIZZZZZ
#------------visualization 
colnames(df)
colnames(clean)
colnames(delivery_data_cleaned)

# Calculate the mean ratings for each Weather category
mean_ratings <- df |>
  group_by(Weather) |>
  summarise(Mean_Rating = mean(Delivery_person_Ratings, na.rm = TRUE))

# Create the bar chart using Amazon's color palette
ggplot(mean_ratings, aes(x = Weather, y = Mean_Rating, fill = Weather)) +
  geom_bar(stat = "identity", color = "#000000", show.legend = FALSE) +  # Use black for border color
  scale_fill_manual(values = c("#FF9900", "#000000","#146eb4","#232d3e","#f2f2f2","#05A0D1")) +  # Custom colors
  labs(title = "Average Delivery Person Ratings by Weather",
       x = "Weather",
       y = "Average Rating") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Center the title
        axis.title.x = element_text(vjust = -0.5),
        axis.title.y = element_text(vjust = 0.5))

# Weather and Ratings
p1_weather <- ggplot(df, aes(x=as.factor(Weather), Delivery_person_Ratings))
p2_weather <- geom_bar(stat = "summary", fun = "mean", fill = "##FF9900")
plot_weather <- p1_weather + p2_weather + labs(x="Weather", y="Mean Delivery Person Ratings", title = "Weather") + theme_minimal()

library(ggplot2)

# Weather has already been done in your example

# Delivery_person_Age
p1_age <- ggplot(df, aes(x=as.factor(Delivery_person_Age), y=Delivery_person_Ratings))
p2_age <- geom_bar(stat = "summary", fun = "mean", fill = "#FF9900")
plot_age <- p1_age + p2_age + labs(x="Delivery Person Age", y="Ratings", title = "Delivery Person's Age on Ratings") + theme_minimal()

# Road_traffic_density
p1_traffic <- ggplot(df, aes(x=as.factor(Road_traffic_density), y=Delivery_person_Ratings))
p2_traffic <- geom_bar(stat = "summary", fun = "mean", fill = "#000000")
plot_traffic <- p1_traffic + p2_traffic + labs(x="Road Traffic Density", y="Ratings", title = "Traffic Density on Ratings") + theme_minimal()

# Vehicle_condition
p1_vehicle <- ggplot(df, aes(x=as.factor(Vehicle_condition), y=Delivery_person_Ratings))
p2_vehicle <- geom_bar(stat = "summary", fun = "mean", fill = "#146eb4")
plot_vehicle <- p1_vehicle + p2_vehicle + labs(x="Vehicle Condition", y="Ratings", title = "Vehicle Condition on Ratings") + theme_minimal()

# Type_of_order
p1_order <- ggplot(df, aes(x=as.factor(Type_of_order), y=Delivery_person_Ratings))
p2_order <- geom_bar(stat = "summary", fun = "mean", fill = "#232d3e")
plot_order <- p1_order + p2_order + labs(x="Type of Order", y="Ratings", title = "Order Type on Ratings") + theme_minimal()

# Type_of_vehicle
p1_vehicle_type <- ggplot(df, aes(x=as.factor(Type_of_vehicle), y=Delivery_person_Ratings))
p2_vehicle_type <- geom_bar(stat = "summary", fun = "mean", fill = "#7c7c7c")
plot_vehicle_type <- p1_vehicle_type + p2_vehicle_type + labs(x="Type of Vehicle", y="Ratings", title = "Vehicle Type on Ratings") + theme_minimal()

# multiple_deliveries
p1_multiples <- ggplot(df, aes(x=as.factor(multiple_deliveries), y=Delivery_person_Ratings))
p2_multiples <- geom_bar(stat = "summary", fun = "mean", fill = "#05A0D1")
plot_multiples <- p1_multiples + p2_multiples + labs(x="Multiple Deliveries", y="Ratings", title = "Multiple Deliveries on Ratings") + theme_minimal()

# Festival
p1_festival <- ggplot(df, aes(x=as.factor(Festival), y=Delivery_person_Ratings))
p2_festival <- geom_bar(stat = "summary", fun = "mean", fill = "#77bc1f")
plot_festival <- p1_festival + p2_festival + labs(x="Festival", y="Ratings", title = "Festivals on Ratings") + theme_minimal()

# City
p1_city <- ggplot(df, aes(x=as.factor(City), y=Delivery_person_Ratings))
p2_city <- geom_bar(stat = "summary", fun = "mean", fill = "#848484")
plot_city <- p1_city + p2_city + labs(x="City", y="Ratings", title = "City on Ratings") + theme_minimal()

# Pickup_Time_Taken
df_filtered <- df %>% filter(Pickup_Time_Taken %in% c(5, 10, 15))

plot_pickup_time_bar <- ggplot(df_filtered, aes(x=as.factor(Pickup_Time_Taken), y=Delivery_person_Ratings)) +
  geom_bar(stat="summary", fun="mean", fill="#FF9900") +
  labs(x="Pickup Time Taken (minutes)", y="Ratings", title="Impact of Pickup Time on Ratings") +
  theme_minimal()

plot_pickup_time_bar 
combo <- (plot_pickup_time_bar | plot_vehicle_type) /
  (plot_traffic | plot_order) 

combo2 <-(plot_vehicle | plot_multiples) /
  (plot_festival | plot_city) 

# Print the combined plot grid
print(combo)
combo3 <- (plot_pickup_time_bar | plot_vehicle_type) /
  (plot_traffic | plot_order) /
  (plot_vehicle | plot_multiples) /
  (plot_festival | plot_city) /
  (plot_age)
combo3

#delivery person age vs Time taken in min
updated <- read.csv("updated.csv")

#drop rating 6 in the delivery rating column
updated <- updated[updated$Delivery_person_Ratings!=6, ]
colnames(updated)
table(updated$Delivery_person_Age)

###VIZ with the updated.csv
#Delivery Person Age 
ggplot(updated, aes(x=Delivery_person_Age, y=Time_taken.min.))+
  stat_summary(fun.y = mean, geom = "line", color = "#232d3e") +
  labs(title="Delivery Person Age vs. Delivery Time",
       x="Delivery Person Age",
       y= "Time taken(min)")+
  theme_minimal()+
  coord_cartesian(xlim = c(20, 40))


##Delivery Rating and pick up time taken - category
ggplot(updated, aes(x=Delivery_person_Ratings, y=Time_taken.min.))+
  stat_summary(fun.y = mean, geom = "line", color = "#FF9900") +
  labs(title="Delivery Rating vs. Delivery Time",
       x="Delivery Rating",
       y= "Pickup time taken)")+
  theme_minimal()

df$Pickup_Time_Taken
str(df)
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(GGally)
library(patchwork)
df <- na.omit(df)
# Split the data into training and validation sets
set.seed(123) # for reproducibility
train.index <- sample(seq_len(nrow(df)), 0.6 * nrow(df))
train.df <- df[train.index, ]
valid.df <- df[-train.index, ]

# Fit the linear model on the training set
delivery.lm <- lm(Delivery_person_Ratings ~ ., data = train.df)
summary(delivery.lm)


###FORWARD SELECTION
delivery.lm.0 <- lm(Delivery_person_Ratings~1, data=train.df)
summary(delivery.lm.0)

forward.lm <- step(delivery.lm.0, scope=list(lower=delivery.lm.0,upper=delivery.lm),direction="forward")

##BACKWARD SELECTION
backward.lm <- step(delivery.lm, direction ="backward")

###STEP SELECTION
step.lm <- step(delivery.lm, direction="both")

#prediction accuracy on validation dataset
library(forecast)
accu.full <- accuracy(predict(delivery.lm, valid.df),valid.df$Delivery_person_Ratings)
accu.forward <- accuracy(predict(forward.lm,valid.df), valid.df$Delivery_person_Ratings)
accu.backward <- accuracy(predict(backward.lm, valid.df),valid.df$Delivery_person_Ratings)
accu.both <- accuracy(predict(step.lm, valid.df),valid.df$Delivery_person_Ratings)
accu.df <- rbind(accu.full, accu.forward, accu.backward, accu.both)
rownames(accu.df) <- c("Full","Forward","Backward","Both/Step")
accu.df


#############Question 2#############

library(dplyr)
# Load the data
amazon <- read.csv("updated.csv")
colnames(amazon)
amazon$Time_Orderd
#EDA
str(amazon)
hist(amazon$Time_taken.min.)

library(ggplot2)
# Calculate the mean and median
mean_time <- mean(amazon$Time_taken.min., na.rm = TRUE)
median_time <- median(amazon$Time_taken.min., na.rm = TRUE)

# Print the values
print(paste("Mean: ", mean_time))
print(paste("Median: ", median_time))

# Plot histogram with mean and median lines clearly marked
library(ggplot2)
ggplot(amazon, aes(x = Time_taken.min.)) +
  geom_histogram(bins = 30, fill = "gray", color = "black") +
  geom_vline(xintercept = mean_time, color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_time, color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Time Taken with Mean and Median",
       x = "Time Taken (min)", y = "Frequency") +
  theme_minimal() +
  geom_text(aes(x = mean_time, label = "Mean", y = 150), vjust = -0.5, color = "red") +
  geom_text(aes(x = median_time, label = "Median", y = 150), vjust = -0.5, color = "blue")


library(ggplot2)

# Create a boxplot of Time_taken.min. against Vehicle_condition, to see which one to drop if there's NA 
ggplot(amazon, aes(x = as.factor(Vehicle_condition), y = Time_taken.min.)) +
  geom_bar(stat = "identity") +
  labs(title = "Bar Chart of Time Taken by Vehicle Condition",
       x = "Vehicle Condition",
       y = "Time Taken (min)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ensures category labels are readable


# Drop based NAs based on mapping
amazon <- amazon |>
  filter(Weatherconditions != "7", Road_traffic_density != "4", Vehicle_condition !=3) 

hist(amazon$Time_taken.min.)
# Load necessary library
library(dplyr)
# Check the number of NA values in the Time_taken.min. column
na_count <- sum(is.na(amazon$Time_taken.min.))
print(paste("Number of NA values:", na_count))

#####FEATURE ENGINEERING
######EFFICIENCY CLASS
# Calculate mean and standard deviation for Time_taken.min.
mean_time_taken <- mean(amazon$Time_taken.min., na.rm = TRUE)
std_dev_time_taken <- sd(amazon$Time_taken.min., na.rm = TRUE)

#CHANGE TO MEAN 
# Calculate Z-scores for Time_taken.min.
amazon$Z_score <- (amazon$Time_taken.min. - mean_time_taken) / std_dev_time_taken

# Classify deliveries as efficient if their Z-score is less than 0
amazon$Efficiency_Class <- ifelse(amazon$Z_score < 0, "Efficient", "Not Efficient")

# Print out some information for verification
print(paste("Mean Time Taken:", mean_time_taken))
print(paste("Standard Deviation of Time Taken:", std_dev_time_taken))
print(head(amazon[, c("Time_taken.min.", "Z_score", "Efficiency_Class")]))

# Optional: Visualize the results
library(ggplot2)
ggplot(amazon, aes(x = Time_taken.min., fill = Efficiency_Class)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  geom_vline(xintercept = mean_time_taken, color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Time Taken with Efficiency Classification",
       x = "Time Taken (min)", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("Efficient" = "green", "Not Efficient" = "red"))


####GET DISTANCE FROM LATTITUDE
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
  
  # Earth's radius in kilometers (you can change it to 6371 for kilometers)
  R <- 6371
  d <- R * c
  
  return(d)  # returns distance in kilometers
}


# Adding distance calculation to the dataset
amazon <- amazon |>
  mutate(distance = haversine(Restaurant_latitude, Restaurant_longitude,
                              Delivery_location_latitude, Delivery_location_longitude))
str(amazon)
# Check results
head(amazon$distance)
amazon



na.omit(amazon)

#######DO PEAK TIME
library(dplyr)

# Assuming 'amazon' is already loaded
# Clean the Time_Order_picked column by fixing edge cases like '24:00' to '00:00' and 'xx:60' to 'xx:00'
amazon$Time_Order_picked <- gsub("^24:(\\d{2})", "00:\\1", amazon$Time_Order_picked)
amazon$Time_Order_picked <- gsub("^(\\d{2}):60", "\\1:00", amazon$Time_Order_picked)

# Directly extract the hour part using substring manipulation
amazon$Hour_Order_Picked <- substr(amazon$Time_Order_picked, 1, 2)

# Count orders per hour
order_counts_per_hour <- amazon %>%
  group_by(Hour_Order_Picked) %>%
  summarise(Order_Count = n(), .groups = 'drop') %>%
  arrange(desc(Order_Count))

# Determine the threshold for peak times using the 60th percentile
threshold <- quantile(order_counts_per_hour$Order_Count, 0.60, na.rm = TRUE)

# Identify hours that are considered peak times
peak_hours <- order_counts_per_hour %>%
  filter(Order_Count >= threshold) %>%
  pull(Hour_Order_Picked)

# Classify times in the original dataset as 'Peak' or 'Off-Peak'
amazon$Peak_time_Flag <- ifelse(amazon$Hour_Order_Picked %in% peak_hours, "Peak", "Off-Peak")

# Optional: Print results and visualize
print(order_counts_per_hour)
print(table(amazon$Peak_time_Flag))

# Visualization
library(ggplot2)
ggplot(amazon, aes(x = Hour_Order_Picked, fill = Peak_time_Flag)) +
  geom_histogram(stat = "count", binwidth = 1) +
  labs(title = "Order Counts by Hour with Peak Time Highlighted",
       x = "Hour of the Day", y = "Number of Orders") +
  scale_fill_manual(values = c("Peak" = "red", "Off-Peak" = "blue")) +
  theme_minimal()



colnames(amazon)
###REMOVE COLUMNS

amazon$Time_Orderd

# Assuming 'amazon' is your DataFrame
amazon <- amazon |>
  select(
    -X, 
    -Restaurant_longitude, 
    -Time_Orderd, 
    -ID, 
    -Delivery_person_ID, 
    -Delivery_person_Ratings,
    -Restaurant_latitude, 
    -Delivery_location_latitude, 
    -Delivery_location_longitude,
    -Time_Order_picked, 
    -Time_taken.min., 
    -Hour_Order_Picked
  )
amazon

str(amazon)

#convert
amazon <- amazon |>
  mutate(across(c(Weatherconditions,
                  Road_traffic_density, Vehicle_condition, Type_of_order,
                  Type_of_vehicle, multiple_deliveries, Festival, City,
                  Peak_time_Flag,Efficiency_Class), as.factor))
amazon <- amazon |>
  select(
    -Z_score)

##double check the types before splitting
str(amazon)
colnames(amazon)

amazon
#peak flag to 1 and 0 
amazon$Peak_time_Flag <- ifelse(amazon$Peak_time_Flag == "Peak", 1, 0)
amazon$Efficiency_Class <- ifelse(amazon$Efficiency_Class=="Efficient",1,0)
str(amazon)
amazon <- na.omit(amazon)
####DATA PARTITION#####
set.seed(123) 
train.index <- sample(seq_len(nrow(amazon)), 0.6 * nrow(amazon))
train.df <- amazon[train.index, ]
valid.df <- amazon[-train.index, ]

####MODEL#####
# Fit the full model using training data
full_model <- glm(Efficiency_Class ~ ., data = train.df, family = binomial())
summary(full_model)
full_model



# backward selection
backward_glm <- step(full_model, direction = "backward")
summary(backward_glm)

#stepwise selection (both forward and backward)
step_glm <- step(full_model, direction = "both")
summary(step_glm)

###Forward
# Initial model with only the intercept
glm_initial <- glm(Efficiency_Class ~ 1, data = train.df, family = binomial())

# Perform forward selection
forward_glm <- step(glm_initial, scope=list(lower = glm_initial, upper = full_model), direction = "forward")
summary(forward_glm)


library(caret)
library(lattice)
# Load necessary library
library(caret)

# Predict and evaluate for each model
# 1. Full Model
predicted_prob_full <- predict(full_model, newdata = valid.df, type = "response")
predicted_classes_full <- ifelse(predicted_prob_full > 0.5, "1", "0")
predicted_classes_full <- factor(predicted_classes_full, levels = c("0", "1"))
conf_matrix_full <- confusionMatrix(predicted_classes_full, valid.df$Efficiency_Class, positive = "1")

# 2. Backward Selection Model
predicted_prob_backward <- predict(backward_glm, newdata = valid.df, type = "response")
predicted_classes_backward <- ifelse(predicted_prob_backward > 0.5, "1", "0")
predicted_classes_backward <- factor(predicted_classes_backward, levels = c("0", "1"))
conf_matrix_backward <- confusionMatrix(predicted_classes_backward, valid.df$Efficiency_Class, positive = "1")

# 3. Stepwise Selection Model
predicted_prob_stepwise <- predict(step_glm, newdata = valid.df, type = "response")
predicted_classes_stepwise <- ifelse(predicted_prob_stepwise > 0.5, "1", "0")
predicted_classes_stepwise <- factor(predicted_classes_stepwise, levels = c("0", "1"))
conf_matrix_stepwise <- confusionMatrix(predicted_classes_stepwise, valid.df$Efficiency_Class, positive = "1")

# 4. Forward Selection Model
predicted_prob_forward <- predict(forward_glm, newdata = valid.df, type = "response")
predicted_classes_forward <- ifelse(predicted_prob_forward > 0.5, "1", "0")
predicted_classes_forward <- factor(predicted_classes_forward, levels = c("0", "1"))
conf_matrix_forward <- confusionMatrix(predicted_classes_forward, valid.df$Efficiency_Class, positive = "1")

# Balanced accuracy
balanced_acc_full <- conf_matrix_full$byClass['Balanced Accuracy']
balanced_acc_backward <- conf_matrix_backward$byClass['Balanced Accuracy']
balanced_acc_stepwise <- conf_matrix_stepwise$byClass['Balanced Accuracy']
balanced_acc_forward <- conf_matrix_forward$byClass['Balanced Accuracy']

# Rbind
balanced_accuracies <- rbind(
  Full_Model = balanced_acc_full,
  Backward_Selection = balanced_acc_backward,
  Stepwise_Selection = balanced_acc_stepwise,
  Forward_Selection = balanced_acc_forward
)

# Print confusion matrices and balanced accuracies
print("Confusion Matrix - Full Model:")
print(conf_matrix_full)
print("Confusion Matrix - Backward Selection:")
print(conf_matrix_backward)
print("Confusion Matrix - Stepwise Selection:")
print(conf_matrix_stepwise)
print("Confusion Matrix - Forward Selection:")
print(conf_matrix_forward)

print("Balanced Accuracies Comparison:")
print(balanced_accuracies)



library(ggplot2)

#
ggplot(amazon, aes(x = Vehicle_condition, fill = Efficiency_Class)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Efficiency Classes by Vehicle Condition",
       x = "Vehicle Condition",
       y = "Count",
       fill = "Efficiency Class") +
  theme_minimal()



###CART TREE FOR EFFICIENCY 
# to reproduce cross validation samples
library(rpart)
library(rpart.plot)
set.seed(123)
# set the smallest value for cp
eff.ct <- rpart(Efficiency_Class ~ ., data = train.df, method = "class", minsplit = 2, 
               cp = 0.00001, xval=5)
prp(eff.ct, type =1, extra = 1, split.font = 2, varlen = -10)

printcp(eff.ct)

#which has the smallesst cp
which.min(eff.ct$cptable[, "xerror"])
# prune the tree with cp that has the min xerror
pruned.ct <- prune(eff.ct, 
                   cp = eff.ct$cptable[which.min(eff.ct$cptable[, "xerror"]), "CP"])

prp(pruned.ct, type =1, extra =1, split.font = 2, varlen = -10)

# Best-pruned tree: use the cp such that the xerror is within min(xerror) + 1xstd 0.41718 0.022732
which.max(eff.ct$cptable[ ,"xerror"]<=0.41718 + 0.022732)

pruned.ct.best <- prune(eff.ct, cp = eff.ct$cptable[which.max(eff.ct$cptable[ ,"xerror"]<0.41718+ 0.022732), "CP"])

prp(pruned.ct.best, type=1, extra = 1, split.font = 1, varlen=-10)


# prediction for unseen data

pruned.ct.best.pred <- predict(pruned.ct.best, valid.df, type = "class")

# confusion matrix
table(prediction = pruned.ct.best.pred, actual = valid.df$Efficiency_Class)


library(caret)  # Ensure the caret package is loaded for confusionMatrix

# Prediction for unseen data using the best-pruned tree
pruned.ct.best.pred <- predict(pruned.ct.best, valid.df, type = "class")


# Generating the confusion matrix to evaluate model performance


# Prediction for unseen data using the best-pruned tree
pruned.ct.best.pred <- predict(pruned.ct.best, valid.df, type = "class")

# Converting predictions and actual labels to factor, ensuring factor levels are consistent
predicted_factors <- factor(pruned.ct.best.pred, levels = c("0", "1"))
actual_factors <- factor(valid.df$Efficiency_Class, levels = c("0", "1"))

# Generating the confusion matrix using predictions, actual labels, and specifying the positive class
conf_matrix <- confusionMatrix(predicted_factors, actual_factors, positive = "1")

# Print the confusion matrix and model performance metrics
print(conf_matrix)
train.df <- na.omit(train.df)

####RF
colnames(amazon)
library(randomForest)
str(amazon)
set.seed(123)
rf <- randomForest(as.factor(Efficiency_Class) ~ ., data = train.df, ntree = 500, 
                   mtry = 4, nodesize = 5, importance=TRUE)


varImpPlot(rf, type=1)


importance(rf,type=1) 
# evaluation
rf.pred <- predict(rf, valid.df)
# Generate the confusion matrix
# Convert predictions to factor ensuring all required levels are present
rf.pred <- factor(rf.pred, levels = levels(valid.df$Efficiency_Class))

# check if the actual classes have all the necessary levels
valid.df$Efficiency_Class <- factor(valid.df$Efficiency_Class, levels = unique(c(valid.df$Efficiency_Class, rf.pred)))

# Check levels
levels(rf.pred)
levels(valid.df$Efficiency_Class)

# Generate the confusion matrix
conf_matrix <- confusionMatrix(rf.pred, valid.df$Efficiency_Class)

# Print the confusion matrix and overall accuracy
print(conf_matrix)




