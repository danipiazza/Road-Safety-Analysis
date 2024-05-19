library(readxl)
library(dplyr)
library(fastDummies)
library(ggplot2)
library(ggthemes)
library(janitor)
library(corrplot)
library(formattable)
library(randomForest)
library(caret)
library(pROC)
library(DMwR)
library(xgboost)
library(reshape2)

#2022 data
casualty <- read.csv("UK/uk_casualty.csv") 
collision <- read.csv("UK/uk_collision.csv")
vehicle <- read.csv("UK/uk_vehicle.csv")
description_var <- readxl::read_xlsx("UK/description.xlsx")
data <- merge(vehicle, collision, by = c("accident_index","accident_year","accident_reference"))

nrow(data)
data$month <- as.integer(sapply(strsplit(data$date, split = "/"), function(x) x[2]))
data <- data %>% select(-c("police_force","date","local_authority_district","local_authority_ons_district","local_authority_highway","first_road_number","second_road_number","junction_detail","lsoa_of_driver","lsoa_of_accident_location","accident_year",
                              "vehicle_direction_from","vehicle_direction_to","vehicle_location_restricted_lane","vehicle_leaving_carriageway","journey_purpose_of_driver","driver_imd_decile","driver_home_area_type",
                              "special_conditions_at_site","generic_make_model","location_easting_osgr",
                              "location_northing_osgr","longitude","latitude","vehicle_manoeuvre","junction_location","propulsion_code","junction_control","engine_capacity_cc","age_of_vehicle"))
#"engine_capacity_cc" and "age_of_vehicle" have more than 30.000 null values, decided to remove them
data <- data %>% 
  mutate_all(~ ifelse(. == -1, NA, .))
data <- data %>% 
  mutate_all(~ ifelse(. == "NULL", NA, .))
data <- na.omit(data)
nrow(data)
data$hour <- as.integer(sapply(strsplit(data$time, split = ":"), function(x) x[1]))
data <- data %>% select(-c("time"))



#first_road_class 6 (unclassified),speed_limit 99	(unknown)
# second_road_class	6	(Unclassified), second_road_class	9	(Unknown)
data <- data %>%
  filter(
    speed_limit != 99,
    first_road_class != 6,
    second_road_class != 6,
    second_road_class != 9,
    pedestrian_crossing_human_control != 9
  )

  
    #road_type1 => 0,1 each category
# 1 Roundabout
# 2 One way street
# 3 Dual carriageway
# 6 Single carriageway
# 7 Slip road
# 9 Unknown
# 12 One way street/Slip road
data <- data %>%
  mutate(road_type_name = case_when(
    road_type == 1 ~ "Roundabout",
    road_type == 2 ~ "One way street",
    road_type == 3 ~ "Dual carriageway",
    road_type == 6 ~ "Single carriageway",
    road_type == 7 ~ "Slip road",
    road_type == 12 ~ "One way street/Slip road",
    TRUE ~ "Unknown"
  ))  %>% 
  filter(road_type_name != "Unknown") %>% 
  filter(road_type_name != "Dual carriageway") %>% 
  dummy_cols(select_columns = "road_type_name",split="/",omit_colname_prefix = TRUE) %>% 
  select(-c("road_type","road_type_name"))



    #pedestrian_crossing_physical_facilities => 0,1
# 0	No physical crossing facilities within 50 meters
# 1	Zebra
# 4	Pelican, puffin, toucan or similar non-junction pedestrian light crossing
# 5	Pedestrian phase at traffic signal junction
# 7	Footbridge or subway
# 8	Central refuge
data$pedestrian_crossing_physical_facilities <- ifelse(data$pedestrian_crossing_physical_facilities == 0,0,1)

    #light_conditions (1,2,3)
# 1	Daylight
# 4	Darkness - lights lit
# 5	Darkness - lights unlit (merge *)
# 6	Darkness - no lighting (merge *)
# 7	Darkness - lighting unknown (drop)
data <- data %>% filter(light_conditions != 7)
data$light_conditions <- ifelse(data$light_conditions == 1, 1,ifelse(data$light_conditions == 4,2,3))

    #weather_conditions => 0,1 each
# 1	Fine no high winds
# 2	Raining no high winds
# 3	Snowing no high winds
# 4	Fine + high winds
# 5	Raining + high winds
# 6	Snowing + high winds
# 7	Fog or mist
# 8	Other
# 9	Unknown
data <- data %>%
  mutate(weather_conditions_name = case_when(
    weather_conditions == 1 ~ "Fine",
    weather_conditions == 2 ~ "Raining",
    weather_conditions == 3 ~ "Snowing",
    weather_conditions == 4 ~ "Fine/High winds",
    weather_conditions == 5 ~ "Raining/High winds",
    weather_conditions == 6 ~ "Snowing/High winds",
    weather_conditions == 7 ~ "Fog or mist",
    TRUE ~ "Unknown"
  )) %>% filter(weather_conditions_name != "Unknown") %>% 
  dummy_cols(select_columns = "weather_conditions_name",split="/",omit_colname_prefix = TRUE) %>% 
  select(-c("weather_conditions","weather_conditions_name","Fine"))

  #road_surface_conditions => 0,1 
# 1	Dry
# 2	Wet or damp
# 3	Snow
# 4	Frost or ice
# 5	Flood over 3cm. deep
# 6	Oil or diesel
# 7	Mud
# 9	unknown (self reported)
data <- data %>% filter(road_surface_conditions != 9)
data$road_dry <- ifelse(data$road_surface_conditions == 1, 1,0)
data <- data %>% select(-c("road_surface_conditions"))

    #carriageway_hazards => 0,1
# 0	None
# 1	Vehicle load on road
# 2	Other object on road
# 3	Previous accident
# 4	Dog on road
# 5	Other animal on road
# 6	Pedestrian in carriageway - not injured
# 7	Any animal in carriageway (except ridden horse)
# 9	unknown (self reported)
data <- data %>% filter(carriageway_hazards != 9)
data$carriageway_hazards <- ifelse(data$carriageway_hazards == 0, 0,1)
data <- data %>% select(-c("carriageway_hazards"))

    #vehicle_type => 0,1 for each category (group some)
# 1	Pedal cycle => Bike -> 2 wheels
# 2	Motorcycle 50cc and under => Scooter -> 2 wheels
# 3	Motorcycle 125cc and under => Scooter -> 2 wheels
# 4	Motorcycle over 125cc and up to 500cc => Motorcycle -> 2 wheels
# 5	Motorcycle over 500cc => Motorcycle -> 2 wheels
# 8	Taxi/Private hire car => Taxi -> 4 wheels
# 9	Car => Car -> 4 wheels
# 10	Minibus (8 - 16 passenger seats)  => Public Transport -> other
# 11	Bus or coach (17 or more pass seats) => Public Transport -> other
# 16	Ridden horse => Horse -> other
# 17	Agricultural vehicle => Drop -> other
# 18	Tram => Public Transport -> other
# 19	Van / Goods 3.5 tonnes mgw or under => Goods -> other
# 20	Goods over 3.5t. and under 7.5t => Goods -> other
# 21	Goods 7.5 tonnes mgw and over => Goods -> other
# 22	Mobility scooter => Scooter -> 2 wheels
# 23	Electric motorcycle => Motorcycle -> 2 wheels
# 90	Other vehicle => Drop 
# 97	Motorcycle - unknown cc => Drop -> 2 wheels
# 98	Goods vehicle - unknown weight => Goods -> other
# 99	Unknown vehicle type (self rep only) => Drop
# 103	Motorcycle - Scooter (1979-1998) => Scooter -> 2 wheels
# 104	Motorcycle (1979-1998) => Motorcycle -> 2 wheels
# 105	Motorcycle - Combination (1979-1998) => Motorcycle -> 2 wheels
# 106	Motorcycle over 125cc (1999-2004) => Motorcycle-> 2 wheels
# 108	Taxi (excluding private hire cars) (1979-2004) => Taxi-> 4 wheels
# 109	Car (including private hire cars) (1979-2004) => Car-> 4 wheels
# 110	Minibus/Motor caravan (1979-1998) => Bus -> other
# 113	Goods over 3.5 tonnes (1979-1998) => Goods -> other
data <- data %>%
  mutate(vehicle_type_name = case_when(
    vehicle_type == 1 ~ "2_wheels",
    vehicle_type == 2 ~ "2_wheels",
    vehicle_type == 3 ~ "2_wheels",
    vehicle_type == 4 ~ "2_wheels",
    vehicle_type == 5 ~ "2_wheels",
    vehicle_type == 8 ~ "4_wheels",
    vehicle_type == 9 ~ "4_wheels",
    vehicle_type == 10 ~ "other_vehicle",
    vehicle_type == 11 ~ "other_vehicle",
    vehicle_type == 16 ~ "other_vehicle",
    vehicle_type == 17 ~ "other_vehicle",
    vehicle_type == 18 ~ "other_vehicle",
    vehicle_type == 19 ~ "other_vehicle",
    vehicle_type == 20 ~ "other_vehicle",
    vehicle_type == 21 ~ "other_vehicle",
    vehicle_type == 22 ~ "2_wheels",
    vehicle_type == 23 ~ "2_wheels",
    vehicle_type == 90 ~ "Drop",
    vehicle_type == 97 ~ "2_wheels",
    vehicle_type == 98 ~ "other_vehicle",
    vehicle_type == 99 ~ "Drop",
    vehicle_type == 103 ~ "2_wheels",
    vehicle_type == 104 ~ "2_wheels",
    vehicle_type == 105 ~ "2_wheels",
    vehicle_type == 106 ~ "2_wheels",
    vehicle_type == 108 ~ "4_wheels",
    vehicle_type == 109 ~ "4_wheels",
    vehicle_type == 110 ~ "other_vehicle",
    vehicle_type == 113 ~ "other_vehicle",
    TRUE ~ "Drop"
  )) %>%
  filter(vehicle_type_name != "Drop") %>% 
  dummy_cols(select_columns = "vehicle_type_name", omit_colname_prefix = TRUE) %>%
  select(-c("vehicle_type", "vehicle_type_name","2_wheels"))


    #towing_and_articulation => 0,1
# 0	No tow/articulation
# 1	Articulated vehicle
# 2	Double or multiple trailer
# 3	Caravan
# 4	Single trailer
# 5	Other tow
# 9	unknown 
data <- data %>% filter(towing_and_articulation != 9)
data$towing_and_articulation <- ifelse(data$towing_and_articulation == 0, 0,1)

    #skidding_and_overturning => 0,1 for each
# 0	None
# 1	Skidded
# 2	Skidded and overturned
# 3	Jackknifed
# 4	Jackknifed and overturned
# 5	Overturned
# 9	unknown 
data <- data %>%
  mutate(skidding_and_overturning_name = case_when(
    skidding_and_overturning == 0 ~ "None",
    skidding_and_overturning == 1 ~ "Skidded",
    skidding_and_overturning == 2 ~ "Skidded/Overturned",
    skidding_and_overturning == 3 ~ "Jackknifed",
    skidding_and_overturning == 4 ~ "Jackknifed/Overturned",
    skidding_and_overturning == 5 ~ "Overturned",
    TRUE ~ "Unknown"
  )) %>% filter(skidding_and_overturning_name != "Unknown") %>% 
  dummy_cols(select_columns = "skidding_and_overturning_name",split="/",omit_colname_prefix = TRUE) %>% 
  select(-c("skidding_and_overturning_name","skidding_and_overturning","None"))



    #hit_object_in_carriageway => 0,1
# 0	None
# 1	Previous accident
# 2	Road works
# 4	Parked vehicle
# 5	Bridge (roof)
# 6	Bridge (side)
# 7	Bollard or refuge
# 8	Open door of vehicle
# 9	Central island of roundabout
# 10	Kerb
# 11	Other object
# 12	Any animal (except ridden horse)
# 99	unknown (self reported)
data <- data %>% filter(hit_object_in_carriageway != 99)
data$hit_object_in_carriageway <- ifelse(data$hit_object_in_carriageway == 0, 0,1)

    #hit_object_off_carriageway => 0,1
# 0	None
# 1	Road sign or traffic signal
# 2	Lamp post
# 3	Telegraph or electricity pole
# 4	Tree
# 5	Bus stop or bus shelter
# 6	Central crash barrier
# 7	Near/Offside crash barrier
# 8	Submerged in water
# 9	Entered ditch
# 10	Other permanent object
# 11	Wall or fence
# 99	unknown (self reported)
data <- data %>% filter(hit_object_off_carriageway != 99)
data$hit_object_off_carriageway <- ifelse(data$hit_object_off_carriageway == 0, 0,1)


    #first_point_of_impact => 0,1 
# 0	Did not impact
# 1	Front
# 2	Back
# 3	Offside
# 4	Nearside
# 9	unknown 
data <- data %>% filter(first_point_of_impact != 9)
data$impact <- ifelse(data$first_point_of_impact == 0, 0,1)
data <- data %>% select(-c("first_point_of_impact"))


    #vehicle_left_hand_drive => 0,1
# 1	No
# 2	Yes
# 9	Unknown
data <- data %>% filter(vehicle_left_hand_drive != 9)
data$vehicle_left_hand_drive <- ifelse(data$vehicle_left_hand_drive == 1, 0,1)

    #sex_of_driver => 0,1
# 1	Male
# 2	Female
# 3	Not known
data <- data %>% filter(sex_of_driver != 3)
data$male_driver <- ifelse(data$sex_of_driver == 1, 1,0)
data <- data %>% select(-c("sex_of_driver"))

  #accident_severity
# 	1	Fatal
# 	2	Serious
# 	3	Slight
data$accident_severity <- ifelse(data$accident_severity == 3, 0,1)

#####################################################

#EDA

count_accident_severity <- data %>%
  count(accident_severity) %>% 
  arrange(accident_severity)


ggplot(count_accident_severity, aes(x = reorder(accident_severity, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#557bb4") +
  labs(title = "Count by Accident Severity", x = "Accident Severity", y = "Count") +
  theme_minimal()

bar_plot <- function(data,var){
  ggplot(data, aes(x = factor(var), y = n, fill = factor(accident_severity))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Number of accident per month for each severity",
      x = "Month",
      y = "Count",
      fill = "Accident severity"
    ) +
    scale_fill_manual(values = c("0" = "#557bb4", "1" = "#f98510"), 
                      labels = c("Slight", "Severe")) +
    theme_minimal()
}
count_month_severity <- data %>%
  count(month, accident_severity)
bar_plot(count_month_severity,count_month_severity$month)

count_day_severity <- data %>%
  count(day_of_week, accident_severity)
bar_plot(count_day_severity,count_day_severity$day_of_week)

count_hour_severity <- data %>%
  count(hour, accident_severity)
bar_plot(count_hour_severity,count_hour_severity$hour)

#morning, afternoon, evening, night
data <- data %>%
  mutate(
    time_of_day = case_when(
      hour >= 5 & hour <= 12 ~ "morning",
      hour >= 13 & hour <= 18 ~ "afternoon",
      hour >= 19 & hour <= 23 ~ "evening",
      hour >= 0 & hour <= 4 ~ "night"
    )
  )


data$time_of_day <- factor(data$time_of_day, levels = c("morning", "afternoon", "evening", "night"))

count_time_severity <- data %>%
  count(time_of_day, accident_severity)
bar_plot(count_time_severity,count_time_severity$time_of_day)

count_speed_severity <- data %>%
  count(speed_limit, accident_severity)
ggplot(data, aes(x = accident_severity, y = speed_limit, fill = factor(accident_severity))) + 
  geom_boxplot() +
  labs(
    title = "Speed limit distribution for each accident severity",
    x = "Severity",
    y = "Speed limit"
  ) +
  scale_fill_manual(
    values = c("0" = "#557bb4", "1" = "#f98510"), 
    labels = c("Slight", "Severe")
  ) +
  theme_minimal()




data$month <- factor(data$month, levels = 1:12, labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))  
data$day_of_week <- factor(data$day_of_week, levels = 1:7, labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) 

unique_accidents <- data %>%
  group_by(accident_index) %>%
  summarize(
    month = first(month),  
    day_of_week = first(day_of_week),  
    hour = first(hour) 
  )

incident_count <- unique_accidents %>%
  count(month, day_of_week, hour)

ggplot(incident_count, aes(x = factor(month), y = factor(day_of_week, labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), fill = n)) +
  geom_tile() +
  labs(
    title = "Heatmap of Incidents by Month, Day of the Week, and Hour",
    x = "Month",
    y = "Day of the Week",
    fill = "Number of Incidents"
  ) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal()

ggplot(incident_count, aes(x = factor(hour), y = factor(day_of_week, labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), fill = n)) +
  geom_tile() +
  labs(
    title = "Heatmap of Incidents by Month, Day of the Week, and Hour",
    x = "Hour",
    y = "Day of the Week",
    fill = "Number of Incidents"
  ) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal()

data <- clean_names(data)
num_data <- data %>% select(-c("accident_index","accident_reference","vehicle_reference","age_of_driver","time_of_day","day_of_week","month"))
std_devs <- apply(num_data, 2, sd)
num_data <- num_data[, std_devs != 0] # remove the ones with 0 standard deviation (constant)
cor_matrix <- cor(num_data)

corrplot(cor_matrix, method = "circle", tl.cex = 0.3, tl.srt = 45) 
#############
#Prediction

# Function to split the dataset into train and test
split_data <- function(data, train_ratio = 0.8) {
  set.seed(123)  # for reproducibility
  index_train <- sample(nrow(data), size = train_ratio * nrow(data))
  train_set <- data[index_train,]
  test_set <- data[-index_train,]
  list(train_set = train_set, test_set = test_set)
}

# Function to create a logistic regression model
logistic_model <- function(train_data, test_data, sampling_method) {
  model <- glm(accident_severity ~ ., family = binomial(), data = train_data)
  
  predictions <- round(predict(model, newdata = test_data, type = "response"))
  
  roc_curve <- roc(test_data$accident_severity, predictions)
  auc_value <- auc(roc_curve)
  
  conf_matrix <- caret::confusionMatrix(
    data = factor(predictions, levels = c("0", "1")),
    reference = as.factor(test_data$accident_severity),
    positive = "1"
  )
  print(conf_matrix$table)
  list(
    auc = auc_value,
    accuracy = conf_matrix$overall["Accuracy"],
    precision = conf_matrix$byClass["Precision"],
    recall = conf_matrix$byClass["Recall"],
    f1 = conf_matrix$byClass["F1"]
  )
}
# Function to train and test models
train_model <- function(train_data, test_data, model_method, tune_grid, train_control) {
  train_data$accident_severity <- as.factor(train_data$accident_severity)
  levels(train_data$accident_severity) <- make.names(levels(train_data$accident_severity))
  
  fit <- train(
    accident_severity ~ .,
    data = train_data,
    method = model_method,
    tuneGrid = tune_grid,
    trControl = train_control,
    metric = "ROC"
  )

  pred<-predict(fit, test_data)
  pred<- as.factor(ifelse(pred == "X1", 1, 0))
  prob <- predict(fit, test_data, type = "prob")
  
  conf_matrix <- caret::confusionMatrix(
    data = as.factor(pred),
    reference = as.factor(test_data$accident_severity),
    positive = "1"
  )
  print(conf_matrix$table)
  
  roc_curve <- roc(test_data$accident_severity, prob[, "X1"])
  auc_value <- auc(roc_curve)
  
  list(
    auc = auc_value,
    accuracy = conf_matrix$overall["Accuracy"],
    precision = conf_matrix$byClass["Precision"],
    recall = conf_matrix$byClass["Recall"],
    f1 = conf_matrix$byClass["F1"]
  )
}

#Function to balance the data
create_balanced_data <- function(train_data, method = "none") {
  train_pos <- train_data[train_data$accident_severity == 1, ]
  train_neg <- train_data[train_data$accident_severity == 0, ]
  
  if (method == "oversampling") {
    balanced_train <- rbind(
      sample_n(train_pos, size = nrow(train_neg), replace = TRUE),
      train_neg
    )
  } else if (method == "SMOTE") {
    train_data$accident_severity <- as.factor(train_data$accident_severity)
    levels(train_data$accident_severity) <- make.names(levels(train_data$accident_severity))
    
    balanced_train <- SMOTE(accident_severity ~ ., data = train_data, perc.over = 100, perc.under = 200)
  } else {
    balanced_train <- train_data
  }
  
  balanced_train[sample(nrow(balanced_train)), ]  
}


create_result_row <- function(algorithm_name, metrics) {
  data.frame(
    Algorithm = algorithm_name,
    Accuracy = metrics$accuracy,
    Precision = metrics$precision,
    Recall = metrics$recall,
    F1_Score = metrics$f1,
    AUC = metrics$auc,
    stringsAsFactors = FALSE
  )
}

# Parameter grid for Random Forest
tune_grid_rf <- expand.grid(
  .mtry = 10,  # Number of features to consider for each split
  .splitrule = "gini"
)

# Parameter grid for Gradient Boosting
tune_grid_gb <- expand.grid(
  nrounds = 100,  # Number of boosting rounds
  eta = c(0.2, 0.4),  # Learning rate, controls the contribution of each tree
  max_depth = 10, 
  gamma = 0,  # Minimum reduction in loss required to split a node
  colsample_bytree = 1,  
  min_child_weight = 1,  
  subsample = 1  
)

# Training control for cross-validation and performance metrics
train_control <- trainControl(
  method = "cv",  # Cross-validation method
  number = 10,  # Number of cross-validation folds
  classProbs = TRUE,  
  summaryFunction = twoClassSummary  
)

data_split <- split_data(num_data)
train_set <- data_split$train_set
test_set <- data_split$test_set
train_set %>% group_by(accident_severity) %>%summarize(Number = n())

train_over <- create_balanced_data(train_set, "oversampling")
train_over %>% group_by(accident_severity) %>% summarize(Number = n())

train_smote <- create_balanced_data(train_set, "SMOTE")
train_smote %>% group_by(accident_severity) %>% summarize(Number = n())

results <- data.frame(
  Algorithm = character(),
  Accuracy = numeric(),
  Precision = numeric(),
  Recall = numeric(),
  F1_Score = numeric(),
  AUC = numeric(),
  stringsAsFactors = FALSE
)

# Logistic Model 
log_results <- logistic_model(train_set, test_set)
results <- rbind(results, create_result_row("Logistic", log_results))

# Logistic Model - Oversampling
log_over_results <- logistic_model(train_over, test_set, "oversampling")
results <- rbind(results,create_result_row("Logistic (Over)", log_over_results))

# Logistic Model - Smote
log_smote_results <- logistic_model(train_smote, test_set, "oversampling")
results <- rbind(results,create_result_row("Logistic (Smote)", log_smote_results))

# Random Forest 
rf_results <- train_model(train_set, test_set, "ranger", tune_grid_rf, train_control)
results <- rbind(results, create_result_row("Random Forest", rf_results))

# Random Forest - Oversampling
rf_over_results <- train_model(train_over, test_set, "ranger", tune_grid_rf, train_control)
results <- rbind(results,create_result_row("Random Forest (Over)", rf_over_results))

# Random Forest - SMOTE
rf_smote_results <- train_model(train_smote, test_set, "ranger", tune_grid_rf, train_control)
results <- rbind(results, create_result_row("Random Forest (Smote)", rf_smote_results))

# Gradient Boosting 
gb_results <- train_model(train_set, test_set, "xgbTree", tune_grid_gb, train_control)
results <- rbind(results, create_result_row("Gradient Boosting", gb_results))

# Gradient Boosting - Oversampling
gb_over_results <- train_model(train_over, test_set, "xgbTree", tune_grid_gb, train_control)
results <- rbind(results, create_result_row("Gradient Boosting Over", gb_over_results))

# Gradient Boosting - SMOTE
gb_smote_results <- train_model(train_smote, test_set, "xgbTree", tune_grid_gb, train_control)
results <- rbind(results, create_result_row("Gradient Boosting (Smote)", gb_smote_results))


rownames(results) <- NULL
results_long <- reshape2::melt(results, id.vars = "Algorithm", 
                               variable.name = "Metric", 
                               value.name = "Value")

ggplot(data = results_long, aes(x = Algorithm, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Comparison of Metrics Across Algorithms",
    x = "Algorithm",
    y = "Value",
    fill = "Metric"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


apply_formattable_color <- function(df) {
  styles <- lapply(names(df)[-1], function(col) {
    max_val <- max(df[[col]]) 
    formatter("span", style = x ~ ifelse(x == max_val, 
                                         style(color = "white", background = "green"), 
                                         NA))
  })
  names(styles) <- names(df)[-1]
  formattable(df, styles)
}

formattable_df <- apply_formattable_color(results)

formattable_df
