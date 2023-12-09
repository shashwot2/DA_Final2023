# reading primary dataset
github_data <- read.csv("enterprise_projects.csv")

#Initalizing libraries that I need
library(readr)
library(dplyr)
library(jsonlite)
library(class)
library(caret)
library(ggplot2)
library(e1070)
library(randomForest)
#Reading secondary sec dataset
sub_data <- read_delim("sub.txt", delim = "\t")

#Normalizing name
github_data$company_name_normalized <- tolower(github_data$company_name)
sub_data$name_normalized <- tolower(sub_data$name)

#Constructing distance-matrix using Levenstein
distance_matrix <- stringdist::stringdistmatrix(github_data$company_name_normalized, sub_data$name_normalized, method = "lv")
threshold <- 0.9
matches <- distance_matrix <= threshold

matched_pairs <- which(matches, arr.ind = TRUE)

matched_indices <- data.frame(row_index = matched_pairs[, 1], col_index = matched_pairs[, 2])

#Removing so that we can conserve memory
rm(distance_matrix, matches)
gc()

matched_indices$sub_data_name <- sub_data$name[matched_indices$col_index]
matched_indices$company_name <- github_data$company_name[matched_indices$row_index]

# Subset github_data and sub_data based on matched indices
github_subset <- github_data[matched_indices$row_index, ]
sub_subset <- sub_data[matched_indices$col_index, ]

# Combine the dataframes
combined_data <- cbind(matched_indices, github_subset, sub_subset)

summary(combined_data)

# Sorting data into descending order so we can take a look at the most commited projects
sorted_data <- github_data[order(-github_data$commit_count),]
head(sorted_data)

#Some summary variables in the commit count 
summary(github_data$commit_count)

#getting ciks to query json data
ciks <- combined_data$cik
formatted_ciks <- sprintf("CIK%010d", as.numeric(ciks))
formatted_ciks
ciks <- combined_data$cik

# Format CIKs to be 10 digits long and prepend with "CIK"
formatted_ciks <- sprintf("CIK%010d", as.numeric(ciks))

#proceessed_ciks is where our minimized ciks are stored
processed_directory <- "./processed_ciks"
values_2020 <- rep(NA, length(ciks))
# Loop through each CIK
for (i in seq_along(ciks)) {
  cik <- formatted_ciks[i]
  file_path <- file.path(processed_directory, paste0(cik, ".json"))
  
  if (file.exists(file_path)) {
    json_data <- fromJSON(file_path)
    if ("val" %in% names(json_data)) {
      values_2020[i] <- json_data$val
    }
    else {
      values_2020[i] <- NA
    }
  }
}

combined_data$values_2020 <- values_2020
combined_data$values_2020

summary(combined_data$values_2020)
#Have to subset to reomove NA values
cleaned_combined_data <- subset(combined_data, !is.na(values_2020))

summary(cleaned_combined_data$values_2020)
hist(cleaned_combined_data$values_2020, main="Histogram of Values 2020", xlab="Values", ylab="Frequency")
hist(log(cleaned_combined_data$values_2020), main="Log-transformed Histogram of Values 2020", xlab="Log(Values)", ylab="Frequency")

summary(cleaned_combined_data$committer_count)
hist(cleaned_combined_data$committer_count, main = "Histogram of commiter count")
length(cleaned_combined_data$values_2020)
#Since we want to do analysis on values which has NAs we're going to remove them.

#I want to define a artibrary value of 2 billion and above as High-market cap
# cleaned_combined_data <- cleaned_combined_data %>%
  # mutate(values_2020_group = ifelse(values_2020 >= 2e9, 1, 0))
library(dplyr)
# Adding a new column 'values_group' to classify 'values_2020' as above (1) or below (0) 2 billion
cleaned_combined_data <- cleaned_combined_data %>%
  mutate(above2b = ifelse(values_2020 >= 2e9, "y", "n"))
cleaned_combined_data$above2b
correlation <- cor(cleaned_combined_data$committer_count, cleaned_combined_data$values_2020, use = "complete.obs")
correlation
# Correlation matrix
correlation_matrix <- cor(cleaned_combined_data[, c("star_number", "pull_requests", "values_2020", "committer_count")])
print(correlation_matrix)

# Create a heatmap
heatmap(correlation_matrix, symm = TRUE, 
        main = "Correlation Matrix Heatmap", 
        Colv = NA, Rowv = NA, 
        scale = "none", 
        margins = c(13, 13))
summary(cleaned_combined_data$values_2020)
boxplot(committer_count ~ values_2020, data = cleaned_combined_data, main = "Author Count by pub traded market cap", xlab = "Publicly traded market-cap", ylab = "Author Count")

# data_selected <- cleaned_combined_data %>% select(star_number, pull_requests, commit_count, values_2020)
# data_clean <- na.omit(data_selected)
set.seed(42) # for reproducibility
train_indices <- sample(1:nrow(cleaned_combined_data), 0.8 * nrow(cleaned_combined_data))
train_data <- cleaned_combined_data[train_indices, ]
test_data <- cleaned_combined_data[-train_indices, ]


regression_model_before_outlier <- lm(committer_count ~ values_2020, data = train_data)
predictions <- predict(regression_model_before_outlier, newdata = test_data)
ggplot(test_data, aes(x = values_2020, y = committer_count)) +
  geom_point(color = "blue", alpha = 0.5) +  # Plot the actual data points
  geom_line(aes(y = predictions), color = "red") +  # Add the regression line
  labs(x = "Values 2020", y = "Committer Count", title = "Linear Regression Model Visualization") +
  theme_minimal()

         
cooks_dist <- cooks.distance(regression_model_before_outlier)
plot(cooks_dist, type="h", main="Cook's Distance", ylab="Cook's Distance", xlab="Index")
abline(h = 4/length(cooks_dist), col = "red")

# Identifying the influential points
influential_points <- which(cooks_dist > (4/length(cooks_dist)))
train_data[influential_points, ]
train_data_clean <- train_data[-influential_points, ]

regression_model_clean <- lm(committer_count ~ values_2020, data = train_data_clean)

# Calculate Cook's Distance for the new model
cooks_dist_clean <- cooks.distance(regression_model_clean)
summary(regression_model_before_outlier)
summary(regression_model_clean)
summary(predictions)
# We have identified outliers and 
library(class)
library(caret)
count_above_below_2b <- table(cleaned_combined_data$above2b)

print(count_above_below_2b)

# Adjusting the threshold to 5 billion
cleaned_combined_data$above5b <- ifelse(cleaned_combined_data$values_2020 >= 5e9, "y", "n")

# Count the new distribution
count_above_below_5b <- table(cleaned_combined_data$above5b)
print(count_above_below_5b)

# Adjusting the threshold to 10 billion
cleaned_combined_data$above10b <- ifelse(cleaned_combined_data$values_2020 >= 1e10, "y", "n")

# Count the new distribution
count_above_below_10b <- table(cleaned_combined_data$above10b)
print(count_above_below_10b)
# Load necessary libraries
cleaned_combined_data$above

# Set a seed for reproducibility
set.seed(42)
cleaned_combined_data
print(names(cleaned_combined_data))
# Assuming cleaned_combined_data has columns: committer_count, and count_above_below_10b
# Remove rows with NA values in the columns used for scaling
cleaned_combined_data <- na.omit(cleaned_combined_data[, c("committer_count", "above10b")])

# Split data into train and test sets (80-20 split)
train_indices <- sample(1:nrow(cleaned_combined_data), 0.8 * nrow(cleaned_combined_data))
test_indices <- setdiff(1:nrow(cleaned_combined_data), train_indices)

# Create train and test datasets
train_data <- cleaned_combined_data[train_indices, ]
test_data <- cleaned_combined_data[test_indices, ]

# Extract labels before scaling
train_labels <- train_data$above10b
test_labels <- test_data$above10b

# Scale your features
# Assuming 'committer_count' and 'star_number' are the features
train_data_scaled <- scale(train_data[, c("committer_count")])
test_data_scaled <- scale(test_data[, c("committer_count")])


k <- 3
# Predict on the test data using kNN
test_data_predictions <- knn(train = train_data_scaled, test = test_data_scaled, cl = train_labels, k = k)

# Evaluate the model using a confusion matrix
confusion_matrix <- table(Predicted = test_data_predictions, Actual = test_labels)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
f1_score <- 2 * ((precision * recall) / (precision + recall))

print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))

# Convert the confusion matrix to a data frame for plotting
confusion_matrix_df <- as.data.frame(as.table(confusion_matrix))

# Plot the confusion matrix using ggplot2
ggplot(confusion_matrix_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%d", Freq)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Count",
       title = "Confusion Matrix",
       subtitle = paste("Accuracy:", round(accuracy, 3), 
                        "Precision:", round(precision, 3), 
                        "Recall:", round(recall, 3), 
                        "F1 Score:", round(f1_score, 3)))


# Fit the SVR model
# Here 'values_2021' is used as the predictor
svm_regressor <- svm(committer_count ~ values_2020 + star_number + author_count, data = train_data)
summary(svm_regressor)
train_control <- trainControl(method = "cv", number = 10) # 10-fold CV

set.seed(42) # for reproducibility
svm_model_cv <- train(committer_count ~ values_2020 + star_number + author_count, 
                      data = train_data, 
                      method = "svmRadial", 
                      trControl = train_control)

print(svm_model_cv)

## RANDOM FOREST
train_indices <- sample(1:nrow(cleaned_combined_data), 0.8 * nrow(cleaned_combined_data))
train_data <- cleaned_combined_data[train_indices, ]
test_data <- cleaned_combined_data[-train_indices, ]

rf_model_committer_count <- randomForest(committer_count~ values_2020  + star_number, data = train_data, ntree = 500, importance = TRUE)
predictions_rf_commit <- predict(rf_model_committer_count, test_data)
randomForest::varImpPlot(rf_model_committer_count)
mse_rf_commit <- mean((test_data$star_number - predictions_rf_commit)^2)
summary(rf_model_commit_count)
print(paste("MSE for commit_count:", mse_rf_commit))
