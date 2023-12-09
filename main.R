# reading primary dataset
github_data <- read.csv("enterprise_projects.csv")

#Initalizing libraries that I need
library(readr)
library(dplyr)
library(parallel)
library(jsonlite)
#Reading secondary sec dataset
sub_data <- read_delim("sub.txt", delim = "\t")
sub_data
github_data
head(sub_data)
head(github_data)
filtered_sub <- sub_data
  filter(cik %in% big_tech_ciks)
sub_data$name
github_data$dominant_domain
github_data$company_name

# matches <- github_data$dominant_domain %in% sub_data$name 
# matches_true <- any(matches)
# matchesthataretrue<- which(matches_true)
# head(github_data)
# print(matchesthataretrue)

# Convert both columns to uppercase for case-insensitive comparison
# sub_data$name <- toupper(sub_data$name)
# github_data$dominant_domain <- toupper(github_data$dominant_domain)
# 
# # Function to check if all characters of str_subset are in str_superset
# is_superset <- function(str_superset, str_subset) {
#   chars_subset <- unlist(strsplit(gsub("[^A-Za-z]", "", str_subset), ""))
#   chars_superset <- unlist(strsplit(gsub("[^A-Za-z]", "", str_superset), ""))
#   
#   all(chars_subset %in% chars_superset)
# }
# 
# cl <- makeCluster(detectCores() - 1 )
# clusterExport(cl, c("sub_data", "is_superset"))

# matches <- sapply(github_data$dominant_domain, function(subset_str) {
#   any(sapply(sub_data$name, function(superset_str) {
#     is_superset(superset_str, subset_str)
#   }))
# })
# stopCluster(cl)
# Filter github_data based on matches
# matched_rows <- github_data[matches, ]
# matched_rows

# github_data <- unique(github_data)
# Normalize strings (convert to lowercase for case-insensitive comparison)
github_data$company_name_normalized <- tolower(github_data$company_name)
github_data$dominant_domain_normalized <- tolower(github_data$dominant_domain)
sub_data$name_normalized <- tolower(sub_data$name)

distance_matrix <- stringdist::stringdistmatrix(github_data$company_name_normalized, sub_data$name_normalized, method = "lv")
threshold <- 0.9
matches <- distance_matrix <= threshold

matched_pairs <- which(matches, arr.ind = TRUE)

matched_indices <- data.frame(row_index = matched_pairs[, 1], col_index = matched_pairs[, 2])

rm(distance_matrix, matches)
gc()

matched_indices$sub_data_name <- sub_data$name[matched_indices$col_index]
# matched_indices$dominant_domain <- github_data$dominant_domain[matched_indices$col_index]
matched_indices$company_name <- github_data$company_name[matched_indices$row_index]
matched_indices
matched
head(github_data$dominant_domain)
head(sub_data$name)
print(matched_indices)

# Subset github_data and sub_data based on matched indices
github_subset <- github_data[matched_indices$row_index, ]
sub_subset <- sub_data[matched_indices$col_index, ]

# Combine the dataframes
combined_data <- cbind(matched_indices, github_subset, sub_subset)

head(combined_data)
summary(combined_data)

cleaned_data[is.infinite(cleaned_data)] <- NA

cleaned_data <- na.omit(cleaned_data)

sum(is.na(cleaned_data))
dim(cleaned_data)
pca_result <- prcomp(cleaned_data, center = TRUE)
pca_result <- prcomp(normalized_data, center = TRUE)

summary(pca_result)
plot(pca_result, type = "lines")
biplot(pca_result)

 # matched_rows <- df[sapply(seq_len(nrow(df)), function(i)
#   is_superset(df$owner_login[i], df$matching_column[i])), ]

# Sorting data into descending order so we can take a look at the most commited projects
sorted_data <- github_data[order(-github_data$commit_count),]
head(sorted_data)

#Some summary variables in the commit count 
summary(github_data$commit_count)

barplot(github_data$commit_count)
plot(github_data)

ciks <- combined_data$cik
ciks
formatted_ciks <- sprintf("CIK%010d", as.numeric(ciks))
formatted_ciks
library(jsonlite)# Assuming 'combined_data' is your dataframe and 'cik' is the name of the CIK column
ciks <- combined_data$cik

# Format CIKs to be 10 digits long and prepend with "CIK"
formatted_ciks <- sprintf("CIK%010d", as.numeric(ciks))

# Directory where processed JSON files are stored
processed_directory <- "./processed_ciks"

# Initialize a vector to store values for 2020
values_2020 <- rep(NA, length(ciks))
# Loop through each CIK
for (i in seq_along(ciks)) {
  cik <- formatted_ciks[i]
  file_path <- file.path(processed_directory, paste0(cik, ".json"))
  
  # Check if the file exists
  if (file.exists(file_path)) {
    # Read the JSON file
    json_data <- fromJSON(file_path)
    # Since there's only one entry, extract the 'val' field directly if it exists
    if ("val" %in% names(json_data)) {
      values_2020[i] <- json_data$val
    }
    else {
      values_2020[i] <- NA
    }
  }
}

# Print the extracted values
print(values_2020)
print(nrow(combined_data))
print(length(values_2020))
# Add 'values_2020' as a new column to 'combined_data'
combined_data$values_2020 <- values_2020
combined_data$values_2020

summary(combined_data$values_2020)
cleaned_combined_data <- subset(combined_data, !is.na(values_2020))
library(ggplot2)
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
library(ggplot2)
library(class)
library(caret)

# Assuming data_selected_scaled has only two features: "committer_count" and "star_number"
# and the target variable "above10b"

# Create a sequence of values for each feature
x_seq <- seq(min(data_selected_scaled[, "committer_count"]), max(data_selected_scaled[, "committer_count"]), length.out = 200)
y_seq <- seq(min(data_selected_scaled[, "star_number"]), max(data_selected_scaled[, "star_number"]), length.out = 200)

# Create a grid
grid <- expand.grid(committer_count = x_seq, star_number = y_seq)

# Predict on the grid
grid$predicted <- knn(train = train_data, test = grid, cl = train_labels, k = k)

# Convert factors to numeric for ggplot (if needed)
grid$predicted_num <- as.numeric(as.factor(grid$predicted))

# Plot
ggplot() +
  geom_tile(data = grid, aes(x = committer_count, y = star_number, fill = predicted_num)) +
  geom_point(data = data_selected, aes(x = committer_count, y = star_number, color = as.factor(above10b)), size = 2) +
  scale_fill_gradient(low = "lightblue", high = "salmon") +
  scale_color_manual(values = c("blue", "red")) +
  labs(fill = "Prediction", color = "Actual") +
  theme_minimal()

library(e1071)
set.seed(42) # for reproducibility
train_indices <- sample(1:nrow(cleaned_combined_data), 0.8 * nrow(cleaned_combined_data))
train_data <- cleaned_combined_data[train_indices, ]
test_data <- cleaned_combined_data[-train_indices, ]

#scaling
train_data$values_2020 <- scale(train_data$values_2020)
test_data$values_2020 <- scale(test_data$values_2020)

# Scale the dependent variable
train_data$committer_count <- scale(train_data$committer_count)
test_data$committer_count <- scale(test_data$committer_count)

train_data
test_data

# Fit the SVR model
# Here 'values_2021' is used as the predictor
svm_regressor <- svm(committer_count ~ values_2020, data = train_data,kernel = 'radial', type = 'eps-regression')
# Make predictions
svr_predictions <- predict(svm_regressor, newdata = test_data)
# str(test_data)

summary(svr_predictions)
length(svr_predictions)
train_data_2 <- createDataPartition(data_selected[[target]], p = .8, list = FALSE)
svm_comparison_regressor <- svm(committer_count ~ values_2020, data = train_data_2, type = 'eps-regression')
true_values <- test_data$committer_count
true_values <- na.omit(true_values)
summary(true_values)
mse_value <- mean((svr_predictions - true_values)^2)

mse_value <- ifelse(any(is.nan(svr_predictions)), NaN, mean((svr_predictions - true_values)^2))
rmse_value <- ifelse(is.nan(mse_value), NaN, sqrt(mse_value))
mae_value <- mean(abs(svr_predictions - true_values), na.rm = TRUE)
rss <- sum((svr_predictions - true_values)^2, na.rm = TRUE)
tss <- sum((true_values - mean(true_values, na.rm = TRUE))^2, na.rm = TRUE)
r_squared <- ifelse(tss == 0, NaN, 1 - rss / tss)

# Print the results
cat("MSE:", mse_value, "\n")
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")
cat("R-squared:", r_squared, "\n")

# Predict committer_count
svr_predictions <- predict(svm_regressor, newdata = train_data)
# Assuming 'predictions' contains the predicted values from the SVR model
# and 'true_values' contains the true values from the test set

# Calculate MSE (Mean Squared Error)
mse_value <- mean((svr_predictions - test_data$commiter_count)^2)

# Calculate RMSE (Root Mean Squared Error)
rmse_value <- sqrt(mse_value)

# Calculate MAE (Mean Absolute Error)
mae_value <- mean(abs(svr_predictions - test_data$committer_count))

# Calculate R-squared
ss_res <- sum((svr_predictions - test_data$committer_count)^2)
ss_tot <- sum((test_data$committer_count - mean(test_data$committer_count))^2)
r_squared <- 1 - ss_res / ss_tot

# Print the results
cat("MSE:", mse_value, "\n")
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")
cat("R-squared:", r_squared, "\n")


library(randomForest)
train_indices <- sample(1:nrow(cleaned_combined_data), 0.8 * nrow(cleaned_combined_data))
train_data <- cleaned_combined_data[train_indices, ]
test_data <- cleaned_combined_data[-train_indices, ]

rf_model_commit_count <- randomForest(committer_count~ values_2020 + author_count + star_number, data = train_data, ntree = 500, importance = TRUE)
summary(rf_model_commit_count)
predictions_rf_commit <- predict(rf_model_commit_count, test_data)

mse_rf_commit <- mean((test_data$star_number - predictions_rf_commit)^2)
print(paste("MSE for commit_count:", mse_rf_commit))
# Need to tune and cross validate

randomForestModel <- randomForest(committer_count ~ values_2020 + author_count, )
numeric_data <- combined_data[, sapply(combined_data, is.numeric)]
numeric_data
# Identify constant columns
constant_columns <- apply(standardized_data, 2, function(x) length(unique(x)) == 1)
constant_columns
standardized_data <- standardized_data[, !constant_columns]
standardized_data
standardized_data <- scale(numeric_data)
standardized_data
standardized_data_cleaned <- na.omit(standardized_data)

dim(standardized_data_cleaned)
constant_columns <- apply(standardized_data_cleaned, 2, var) == 0

standardized_data_no_constant <- standardized_data_cleaned[, !constant_columns]

dim(standardized_data_no_constant)

pca_result <- prcomp(standardized_data_no_constant, center = TRUE, scale. = TRUE)

summary(pca_result)

pca_scores <- pca_result$x[, 1:2]

str(standardized_data_cleaned)
if (!is.data.frame(standardized_data_cleaned)) {
  standardized_data_cleaned <- as.data.frame(standardized_data_cleaned)
}
plot(pca_scores[,1], pca_scores[,2], xlab="PC1", ylab="PC2", main="PCA - First Two Principal Components")
plot(pca_scores[,1], pca_scores[,2], col=standardized_data_cleaned$your_category, xlab="PC1", ylab="PC2", main="PCA - First Two Principal Components")
legend("topright", legend=levels(factor(standardized_data_cleaned$your_category)), col=1:length(levels(factor(standardized_data_cleaned$your_category))), pch=1)

categories <- your_original_data$CategoryVariable
unique_categories <- unique(categories)
colors <- rainbow(length(unique_categories))

plot(pca_scores[,1], pca_scores[,2], col=colors[as.factor(categories)], pch=16,
     xlab="PC1", ylab="PC2", main="PCA - Colored by Category")

legend("topright", legend=unique_categories, col=colors, pch=16)

model_commit_count <- lm(star_number ~ values_2020, data = train_data)
summary(model_commit_count)
predictions <- predict(model_commit_count, test_data)

model_star_number <- lm(star_number ~ commit_count + pull_requests, data = train_data)
summary(model_star_number)
predictions_star <- predict(model_star_number, test_data)
mse_star <- mean((test_data$star_number - predictions_star)^2)
mse_star
head(combined_data)
rf_model_commit_count <- randomForest(star_number~ values_2020 + author_count, data = train_data, ntree = 500, importance = TRUE)

print(rf_model_commit_count)

predictions_rf_commit <- predict(rf_model_commit_count, test_data)

mse_rf_commit <- mean((test_data$star_number - predictions_rf_commit)^2)
print(paste("MSE for commit_count:", mse_rf_commit))
# Need to tune and cross validate

library(e1071)
svm_model_commit_count <- svm(commit_count ~ star_number + pull_requests, data = train_data)
summary(svm_model_commit_count)
predictions_svm_commit <- predict(svm_model_commit_count, test_data)
mse_svm_commit <- mean((test_data$commit_count - predictions_svm_commit)^2)
print(paste("MSE for commit_count:", mse_svm_commit))

svm_model_star_number <- svm(star_number ~ commit_count + pull_requests, data = train_data)
summary(svm_model_star_number)
predictions_svm_star <- predict(svm_model_star_number, test_data)
mse_svm_star <- mean((test_data$star_number - predictions_svm_star)^2)
print(paste("MSE for star_number:", mse_svm_star))


