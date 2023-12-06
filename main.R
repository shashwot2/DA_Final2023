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

github_data <- unique(github_data)
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

# Initialize a vector to store values for 2021
values_2021 <- rep(NA, length(ciks))
file_path <- file.path("./processed_ciks", paste0("CIK0000001750", ".json"))
if (file.exists(file_path)) {
  json_data <- fromJSON(file_path)
  print(json_data$val)
  print(names(json_data))
  
}
# Loop through each CIK
for (i in seq_along(ciks)) {
  cik <- formatted_ciks[i]
  file_path <- file.path(processed_directory, paste0(cik, ".json"))
  
  # Check if the file exists
  if (file.exists(file_path)) {
    # Read the JSON file
    json_data <- fromJSON(file_path)
    print(json_data$val)
    # Since there's only one entry, extract the 'val' field directly if it exists
    if ("val" %in% names(json_data)) {
      values_2021[i] <- json_data$val
    }
    else {
      values_2021[i] <- NA
    }
  }
}

# Print the extracted values
print(values_2021)
print(nrow(combined_data))
print(length(values_2021))
# Add 'values_2021' as a new column to 'combined_data'
combined_data$values_2021 <- values_2021
combined_data$values_2021
# Print out the dataframe to confirm the new column has been added
data_selected <- combined_data %>% select(star_number, pull_requests, commit_count, values_2021)
data_clean <- na.omit(data_selected)

set.seed(42) # for reproducibility
train_indices <- sample(1:nrow(data_clean), 0.8 * nrow(data_clean))
train_data <- data_clean[train_indices, ]
test_data <- data_clean[-train_indices, ]
model_commit_count <- lm(star_number ~ values_2021, data = train_data)
summary(model_commit_count)
# Use the model to make predictions on the test data
predictions <- predict(model_commit_count, test_data)

# Calculate the mean squared error using the actual 'star_number' values and the predictions
mse <- mean((test_data$star_number - predictions)^2)

# Print the mean squared error
print(mse)

model_star_number <- lm(star_number ~ commit_count + pull_requests, data = train_data)
summary(model_star_number)
predictions_star <- predict(model_star_number, test_data)
mse_star <- mean((test_data$star_number - predictions_star)^2)
mse_star

#The model is significant and the predictors (star_number, pull_requests) are both statistically significant.
#The R-squared value is moderately low, suggesting that the model explains some but not all of the variability in the response variable.
# They just have low explanatory power

library(randomForest)
rf_model_commit_count <- randomForest(commit_count ~ star_number + pull_requests, data = train_data, ntree = 500, importance = TRUE)

print(rf_model_commit_count)

predictions_rf_commit <- predict(rf_model_commit_count, test_data)

mse_rf_commit <- mean((test_data$commit_count - predictions_rf_commit)^2)
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

# Correlation matrix
correlation_matrix <- cor(train_data[, c("star_number", "pull_requests")])
print(correlation_matrix)

