# reading primary dataset
github_data <- read.csv("enterprise_projects.csv")

#Initalizing libraries that I need
library(readr)
library(dplyr)
library(parallel)

#Reading secondary sec dataset
sub_data <- read_delim("sub.txt", delim = "\t")

head(sub_data)
head(github_data)
filtered_sub <- sub_data %>% 
  filter(cik %in% big_tech_ciks)

sub_data$name
github_data$dominant_domain
github_data$company_name

matches <- github_data$dominant_domain %in% sub_data$name 
matches_true <- any(matches)
matchesthataretrue<- which(matches_true)
head(github_data)
print(matchesthataretrue)

# Convert both columns to uppercase for case-insensitive comparison
sub_data$name <- toupper(sub_data$name)
github_data$dominant_domain <- toupper(github_data$dominant_domain)

# Function to check if all characters of str_subset are in str_superset
is_superset <- function(str_superset, str_subset) {
  chars_subset <- unlist(strsplit(gsub("[^A-Za-z]", "", str_subset), ""))
  chars_superset <- unlist(strsplit(gsub("[^A-Za-z]", "", str_superset), ""))
  
  all(chars_subset %in% chars_superset)
}

cl <- makeCluster(detectCores() - 1 )
clusterExport(cl, c("sub_data", "is_superset"))

matches <- sapply(github_data$dominant_domain, function(subset_str) {
  any(sapply(sub_data$name, function(superset_str) {
    is_superset(superset_str, subset_str)
  }))
})
stopCluster(cl)
# Filter github_data based on matches
matched_rows <- github_data[matches, ]
matched_rows

github_data <- unique(github_data)
# Normalize strings (convert to lowercase for case-insensitive comparison)
github_data$company_name_normalized <- tolower(github_data$company_name)
github_data$dominant_domain_normalized <- tolower(github_data$dominant_domain)

# Compute the distance matrix
distance_matrix <- stringdist::stringdistmatrix(github_data$company_name_normalized, github_data$dominant_domain_normalized, method = "jw")
threshold <- 0.2
matches <- distance_matrix <= threshold

# Get the row and column indices of matches
matched_pairs <- which(matches, arr.ind = TRUE)

# Create a data frame from the matched pairs
matched_indices <- data.frame(row_index = matched_pairs[, 1], col_index = matched_pairs[, 2])

# Free up memory by removing large objects
rm(distance_matrix, matches)
gc()

# Add the matched names to the data frame
matched_indices$company_name <- github_data$company_name[matched_indices$row_index]
matched_indices$dominant_domain <- github_data$dominant_domain[matched_indices$col_index]

# Display the results
print(matched_indices)

matched_indices <- unique(matched_indices)
print(matched_indices)

# # Apply the function across rows
# matched_rows <- df[sapply(seq_len(nrow(df)), function(i)
#   is_superset(df$owner_login[i], df$matching_column[i])), ]

# Sorting data into descending order so we can take a look at the most commited projects
sorted_data <- github_data[order(-github_data$commit_count),]
head(sorted_data)

#Some summary variables in the commit count
summary(github_data$commit_count)

barplot(github_data$commit_count)
plot(github_data)

data_selected <- github_data %>% select(star_number, pull_requests, commit_count)
data_clean <- na.omit(data_selected)

set.seed(42) # for reproducibility
train_indices <- sample(1:nrow(data_clean), 0.8 * nrow(data_clean))
train_data <- data_clean[train_indices, ]
test_data <- data_clean[-train_indices, ]
model_commit_count <- lm(commit_count ~ star_number + pull_requests, data = train_data)
summary(model_commit_count)
predictions <- predict(model_commit_count, test_data)
mse <- mean((test_data$commit_count - predictions)^2)
mse
model_star_number <- lm(star_number ~ commit_count + pull_requests, data = train_data)
summary(model_star_number)
predictions_star <- predict(model_star_number, test_data)
mse_star <- mean((test_data$star_number - predictions_star)^2)
mse_star