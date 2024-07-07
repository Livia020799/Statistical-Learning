# Libraries
library(clustMixType)
library(readxl)
library(data.table)
library(caret)
library(ggplot2)
library(dplyr)
library(openxlsx)  
library(cluster)   

# Read the Excel file into a DataFrame
data <- read_xlsx("C:/Users/valer/OneDrive/Desktop/data_pokemon.xlsx")

# Keep the 'Nome' column
pokemon_names <- data$Nome

# Remove the 'Nome' column for clustering
data_class <- data[, !(names(data) %in% c("Nome"))]

# Ensure column names are valid variable names in R
names(data_class) <- make.names(names(data_class))

# Define the names of categorical and numerical columns
categorical_cols <- c("Type.1", "Type.2", "item.effect", "move.1.type", "status.effect.move.1", 
                      "move.2.type", "status.effect.move.2", "move.3.type", "status.effect.move.3", 
                      "move4.type", "status.effect.move.4", "Nature", "Ability")
numeric_cols <- setdiff(names(data_class), c(categorical_cols, "Target"))


# Check for the existence of categorical columns in the dataset
existing_categorical_cols <- categorical_cols[categorical_cols %in% names(data_class)]
missing_categorical_cols <- setdiff(categorical_cols, existing_categorical_cols)

if (length(missing_categorical_cols) > 0) {
  warning("The following categorical columns are missing in the dataset: ", paste(missing_categorical_cols, collapse = ", "))
}

# Transform categorical variables into factors
for (col in existing_categorical_cols) {
  data_class[[col]][is.na(data_class[[col]])] <- "Unknown"
  data_class[[col]] <- factor(data_class[[col]])
}

# Explicitly convert all numeric columns to numeric
for (col in numeric_cols) {
  if (col %in% names(data_class)) {
    data_class[[col]] <- as.numeric(as.character(data_class[[col]]))
  }
}




# Check for NA in numeric columns and replace them with the column mean
na_count <- sapply(data_class[, numeric_cols, drop = FALSE], function(x) sum(is.na(x)))

for (col in names(na_count[na_count > 0])) {
  data_class[[col]][is.na(data_class[[col]])] <- mean(data_class[[col]], na.rm = TRUE)
}

# Perform K-Prototypes Clustering
set.seed(2347)  # For reproducibility
kproto_result <- kproto(data_class, k = 3, iter.max = 100, nstart = 10)

# Assign clusters to the data
data_class$Cluster <- kproto_result$cluster

# Add the Pokémon names back to the classified data
data_class$Nome <- pokemon_names

# Adjust the 'Target' variable if necessary
for (i in 1:nrow(data_class)) {
  if (data_class$Target[i] > 5) {
    data_class$Target[i] <- 0.01 * data_class$Target[i] + 5
  }
}

# Calculate the sum of the 'Target' variable for each cluster
cluster_sums <- aggregate(Target ~ Cluster, data = data_class, sum)
clust_leng <- sapply(1:3, function(cluster) length(data_class$Cluster[data_class$Cluster == cluster]))

# Identify the cluster with the highest average 'Target' value
strong_cluster <- cluster_sums$Cluster[which.max(cluster_sums$Target / clust_leng)]

# Extract the Pokémon belonging to the strong cluster
strong_pokemon <- data_class[data_class$Cluster == strong_cluster, ]

# Save the strong Pokémon to an XLSX file
write.xlsx(strong_pokemon, "k_prototype_results.xlsx", rowNames = FALSE)

# Print the names of strong Pokémon
print(head(strong_pokemon$Nome))

# Calculate feature importance using silhouette score
calculate_silhouette_score <- function(data, clusters, feature) {
  # Calculate the Gower distance for mixed data types
  gower_dist <- daisy(data[, feature, drop = FALSE], metric = "gower")
  
  # Calculate silhouette width for each data point
  silhouette_scores <- silhouette(clusters, gower_dist)
  
  # Average silhouette width per cluster
  avg_silhouette <- aggregate(silhouette_scores[, 3], by = list(cluster = silhouette_scores[, 1]), FUN = mean)
  
  # Return the average silhouette score for the feature
  return(mean(avg_silhouette$x))
}

# Compute the silhouette scores for each feature
silhouette_scores <- sapply(c(numeric_cols, existing_categorical_cols), function(feature) {
  calculate_silhouette_score(data_class, data_class$Cluster, feature)
})

# Create a dataframe for feature importance
importance_df <- data.frame(
  Feature = names(silhouette_scores),
  Importance = silhouette_scores
)

# Plot the feature importance with a color gradient from red to green
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "red", high = "green") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Feature Importance for K-Prototypes (Silhouette Score)", x = "Feature", y = "Importance")

# Plot clusters formed by K-Prototypes
ggplot(data_class, aes(x = factor(Cluster), fill = factor(Cluster))) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Distribution of Clusters", x = "Cluster", y = "Count")

# Scatter plot of clusters with the original features
plot_features <- function(data, feature1, feature2) {
  ggplot(data, aes_string(x = feature1, y = feature2, color = "factor(Cluster)")) +
    geom_point(alpha = 0.7) +
    theme_minimal() +
    labs(title = paste("Clusters by", feature1, "and", feature2), x = feature1, y = feature2)
}

# scatter plot
plot_features(data_class, "Nature", "Evs.atk")

