# Load necessary libraries
library(readxl)
library(data.table)
library(caret)
library(doParallel)
library(randomForest)
library(ggplot2)
library(e1071)
library(foreach)
library(openxlsx)  # For writing data to a .xlsx file

# Read the Excel file into a DataFrame
data <- read_xlsx("data_pokemon.xlsx")

# Save the names of the Pokémon in a separate variable
pokemon_names <- data$Nome

# Remove the 'Nome' column for preprocessing and classification
data <- data[, !(names(data) %in% c("Nome"))]

# Ensure column names are valid variable names in R
names(data) <- make.names(names(data))

# Define the names of categorical and numerical columns
categorical_cols <- c("Type.1", "Type.2", "item.effect", "move.1.type", "status.effect.move.1", 
                      "move.2.type", "status.effect.move.2", "move.3.type", "status.effect.move.3", 
                      "move4.type", "status.effect.move.4", "Nature", "Ability")
numeric_cols <- setdiff(names(data), categorical_cols)

# Check for the existence of categorical columns in the dataset
existing_categorical_cols <- categorical_cols[categorical_cols %in% names(data)]
missing_categorical_cols <- setdiff(categorical_cols, existing_categorical_cols)

if (length(missing_categorical_cols) > 0) {
  warning("The following categorical columns are missing in the dataset: ", paste(missing_categorical_cols, collapse = ", "))
}

# Convert categorical variables to factors
for (col in existing_categorical_cols) {
  data[[col]][is.na(data[[col]])] <- "Unknown"
  data[[col]] <- factor(data[[col]])
}

# Explicitly convert all numeric columns to numeric
for (col in numeric_cols) {
  data[[col]] <- as.numeric(as.character(data[[col]]))
}

# Check for NA in numeric columns and replace them with the column mean
na_count <- sapply(data[, numeric_cols, with = FALSE], function(x) sum(is.na(x)))

for (col in names(na_count[na_count > 0])) {
  data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
}

# Remove numeric columns with zero variance
num_data <- data[, numeric_cols, with = FALSE]
invalid <- which(sapply(num_data, function(x) var(x, na.rm = TRUE)) == 0)

if (length(invalid) > 0) {
  data <- data[, -invalid, with = FALSE]
  numeric_cols <- numeric_cols[-invalid]
}

# One-hot encode the categorical variables
dummies <- dummyVars(~ ., data = data[, existing_categorical_cols, with = FALSE])
data_dummies <- predict(dummies, newdata = data)

# Combine numeric variables and one-hot encoded categorical variables
data_combined <- cbind(data[, numeric_cols, drop = FALSE], data_dummies)

# Create a new target variable suitable for classification
# Binning based on quantiles
quantile_bins <- quantile(data$Target, probs = c(0, 0.25, 0.50, 0.75, 1))

# Assign classes based on quantiles
data_combined$TargetClass <- cut(data$Target, breaks = quantile_bins, labels = 0:3, include.lowest = TRUE)

# Ensure the TargetClass variable is a factor for classification with Random Forest
data_combined$TargetClass <- factor(data_combined$TargetClass, levels = 0:3)

# Remove the Target column from predictors
data_combined <- data_combined[, !(names(data_combined) %in% c("Target"))]

# Split the data into training and test sets
set.seed(123)
sample_index <- sample(1:nrow(data_combined), 0.8 * nrow(data_combined)) # 80% for training set
train_set <- data_combined[sample_index, ]
test_set <- data_combined[-sample_index, ]

########################RANDOM FOREST########################
# Define the grid of hyperparameters to search over
tune_grid <- expand.grid(
  mtry = c(2, 3, 4, 5, 6),      
  ntree = c(100, 90, 75, 60, 40, 30),  
  maxnodes = c(18, 16, 14, 12, 10) 
)

# Custom function to train Random Forest with different ntree and maxnodes
set.seed(123)
custom_rf <- function(mtry, ntree, maxnodes) {
  randomForest(
    TargetClass ~ ., 
    data = train_set,
    mtry = as.numeric(mtry),
    ntree = as.numeric(ntree),
    maxnodes = as.numeric(maxnodes),
    nodesize = 1,
    importance = TRUE
  )
}

# Search for the best combination of parameters
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

results <- foreach(i = 1:nrow(tune_grid), .combine = rbind, .packages = c("randomForest", "caret")) %dopar% {
  set.seed(123)
  mtry <- tune_grid[i, "mtry"]
  ntree <- tune_grid[i, "ntree"]
  maxnodes <- tune_grid[i, "maxnodes"]
  
  model <- custom_rf(mtry, ntree, maxnodes)
  predictions <- predict(model, newdata = test_set)
  confusion_matrix <- caret::confusionMatrix(predictions, test_set$TargetClass)
  accuracy <- confusion_matrix$overall['Accuracy']
  
  # Return a row with parameters and accuracy
  return(c(mtry, ntree, maxnodes, accuracy))
}

# Convert results to a data frame for easier manipulation
results <- as.data.frame(results)
colnames(results) <- c("mtry", "ntree", "maxnodes", "accuracy")

# Find the best parameters based on accuracy
best_accuracy <- max(results$accuracy)
best_params <- results[which.max(results$accuracy), 1:3]

print(paste("Best Accuracy:", best_accuracy))
print(paste("Best Parameters:", paste(best_params, collapse = ", ")))

# Stop the parallel cluster
stopCluster(cl)

# Train the final model with the best parameters found
best_rf_model <- custom_rf(mtry = as.numeric(best_params[1]), ntree = as.numeric(best_params[2]), maxnodes = as.numeric(best_params[3]))

# Evaluate the model on the test set
predictions <- predict(best_rf_model, newdata = test_set)
conf_matrix <- confusionMatrix(predictions, test_set$TargetClass)
print(conf_matrix)

# Extract strong Pokémon classified in the test set
strong_pokemon_test <- test_set[predictions == 3, ]
strong_pokemon_train <- train_set[train_set$TargetClass == 3, ]
strong_pokemon <- rbind(strong_pokemon_train, strong_pokemon_test)

# Convert rownames to numeric and get original indices
strong_pokemon_indices <- as.numeric(rownames(strong_pokemon))

# Retrieve original data for strong Pokémon
strong_pokemon_original <- data[strong_pokemon_indices, ]

# Add the names of the Pokémon
strong_pokemon_original_name <- pokemon_names[strong_pokemon_indices]

strong_pokemon_original <- cbind(strong_pokemon_original_name, strong_pokemon_original)

# Save the strong Pokémon to an .xlsx file
write.xlsx(strong_pokemon_original, "random_forest_results.xlsx", rowNames = FALSE)

# Evaluate feature importance
importance_matrix <- importance(best_rf_model, scale = FALSE)
importance_df <- as.data.frame(importance_matrix)

# Sum the importances of dummy variables to get the importance of the original variables
original_importance <- data.frame(Overall = numeric(), Feature = character())

# Summing the importance of dummy variables for categorical features
for (col in existing_categorical_cols) {
  col_dummies <- grep(paste0("^", make.names(col)), rownames(importance_df), value = TRUE)
  if (length(col_dummies) > 0) {
    original_importance <- rbind(original_importance, data.frame(Overall = sum(importance_df[col_dummies, "MeanDecreaseGini"]), Feature = col))
  }
}

# Adding the numeric variables' importance
numeric_cols <- setdiff(numeric_cols, "Target")

# Create the numeric_importance DataFrame without "target"
numeric_importance <- data.frame(
  Overall = importance_df[make.names(numeric_cols), "MeanDecreaseGini"],
  Feature = numeric_cols
)

# Adding the numeric variables' importance
original_importance <- rbind(original_importance, numeric_importance)

# Sort the features by importance 
original_importance <- original_importance[order(original_importance$Overall, decreasing = TRUE), ]

# Plot the feature importance with a color scale
ggplot(original_importance, aes(x = reorder(Feature, Overall), y = Overall, fill = Overall)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_gradient(low = "red", high = "green") +
  labs(title = "Feature Importance", x = "Feature", y = "Importance")
