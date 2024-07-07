# Load libraries
library(readxl)
library(data.table)
library(caret)
library(doParallel)
library(e1071)
library(foreach)
library(openxlsx)  # For writing data to a .xlsx file

# Read the Excel file into a DataFrame
data <- read_xlsx("C:/Users/valer/OneDrive/Desktop/data_pokemon.xlsx")

# Save the names of the Pokémon in a separate variable
pokemon_names <- data$Nome

# Ensure column names are valid variable names in R
names(data) <- make.names(names(data))

# Define the names of categorical and numerical columns
categorical_cols <- c("Type.1", "Type.2", "item.effect", "move.1.type", "status.effect.move.1", 
                      "move.2.type", "status.effect.move.2", "move.3.type", "status.effect.move.3", 
                      "move4.type", "status.effect.move.4", "Nature", "Ability")
numeric_cols <- setdiff(names(data), c(categorical_cols, "Nome", "Target"))

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
  if (col %in% names(data)) {
    data[[col]] <- as.numeric(as.character(data[[col]]))
  }
}

# Check for NA in numeric columns and replace them with the column mean
na_count <- sapply(data[, numeric_cols, drop = FALSE], function(x) sum(is.na(x)))

for (col in names(na_count[na_count > 0])) {
  data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
}

# Remove numeric columns with zero variance
num_data <- data[, numeric_cols, drop = FALSE]
invalid <- which(sapply(num_data, function(x) var(x, na.rm = TRUE)) == 0)

if (length(invalid) > 0) {
  data <- data[, -invalid, drop = FALSE]
  numeric_cols <- numeric_cols[-invalid]
}

# One-hot encode the categorical variables
dummies <- dummyVars(~ ., data = data[, existing_categorical_cols, drop = FALSE])
data_dummies <- predict(dummies, newdata = data)

# Combine numeric variables and one-hot encoded categorical variables
numeric_cols_existing <- numeric_cols[numeric_cols %in% names(data)]
data_combined <- cbind(data[, numeric_cols_existing, drop = FALSE], data_dummies)

# Add the 'Target' column back to the combined data
data_combined$Target <- data$Target
summary(data_combined$Target)
for(i in 1:150){
  if(data_combined$Target[i]<=0.8){
    data_combined$Target[i]="<=0.8"
  }
  else if(data_combined$Target[i]>=0.8){
    data_combined$Target[i]=">=0.8"
  }
}


data_combined$Target <- factor(data_combined$Target)

#The Target variable is the original variable we want to predict. Although it has
#many unique values, the decision to group them in to classes was to 
#ensure that the model has enough data for training and can make meaningful predictions.
#Using the Target variable directly (with transformed levels) helps to simplify the
#model's task by reducing the number of classes it needs to distinguish between.

# Split the data into training and test sets using stratified sampling
set.seed(123)
train_index <- createDataPartition(data_combined$Target, p = 0.8, list = FALSE)
train_set <- data_combined[train_index, ]
test_set <- data_combined[-train_index, ]

# Ensure the levels of Target are the same in train and test sets
train_set$Target <- factor(train_set$Target, levels = levels(data_combined$Target))
test_set$Target <- factor(test_set$Target, levels = levels(data_combined$Target))

########################SVM########################
# Preparing the data by using Target as the target variable.
# The Target variable is used for classification, as it is the original variable we want to predict.
# Grouping less frequent levels into "Other" helps to ensure enough samples per class and avoid instability during training.

# Train an SVM model using the caret library.
# Evaluate the model's performance.
library(e1071)  # For SVM
# Configure parallel computing
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Define the grid of hyperparameters
tune_grid <- expand.grid(C = 2^(-5:2), sigma = 2^(-15:3))

# Train the SVM model with hyperparameter tuning
control <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
svm_model <- train(Target ~ ., data = train_set,
                   method = "svmRadial", trControl = control, tuneGrid = tune_grid)

# Stop the parallel cluster
stopCluster(cl)

# Evaluate the model on the test set
predictions <- predict(svm_model, newdata = test_set)
conf_matrix <- confusionMatrix(predictions, test_set$Target)
print(conf_matrix)

# Print the SVM model
print(svm_model)
# COMMENTS:
# 1. The Target variable was transformed to a factor to ensure compatibility with the SVM model.
# 2. Stratified sampling was used to split the data into training and test sets to maintain the distribution of the target variable.
# 3. Despite the preprocessing steps, the model's performance is not ideal, as indicated by the confusion matrix and Kappa value.
#    The model shows a low accuracy. This indicates that the SVM model is not effectively distinguishing between the different classes.
# 4. The low Kappa value further indicates poor agreement between the predicted and actual classes.
library(ggplot2)
library(caret)
library(e1071)
library(dplyr)

# Dimensionality reduction using PCA for visualization
# PCA is performed on the combined dataset (excluding the 'Target' column) to reduce it to 2D
pca <- prcomp(data_combined[, -which(names(data_combined) %in% c("Target"))], center = TRUE, scale. = TRUE)

# Create a data frame with the first two principal components
pca_data <- data.frame(pca$x[, 1:2])
pca_data$Target <- data_combined$Target

# Plotting the original data points in the PCA-reduced space
ggplot(pca_data, aes(x = PC1, y = PC2, color = Target)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  ggtitle("SVM Decision Boundaries (PCA-reduced Data)")

# Visualizing the decision boundaries using ggplot2 and PCA-reduced data
# Create a grid of points in PCA-reduced space
grid <- expand.grid(PC1 = seq(min(pca_data$PC1), max(pca_data$PC1), length.out = 100),
                    PC2 = seq(min(pca_data$PC2), max(pca_data$PC2), length.out = 100))

# Reconstruct the grid points back to the original feature space
# inverse_pca function to project PCA-reduced points back to original space
inverse_pca <- function(pca_model, pca_data) {
  scores <- as.matrix(pca_data) %*% t(pca_model$rotation[, 1:ncol(pca_data)])
  if (!is.null(pca_model$center)) {
    scores <- scale(scores, center = -pca_model$center, scale = FALSE)
  }
  if (!is.null(pca_model$scale)) {
    scores <- scale(scores, center = FALSE, scale = 1 / pca_model$scale)
  }
  return(scores)
}

# Project the PCA grid back to the original space
reconstructed_grid <- inverse_pca(pca, grid)
reconstructed_grid <- as.data.frame(reconstructed_grid)
colnames(reconstructed_grid) <- colnames(data_combined)[-which(names(data_combined) %in% c("Target"))]

# Predict the class labels for the grid points using the trained SVM model
grid$Target <- predict(svm_model, newdata = reconstructed_grid)

# Plot decision boundaries and data points
ggplot() +
  geom_point(data = pca_data, aes(x = PC1, y = PC2, color = Target), alpha = 0.5) +
  geom_tile(data = grid, aes(x = PC1, y = PC2, fill = Target), alpha = 0.3) +
  theme_minimal() +
  ggtitle("SVM Decision Boundaries (PCA-reduced Data)") +
  scale_fill_manual(values = c("red", "blue", "green", "purple"))
