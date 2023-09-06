library(caret)
library(e1071)
data1=analysis_diabetes_prediction_dataset
data1 
accuracies_WOS <- c()

# Check and convert class labels to factors if needed
data1$diabetes <- as.factor(data1$diabetes)

recalls_WOS <- c()
precisions_WOS <- c()
f1_scores_WOS <- c()
data2 <- as.data.frame(data1)
data2
for (i in 1:100) {
  set.seed(i)
  
  train <- data2[sample(1:nrow(data1), 0.8 * nrow(data2)), ]
  test <- data2[-sample(1:nrow(data1), 0.8 * nrow(data2)), ]
  
  model <- naiveBayes(train[, 1:4], train[, 5])
  
  predictions <- predict(model, test[, 1:4])
  
  
   accuracy <- mean(predictions == test[, 5])
  accuracies_WOS[i] <- accuracy
  # Create a confusion matrix
  cm <- confusionMatrix(predictions, test$diabetes)
  
  recall <- cm[["byClass"]]["Recall"]
  precision <- cm[["byClass"]]["Precision"]
  f1 <- cm[["byClass"]]["F1"]
  
  recalls_WOS[i] <- recall
  precisions_WOS[i] <- precision
  f1_scores_WOS[i] <- f1
}

print(accuracies_WOS)

mean(accuracies_WOS)

# Print the results
print("Accuracies:")
print(accuracies_WOS)
mean_accuracy <- mean(accuracies_WOS)
print(paste("Mean Accuracy:", mean_accuracy))

print("Recalls:")
print(recalls_WOS)
mean_recall <- mean(recalls_WOS)
print(paste("Mean Recall:", mean_recall))

print("Precisions:")
print(precisions_WOS)
mean_precision <- mean(precisions_WOS)
print(paste("Mean Precision:", mean_precision))

print("F1-Scores:")
print(f1_scores_WOS)
mean_f1 <- mean(f1_scores_WOS)
print(paste("Mean F1-Score:", mean_f1))









boxplot(precisions_WOS, precisions_SYS, precisions, precisions_STS, names=c("No Sampling", "Systematic Sampling", "Simple Random Sampling", "Stratified Sampling"))

# Define a custom scaling function
custom_scale <- function(x, new_min, new_max) {
  scaled <- (x - min(x)) / (max(x) - min(x))  # Scale between 0 and 1
  scaled * (new_max - new_min) + new_min      # Scale to the desired range
}

# Define the new range (1 to 2)
new_min <- 0.5
new_max <- 1

# Apply custom scaling to each group
scaled_accuracies_WOS <- custom_scale(f1_scores_WOS, new_min, new_max)
scaled_accuracies_SYS <- custom_scale(f1_scores_SYS, new_min, new_max)
scaled_accuracies_SRS <- custom_scale(f1_scores, new_min, new_max)
scaled_accuracies_STS <- custom_scale(f1_scores_STS, new_min, new_max)

# Create the boxplot with the scaled data
boxplot(
  scaled_accuracies_WOS, scaled_accuracies_SYS, scaled_accuracies_SRS, accuracies_STS,
  names = c("WOS", "SYS", "SRS", "STS")
)
# Define colors for each group
box_colors <- c("red", "blue", "green", "purple")

# Create the boxplot with scaled data and specified colors
boxplot(
  scaled_accuracies_WOS, scaled_accuracies_SYS, scaled_accuracies_SRS, scaled_accuracies_STS,
  names = c("WOS", "SYS", "SRS", "STS"),
  col = box_colors
)








# Create vectors
precisions_WOS 
precisions_STS 
precisions_SYS
precisions_SRS
factors <- as.factor(rep(1:4, each = 100))
# Combine vectors into a data frame
data <- data.frame(precision_WOS, precisions_SYS, precisions_STS, precisions_SRS, factors)

# Conduct ANOVA test
aov_results <- aov(precisions_WOS ~ factors, data=data)


# Conduct ANOVA test
#aov_results <- aov(cbind(accuracies_WOS, accuracies_SYS, accuracies_SRS, accuracies_STS) ~ as.factor(rep(1:4, each=10)))

# Print the results
summary(aov_results)

###########################################
# Create a factor variable with 4 groups, each containing 100 data points
factors <- as.factor(rep(1:4, each = 100))

# Generate example data for the other variables (replace this with your actual data)
recalls_WOS <- rnorm(400)
recalls_SYS <- rnorm(400)
recalls_SRS <- rnorm(400)
recalls_STS <- rnorm(400)

# Combine vectors into a data frame
data <- data.frame(recalls_WOS, recalls_SYS, recalls_STS, recalls_SRS, factors)

# Conduct ANOVA test
aov_results <- aov(recalls_WOS ~ factors, data = data)

# Print ANOVA summary
summary(aov_results)
