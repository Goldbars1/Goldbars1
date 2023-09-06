accuracies_SRS <- c()
data1
for (i in 1:100) {
set.seed(i)

sample <- sample(1:nrow(data1), 0.7 * nrow(data1))
sample

train <- data1[sample, ]
test <- data1[-sample, ]

model <- naiveBayes(diabetes ~ ., data = train)
model

predictions <- predict(model, test)

accuracy <- mean(predictions == test$diabetes)

accuracies_SRS[i] <- accuracy
}
print(accuracies_SRS)
mean(accuracies_SRS)




############################
library(caret)
library(e1071)

# Load your actual dataset here
data1 <- analysis_diabetes_prediction_dataset_2_
data1
# Check and convert class labels to factors if needed
data1$diabetes <- as.factor(data1$diabetes)

accuracies_SRS <- c()
recalls_SRS <- c()
precisions_SRS <- c()
f1_scores_SRS <- c()

for (i in 1:100) {
  set.seed(i)
  
  sample <- sample(1:nrow(data1), 0.7 * nrow(data1))
  
  train <- data1[sample, ]
  test <- data1[-sample, ]
  
  model <- naiveBayes(diabetes ~ ., data = train)
  
  predictions <- predict(model, test)
  
  accuracy <- mean(predictions == test$diabetes)
  accuracies_SRS[i] <- accuracy
  
  # Create a confusion matrix
  cm <- confusionMatrix(predictions, test$diabetes)
  
  recall <- cm[["byClass"]]["Recall"]
  precision <- cm[["byClass"]]["Precision"]
  f1 <- cm[["byClass"]]["F1"]
  
  recalls_SRS[i] <- recall
  precisions_SRS[i] <- precision
  f1_scores_SRS[i] <- f1
}

# Print the results
print("Accuracies:")
print(accuracies_SRS)
mean_accuracy <- mean(accuracies_SRS)
print(paste("Mean Accuracy:", mean_accuracy))

print("Recalls:")
print(recalls_SRS)
mean_recall <- mean(recalls_SRS)
print(paste("Mean Recall:", mean_recall))

print("Precisions:")
print(precisions_SRS)
mean_precision <- mean(precisions_SRS)
print(paste("Mean Precision:", mean_precision))

print("F1-Scores:")
print(f1_scores_SRS)
mean_f1 <- mean(f1_scores_SRS)
print(paste("Mean F1-Score:", mean_f1))
