library(caret)
library(e1071)
data1=analysis_diabetes_prediction_dataset_2_
data1$diabetes <- as.factor(data1$diabetes)

accuracies_STS <- c()
recalls_STS <- c()
precisions_STS <- c()
f1_scores_STS <- c()

for (i in 1:100) {
strata <- data1$diabetes
sample <- createDataPartition(strata, p = 0.75, list = FALSE)
train <- data1[sample, ]
test <- data1[-sample, ]

model <- naiveBayes(diabetes ~ ., data = train)

predictions <- predict(model, test)

accuracy <- mean(predictions == test$diabetes)

accuracies_STS[i] <- accuracy
# Create a confusion matrix
cm <- confusionMatrix(predictions, test$diabetes)

recall <- cm[["byClass"]]["Recall"]
precision <- cm[["byClass"]]["Precision"]
f1 <- cm[["byClass"]]["F1"]

recalls_STS[i] <- recall
precisions_STS[i] <- precision
f1_scores_STS[i] <- f1
}

accuracies_STS
mean(accuracies_STS)
# Print the results
print("Accuracies:")
print(accuracies_STS)
mean_accuracy <- mean(accuracies_STS)
print(paste("Mean Accuracy:", mean_accuracy))

print("Recalls:")
print(recalls_STS)
mean_recall <- mean(recalls_STS)
print(paste("Mean Recall:", mean_recall))

print("Precisions:")
print(precisions_STS)
mean_precision <- mean(precisions_STS)
print(paste("Mean Precision:", mean_precision))

print("F1-Scores:")
print(f1_scores_STS)
mean_f1 <- mean(f1_scores_STS)
print(paste("Mean F1-Score:", mean_f1))





