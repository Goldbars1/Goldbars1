library(caret)
library(e1071)

# Load your actual dataset here
data1 <- analysis_diabetes_prediction_dataset_2_

# Check and convert class labels to factors if needed
data1$diabetes <- as.factor(data1$diabetes)

accuracies_SYS <- c()
recalls_SYS <- c()
precisions_SYS <- c()
f1_scores_SYS <- c()

for (i in 1:100) {

features <- c("gender", "age", "hypertension", "smoking_history", 
              "heart_disease", "bmi", "HbA1c_level", "blood_glucose_level", "diabetes")

set.seed(i)
train_index <- sample(nrow(data1), 0.7 * nrow(data1))
train_data <- data1[train_index, features]
train_data
test_data <- data1[-train_index, features]
test_data

model <- naiveBayes(diabetes ~ ., data = train_data)

predictions <- predict(model, test_data)

accuracy_SYS <- mean(predictions == test_data$diabetes)

accuracies_SYS[i] <- accuracy_SYS
# Create a confusion matrix
cm <- confusionMatrix(predictions, test_data$diabetes)

recall <- cm[["byClass"]]["Recall"]
precision <- cm[["byClass"]]["Precision"]
f1 <- cm[["byClass"]]["F1"]

recalls_SYS[i] <- recall
precisions_SYS[i] <- precision
f1_scores_SYS[i] <- f1
}

print(accuracies_SYS)

mean(accuracies_SYS)

# Print the results
print("Accuracies:")
print(accuracies_SYS)
mean_accuracy <- mean(accuracies_SYS)
print(paste("Mean Accuracy:", mean_accuracy))

print("Recalls:")
print(recalls_SYS)
mean_recall <- mean(recalls_SYS)
print(paste("Mean Recall:", mean_recall))

print("Precisions:")
print(precisions_SYS)
mean_precision <- mean(precisions_SYS)
print(paste("Mean Precision:", mean_precision))

print("F1-Scores:")
print(f1_scores_SYS)
mean_f1 <- mean(f1_scores_SYS)
print(paste("Mean F1-Score:", mean_f1))








boxplot(accuracies_WOS, accuracies_SYS, accuracies_SRS, accuracies_STS, names=c("No Sampling", "Systematic Sampling", "Simple Random Sampling", "Systematic Sampling"))

# Conduct ANOVA test
aov_results <- aov(cbind(accuracies_WOS, accuracies_SYS, accuracies_SRS, accuracies_STS) ~ as.factor(rep(1:4, each=4)))

# Print the results
summary(aov_results)
