# Install required packages (only once)
install.packages(c("rpart", "caret", "ggplot2", "reshape2",
                   "rpart.plot"))
# Load required libraries
library(rpart)
library(caret)
library(ggplot2)
library(reshape2)
library(rpart.plot)
# Load the dataset
data <- read.csv("C:/Users/Avanthika/Downloads/diabetes.csv")
# Convert Outcome to factor
data$Outcome <- as.factor(data$Outcome)
# Train-test split (80% training, 20% testing)
set.seed(42)
trainIndex <- createDataPartition(data$Outcome, p = 0.8, list
                                  = FALSE)trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]
# Train CART model
cart_model <- rpart(Outcome ~ ., data = trainData, method =
                      "class", control = rpart.control(cp = 0.01))
# Plot the Decision Tree
rpart.plot(cart_model, main = "CART Decision Tree for
Diabetes Prediction", type = 2, extra = 104, fallen.leaves =
             TRUE)
# Predict on test set
predictions <- predict(cart_model, testData, type = "class")
# Print Confusion Matrix (text)
conf_matrix <- confusionMatrix(predictions,
                               testData$Outcome)
print(conf_matrix)
# Plot Confusion Matrix (heatmap)
cm <- table(Predicted = predictions, Actual =
              testData$Outcome)cm_melted <- melt(cm)
ggplot(data = cm_melted, aes(x = Actual, y = Predicted, fill =
                               value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), size = 5, color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  ggtitle("Confusion Matrix: CART Decision Tree on Diabetes
Data")
