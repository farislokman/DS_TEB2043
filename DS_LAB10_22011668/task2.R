# 1. Install and load necessary packages
#install.packages("e1071")
#install.packages("caTools")
#install.packages("class")

library(e1071)
library(caTools)
library(class)

# 2. Load the dataset
data(ChickWeight)

# 3. Splitting data into train and test data
set.seed(123)
split <- sample.split(ChickWeight$Diet, SplitRatio = 0.7)
train_cl <- subset(ChickWeight, split == TRUE)
test_cl <- subset(ChickWeight, split == FALSE)

# 4. Feature Scaling [cite: 109]
train_scale <- scale(train_cl[, 1:2])
test_scale <- scale(test_cl[, 1:2])

# 5. Choosing the Optimal K value [cite: 128]
k_values <- c(1, 3, 5, 7, 15, 19)

for (k_val in k_values) {
  classifier_knn <- knn(
    train = train_scale,
    test = test_scale,
    cl = train_cl$Diet,
    k = k_val
  )
  
  misClassError <- mean(classifier_knn != test_cl$Diet)
  print(paste("K =", k_val, "| Accuracy =", 1 - misClassError))
}

# 6. Evaluating the Model with the Optimal K
optimal_k <- 5

final_classifier <- knn(
  train = train_scale,
  test = test_scale,
  cl = train_cl$Diet,
  k = optimal_k
)

# Confusion Matrix
cm <- table(test_cl$Diet, final_classifier)
cat("\nConfusion Matrix for Optimal K:\n")
print(cm)