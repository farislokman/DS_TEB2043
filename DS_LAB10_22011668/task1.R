# 1. Load the built-in dataset
data(Theoph)

# 2. Create the Relationship Model
x <- Theoph$Wt
y <- Theoph$Dose
linear_model <- lm(y ~ x) # Apply the lm() function


# 3. Get and Discuss the Summary of the Relationship
print(summary(linear_model))

# 4. Visualize the Regression Graphically
plot(x, y,
     col = "blue", main = "Theophylline Dose vs Weight",
     xlab = "Weight in kg", ylab = "Dose (mg/kg)", pch = 16
)
abline(lm(y ~ x), col = "red", lwd = 2)

# 5. Predicting multiple values
new_weights <- data.frame(x = c(90, 95, 100))
predicted_doses <- predict(linear_model, new_weights)

cat("\nPredicted Doses for 90kg, 95kg, and 100kg:\n")
print(predicted_doses) # [cite: 48]