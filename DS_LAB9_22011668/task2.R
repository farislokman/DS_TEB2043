
library(caret) 

# 2. Load the dataset
data_raw <- mtcars

# 3. Log Transformation 
# Note: This works best on positive values (which mtcars has)
data_log <- log(data_raw)

# 4. Standard Scaling (Z-score)
# scale() returns a matrix by default, so as.data.frame is good practice
data_standard <- as.data.frame(scale(data_raw))

# 5. Min-Max Scaling (0 to 1 Range)
# preProcess is powerful because it saves the scaling rules for future data
process_minmax <- preProcess(data_raw, method = c("range"))
data_minmax <- predict(process_minmax, data_raw)

# --- COMPARISON SUMMARY ---
# I've wrapped these in a list to make the output easier to read in the console
comparison_list <- list(
  Raw = summary(data_raw$mpg),
  Log = summary(data_log$mpg),
  Standardized = summary(data_standard$mpg),
  MinMax = summary(data_minmax$mpg)
)

print(comparison_list)

# Quick check on the ranges
cat("\nRange Check for Min-Max 'mpg':", range(data_minmax$mpg))
cat("\nMean for Standardized 'mpg' (should be 0):", round(mean(data_standard$mpg), 2))