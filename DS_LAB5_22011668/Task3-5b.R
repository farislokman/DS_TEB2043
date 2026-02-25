
print("Check whether an n digits number is Armstrong or not:")
print("----------------------------------------------")

# Input number
num <- as.numeric(readline("Input an integer: "))

# Store original number
original_num <- num

# Convert to digits
digits <- as.numeric(strsplit(as.character(num), "")[[1]])

# Number of digits
n <- length(digits)

# Calculate Armstrong sum
armstrong_sum <- sum(digits^n)

# Check Armstrong
if (armstrong_sum == original_num) {
  print(paste(original_num, "is an Armstrong number."))
} else {
  print(paste(original_num, "is not an Armstrong number."))
}
