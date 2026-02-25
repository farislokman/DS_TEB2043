
# Input integer
n <- as.numeric(readline("Input an integer: "))

# Loop to display cube
for (i in 1:n) {
  print(paste("Number is:", i,
              "and cube of", i, "is:", i^3))
}
