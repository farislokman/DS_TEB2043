# Combine values 1:24 to fill 2x4x3
Array1 <- array(1:24, dim = c(2,4,3))

# Print Array1
cat("Array1\n")
print(Array1)


# Combine values 25:54 to fill 3x2x5
Array2 <- array(25:54, dim = c(3,2,5))

# Print Array2
cat("Array2\n")
print(Array2)


cat("The second row of the second matrix of the array:\n")
print(Array1[2, , 2])  # row 2, all columns, matrix 2
cat("The element in the 3rd row and 3rd column of the 1st matrix:\n")
print(Array2[3, 2, 1])