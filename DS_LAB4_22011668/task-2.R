V1 <- c(2,3,1,5,4,6,8,7,9)

# Matrix 1
Matrix1 <- matrix(V1, nrow = 3, ncol = 3, byrow = TRUE)

#rename row col
rownames(Matrix1) <- c("R1","R2","R3")
colnames(Matrix1) <- c("C1","C2","C3")

cat("Matrix-1:\n")
print(Matrix1)

# Transpose
Matrix2 <- t(Matrix1)

# Rename rows and columns
rownames(Matrix2) <- c("R1","R2","R3")
colnames(Matrix2) <- c("C1","C2","C3")

cat("\nMatrix-2 (Transpose of Matrix-1):\n")
print(Matrix2)

add_matrix <- Matrix1 + Matrix2
sub_matrix <- Matrix1 - Matrix2
mul_matrix <- Matrix1 * Matrix2
div_matrix <- round(Matrix1/Matrix2,2)

cat("\nAddition Matrix1 + Matrix2:\n")
print(add_matrix)

cat("\nSubtraction Matrix1 - Matrix2:\n")
print(sub_matrix)

cat("\nElement-wise Multiplication Matrix1 * Matrix2:\n")
print(mul_matrix)

cat("\nElement-wise Division Matrix1 / Matrix2:\n")
print(div_matrix)
