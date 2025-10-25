# Creating a 3x3 matrix (filled column-wise by default)
mat <- matrix(5:13, nrow = 3, ncol = 3)
print(mat) 

# Filling a Matrix Row-Wise
mat <- matrix(7:15, nrow = 3, byrow = TRUE)
print(mat) 

# Naming Rows and Columns
# Creating a matrix
mat <- matrix(4:12, nrow = 3)

# Assigning row and column names
rownames(mat) <- c("Row1", "Row2", "Row3")
colnames(mat) <- c("Col1", "Col2", "Col3")

print(mat)

# Accessing Elements in a Matrix
# Create a 3x3 matrix
mat <- matrix(8:16, nrow = 3)

# Access element at row 2, column 3
print(mat[2, 3])

# Access entire row 1
print(mat[1, ])

# Access entire column 2
print(mat[, 2])

# Matrix Arithmetic
mat1 <- matrix(4:7, nrow = 2)
mat2 <- matrix(1:4, nrow = 2)

# Matrix addition
sum_mat <- mat1 + mat2
print(sum_mat)

# Matrix multiplication (element-wise)
prod_mat <- mat1 * mat2
print(prod_mat)

# Matrix multiplication (dot product)
dot_prod_mat <- mat1 %*% mat2  # %*% for matrix multiplication
print(dot_prod_mat)

#Transpose and Inverse of a Matrix
# Transpose of a matrix
t_mat <- t(mat)
print(t_mat)

# Inverse of a matrix (for square matrices)
square_mat <- matrix(c(5, 8, 7, 6), nrow = 2)
inv_mat <- solve(square_mat)
print(inv_mat)