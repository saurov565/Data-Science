# Creating Different Types of Vectors
# Numeric Vector
num_vec <- c(15, 28, 38, 46, 59)
print(num_vec)

# Character Vector
char_vec <- c("Apple", "Samsung", "Xiaomi")
print(char_vec)

# Logical Vector
bol_vec <- c(0, 1, 1, 0)
print(bol_vec)

# Vector Operations
# Arithmetic Operations
vec1 <- c(2, 4, 6)
vec2 <- c(1, 3, 5)

sum_vec <- vec1 + vec2  # Element-wise addition
prod_vec <- vec1 * vec2 # Element-wise multiplication

print(sum_vec) 
print(prod_vec)

# Accessing Elements in a Vector
# Create a vector
num_vec <- c(10, 15, 25, 30, 35, 40, 45, 50)

# Access elements using index (1-based index)
print(num_vec[2]) 

# Access multiple elements
print(num_vec[c(1, 4, 3)])

# Access elements using a condition
print(num_vec[num_vec > 25])

# Modifying a Vector
# Modify an element
num_vec[2] <- 100
print(num_vec) 

# Append new elements
num_vec <- c(num_vec, 60, 70)
print(num_vec) 

# Vector Functions
vec <- c(5, 10, 15, 20, 25, 34, 55, 60, 70, 88, 90)

# Length of the vector
print(length(vec))

# Sum of all elements
print(sum(vec)) 

# Mean (average) of elements
print(mean(vec)) 

# Sorting a vector
sorted_vec <- sort(vec, decreasing = TRUE)
print(sorted_vec) 

# Sequence and Repetition in Vectors
# Sequence from 1 to 10
seq_vec <- seq(1, 10, by = 2)  # Steps of 2
print(seq_vec) 

# Repeat elements
rep_vec <- rep(c(1, 2, 3), times = 3)  # Repeat entire vector
print(rep_vec)