# Creating an array with dimensions (3x3x2)
arr <- array(6:23, dim = c(3, 3, 2))
print(arr)

# Accessing Elements in an Array
# Create a 3x3x2 array
arr <- array(6:23, dim = c(3, 3, 2))
# Access element at [2nd row, 3rd column, 1st layer]
print(arr[2, 3, 1])  

# Access entire 2nd row from Layer 1
print(arr[2, , 1])

# Access entire 3rd column from Layer 2
print(arr[, 3, 2])

#Performing Operations on Arrays
# Creating two 3x3x2 arrays
arr1 <- array(4:21, dim = c(3, 3, 2))
arr2 <- array(25:40, dim = c(3, 3, 2))
# Element-wise addition
sum_arr <- arr1 + arr2
print(sum_arr)

# Element-wise multiplication
prod_arr <- arr1 * arr2
print(prod_arr)

# Applying Functions to Arrays
# Creating an array
arr <- array(1:15, dim = c(3, 2, 2))
# Sum of all elements in the array
print(sum(arr))

# Mean of all elements
print(mean(arr))

# Apply function to each row (margin = 1)
apply(arr, MARGIN = 1, FUN = sum)

# Apply function to each column (margin = 2)
apply(arr, MARGIN = 2, FUN = mean)

