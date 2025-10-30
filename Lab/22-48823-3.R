x <- 4
if (x >3 ) {
  print("x is greater than 3")
}

x <- 6
if (x > 5) {
  print("x is greater than 5")
} else {
  print("x is 5 or less")
}

score <- 85
if (score >= 90) {
  print("Grade A+")
} else if (score >= 85) {
  print("Grade A")
} else if (score >= 80){
  print("Grade B+")
} else if (score >= 70) {
  print("Grade C")
} else {
  print("Grade F")
}

for (i in 1:7) {
  print(paste("Iteration", i))
}

#repeat Loop (with break)
i <- 1
repeat {
  print(i)
  i <- i + 1
  if (i > 6) break
}

#next Statement (skip to next iteration)
for (i in 1:8) {
  if (i == 4) next
  print(i)
}

#break Statement (exit the loop)
for (i in 1:10) {
  if (i == 8) break
  print(i)
}

#mean()
numbers <- c(5, 15, 10, 20, 25)
mean(numbers)
sum(numbers)
length(numbers)

#round()
pi_val <- 3.14159
round(pi_val, 4)  

paste("Hello", "World")

#Simple function to add two numbers
add_numbers <- function(a, b) {
  return(a + b)
}
add_numbers(7, 3)

#Function to check if a number is even
is_even <- function(x) {
  if (x %% 2 == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
is_even(5)

#Function with default parameter
greet <- function(name = "World") {
  paste("Hello", name)
}
greet() 
greet("Abir")

#Anonymous (Lambda) Function with sapply()
numbers <- 2:8
squared <- sapply(numbers, function(x) x^2)
print(squared)

#Reading a CSV File
data <- read.csv("/Users/abulbasharsaurov/Downloads/employee_dataset.csv")
head(data)

#Reading a Text File (tab-delimited)
data <- read.table("/Users/abulbasharsaurov/Downloads/employee_dataset_ordered.txt", header = TRUE, sep = "\t")
head(data)

#Reading Data from a URL
url <- "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"
data <- read.csv(url)
head(data)

#Exercise 1
score <- 75
if (score >= 90){
  print("Excellent")
} else if (score >= 75){
  print("Good")
} else if(score >= 50){
  print("Pass")
} else{
  print("Fail")
}

#Exercise 2
numbers <- 1:10
squares <- c()
for(num in numbers){
  squares <- c(squares, num^2)
}
print(squares)

#Exercise 3
count <- 1
while(count < 20){
  if(count %% 2== 0){
    print(count)
  }
  count <- count+1
}

#Exercise 4
multiply <- function(a, b){
  product <- a * b
  return(product)
}
result <- multiply(5,7)
print(result)

#Exercise 5
calculate_stats <- function(numbers){
  mean_val <- mean(numbers)
  median_val <- median(numbers)
  sd_val <- sd(numbers)
  
  return(list(
    Mean = mean_val,
    Median = median_val,
    Standard_Deviation = sd_val
  ))
}
value <- c(5,10,15,20,25)
result <- calculate_stats(value)
print(result)

#Exercise 6
grade_result <- function(score) {
  if (score >= 90) {
    print("Grade: A")
  } else if (score >= 75) {
    print("Grade: B")
  } else if (score >= 50) {
    print("Grade: C")
  } else {
    print("Grade: F")
  }
}
grade_result(95)
grade_result(80)
grade_result(60)
grade_result(40)

#Exercise 7
data <- read.csv("/Users/abulbasharsaurov/Downloads/students.csv")
head(data, 5)
str(data)






