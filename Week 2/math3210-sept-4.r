# 28th of August

#c(): Combines multiple elements into one atomic vector.
#length(): Returns the length (number of elements) of an object.
#class(): Returns the class of an object.
#typeof(): Returns the type of an object. 
#attributes(): Returns further metadata of arbitrary type.

# Checking class/type
#is.numeric()
#is.integer()
#is.logical()
#is.character()
#is.vector()

# Data types in R

# 1. Create a character vector with the following elements: "a", "b", "c".
char_vec <- c("a", "b", "c")
class(char_vec) # Check the type of the vector
is.character(char_vec)

# 2. Create a numeric vector with the following elements: 3, 5.7, 8.9.
num_vec <- c(3, 5.7, 8.9)
class(num_vec) # Check the type of the vector
is.numeric(num_vec)

# 3. Create an integer vector with the following elements: 2L, 4L, 6L.
int_vec <- c(2L, 4L, 6L)
class(int_vec) # Check the type of the vector
is.integer(int_vec) # Verify if the vector is of integer type

# 4. Create a logical vector with the following elements: TRUE, FALSE, TRUE.
log_vec <- c(TRUE, FALSE, TRUE)
class(log_vec) # Check the type of the vector
is.logical(log_vec) # Verify if the vector is of logical type

# 5. Create a complex vector with the following elements: 1+2i, 3+4i.
complex_vec <- c(1+2i, 3+4i)
class(complex_vec) # Check the type of the vector
is.complex(complex_vec) # Verify if the vector is of complex type


# if you are not sure, ask for help

help("class")
help("is.logical")
help("numeric")

# 6. Create and manipulate a list.
# Create a list containing a numeric vector, a character vector, and a logical vector.
my_list <- list(
  Numbers = c(1, 2, 3, 4, 5),
  Characters = c("A", "B", "C", "D", "E"),
  Logicals = c(TRUE, FALSE, TRUE, FALSE, TRUE)
)
print(my_list) # Display the list

# Access the second element of the "Numbers" vector in the list.
second_number <- my_list$Numbers[2]
print(second_number) # Display the accessed element

# Add a new element to the list containing a vector of complex numbers.
my_list$ComplexNumbers <- c(1+2i, 3+4i, 5+6i)
print(my_list) # Display the updated list

# 7. Create and manipulate a matrix.
# Create a 3x3 matrix with the numbers 1 to 9.
my_matrix <- matrix(1:9, nrow = 3, ncol = 3)
print(my_matrix) # Display the matrix

# Access the element in the 2nd row and 3rd column.
element_2_3 <- my_matrix[2, 3]
print(element_2_3) # Display the accessed element

# Calculate the column-wise means of the matrix.
col_means <- colMeans(my_matrix)
print(col_means) # Display the column-wise means

# 8. Create and manipulate an array.
# Create a 3x3x2 array with the numbers 1 to 18.
my_array <- array(1:18, dim = c(3, 3, 2))
print(my_array) # Display the array

# Access the element in the 3rd row, 2nd column of the 1st matrix.
element_3_2_1 <- my_array[3, 2, 1]
print(element_3_2_1) # Display the accessed element

# Calculate the sum of all elements in the array.
sum_array <- sum(my_array)
print(sum_array) # Display the sum of all elements

# 9. Create and manipulate a data frame.
# Create a data frame with three columns: ID (1 to 5), Name (A to E), and Age (random values).
df <- data.frame(
  ID = 1:5,
  Name = c("A", "B", "C", "D", "E"),
  Age = c(23, 34, 28, 45, 31)
)
print(df) # Display the data frame

# Add a new column to the data frame for Gender.
df$Gender <- c("M", "F", "M", "F", "M")
print(df) # Display the updated data frame

# Filter the data frame to include only rows where Age is greater than 30.
df_filtered <- df[df$Age > 30, ]
print(df_filtered) # Display the filtered data frame


#Missing values One additional important element is the missing values. 
#R knows two types of missing values: NaN and NA. 
#NaN is “not a number” and results from mathematical operations which are illegal/invalid. 
#NA on the other hand stands for ‘not available’ and indicates a missing value. NAs can occur in all the vectors discussed above, and keep their class.

# 10. Create a numeric vector with both NA and NaN values.
num_vector <- c(1, 2, NA, 4, NaN, 6)
print(num_vector) # Display the vector

# 2. Check for NA values in the vector using is.na().
na_check <- is.na(num_vector)
print(na_check) # Display TRUE where NA values are present

# 3. Check for NaN values in the vector using is.nan().
nan_check <- is.nan(num_vector)
print(nan_check) # Display TRUE where NaN values are present

# 4. Count the number of NA and NaN values in the vector.
na_count <- sum(is.na(num_vector))
nan_count <- sum(is.nan(num_vector))

print(paste("Number of NA values:", na_count))   # Display the number of NAs
print(paste("Number of NaN values:", nan_count)) # Display the number of NaNs

# 5. Replace NA values with the mean of the non-missing elements.
mean_value <- mean(num_vector, na.rm = TRUE)
num_vector[is.na(num_vector)] <- mean_value
print(num_vector) # Display the vector after replacing NA

# 6. Replace NaN values with zero.
num_vector[is.nan(num_vector)] <- 0
print(num_vector) # Display the vector after replacing NaN

# 11. sequences

# Numeric Sequences and Repetitions in R

# 1. Equidistant numeric sequence
# seq() function creates sequences. Here, we're creating a sequence from 1.5 to 2.5
# with 5 equally spaced numbers.
sequence1 <- seq(from = 1.5, to = 2.5, length.out = 5)
print(sequence1)  # Display the sequence

# 2. Decreasing sequence with a specified increment
# Here, we're creating a sequence from 4 to -4 with a decrement of 0.5.
sequence2 <- seq(from = 4, to = -4, by = -0.5)
print(sequence2)  # Display the sequence

# 3. Integer sequence using seq()
# This sequence is from 10 to 100 in steps of 10. Since all parameters are integers,
# the resulting sequence is of integer type.
int_sequence <- seq(from = 10L, to = 100L, by = 10L)
print(int_sequence)  # Display the integer sequence
print(class(int_sequence))  # Check the class of the sequence

# 4. Numeric sequence with seq()
# In this example, the 'from' value is numeric, so the resulting sequence is numeric.
num_sequence <- seq(from = 10, to = 100L, by = 10L)
print(num_sequence)  # Display the numeric sequence
print(class(num_sequence))  # Check the class of the sequence

# 5. Simple integer sequences using colon operator
# Colon operator creates a sequence from 1 to 4.
colon_sequence <- 1:4
print(colon_sequence)  # Display the sequence
print(class(colon_sequence))  # Check the class of the sequence

# 6. Decreasing integer sequence using colon operator
# Creates a sequence from 30 to 25 in steps of -1.
decreasing_sequence <- 30:25
print(decreasing_sequence)  # Display the sequence
print(class(decreasing_sequence))  # Check the class of the sequence

# 7. Integer sequence along an existing object
# seq_along() creates a sequence from 1 to the length of the vector 'cities'.
cities <- c("Vienna", "Paris", "Berlin", "Rome", "Bern")
seq_along_cities <- seq_along(cities)
print(seq_along_cities)  # Display the sequence

# 8. Simple integer sequence from 1 to n using seq_len()
# seq_len() creates a sequence from 1 to 10.
seq_len_example <- seq_len(10)
print(seq_len_example)  # Display the sequence

# 9. Character sequences using LETTERS and letters
# LETTERS and letters are pre-defined character vectors containing the alphabet in upper and lower case, respectively.

# First 7 letters of the alphabet in upper case
uppercase_letters <- LETTERS[1:7]
print(uppercase_letters)  # Display the first 7 uppercase letters

# First 7 letters of the alphabet in lower case
lowercase_letters <- letters[1:7]
print(lowercase_letters)  # Display the first 7 lowercase letters

# 10. Repeat elements using rep()
# Repeat the number 5, 10 times
repeat_num <- rep(5, times = 10)
print(repeat_num)  # Display repeated number

# Repeat a vector of numbers, 3 times
repeat_vector <- rep(1:3, times = 3)
print(repeat_vector)  # Display repeated vector

# Repeat each element of the vector 3 times
repeat_each <- rep(1:3, each = 3)
print(repeat_each)  # Display the vector with repeated elements


# Replicating Elements in R

# 1. Replicating a specific value multiple times
# This is the simplest case where we replicate the value 2L five times.
rep_value <- rep(2L, 5)
print(rep_value)  # Display the replicated value

# 2. Replicating each element of a vector multiple times
# Here we have a character vector 'cities'. We will replicate each city name three times.
cities <- c("Vienna", "Bern", "Rome")
rep_each <- rep(cities, each = 3)
print(rep_each)  # Display the vector with each element replicated three times

# 3. Replicating an entire vector multiple times
# In this case, we replicate the entire 'cities' vector three times.
rep_vector <- rep(cities, times = 3)
print(rep_vector)  # Display the vector replicated three times

# 4. Replicating elements of a vector with different frequencies
# Here, each element in 'cities' is replicated a different number of times.
rep_diff <- rep(cities, times = c(3, 2, 5))
print(rep_diff)  # Display the vector with elements replicated according to the specified times

# 5. Using rep.int() for efficient replication
# rep.int() is a faster version of rep() but has fewer features. Here we replicate the value 4 three times.
rep_int_example <- rep.int(4, 3)
print(rep_int_example)  # Display the result of rep.int()

# 6. Replicating elements until a specific length is reached using rep_len()
# Replicating elements of the vector c(4, 5, 6) until the vector has a length of 5.
rep_len_5 <- rep_len(c(4, 5, 6), length.out = 5)
print(rep_len_5)  # Display the vector with length 5

# Replicating elements of the vector c(4, 5, 6) until the vector has a length of 9.
rep_len_9 <- rep_len(c(4, 5, 6), length.out = 9)
print(rep_len_9)  # Display the vector with length 9

# Subsetting Matrices, Arrays, and Data Frames in R

# 1. Subsetting a Matrix
# Create a 3x3 matrix with values from 1 to 9.
my_matrix <- matrix(1:9, nrow = 3, ncol = 3)
print(my_matrix)  # Display the matrix

# Accessing a single element: 2nd row, 3rd column
element_2_3 <- my_matrix[2, 3]
print(element_2_3)  # Display the accessed element

# Accessing an entire row: 1st row
row_1 <- my_matrix[1, ]
print(row_1)  # Display the first row

# Accessing an entire column: 2nd column
col_2 <- my_matrix[, 2]
print(col_2)  # Display the second column

# Accessing a sub-matrix: first two rows and first two columns
sub_matrix <- my_matrix[1:2, 1:2]
print(sub_matrix)  # Display the sub-matrix


# 3. Subsetting a Data Frame
# Create a data frame with sample data.
df <- data.frame(
  ID = 1:5,
  Name = c("Alice", "Bob", "Charlie", "David", "Eva"),
  Age = c(23, 35, 45, 28, 30),
  Gender = c("F", "M", "M", "M", "F")
)
print(df)  # Display the data frame

# Accessing a single column: 'Name' column
name_column <- df$Name
print(name_column)  # Display the 'Name' column

# Accessing a single row: 3rd row
row_3 <- df[3, ]
print(row_3)  # Display the third row

# Accessing multiple rows and columns: Rows 2 to 4, columns 'Name' and 'Age'
subset_df <- df[2:4, c("Name", "Age")]
print(subset_df)  # Display the subset of the data frame

# Accessing rows based on a condition: Age greater than 30
age_gt_30 <- df[df$Age > 30, ]
print(age_gt_30)  # Display rows where Age is greater than 30

# 4. Subsetting with Logical Vectors
# Access rows where 'Gender' is 'F'
gender_f <- df[df$Gender == "F", ]
print(gender_f)  # Display rows where Gender is 'F'

# Access elements in a matrix that are greater than 5
matrix_gt_5 <- my_matrix[my_matrix > 5]
print(matrix_gt_5)  # Display elements greater than 5

# August 30

# Introduction to R Functions

# Purpose of a Function:
# A function is a reusable block of code that performs a specific task. 
# Functions help to organize code, avoid repetition, and make it easier to debug and maintain.

# 1. Syntax of an R Function:
# The general syntax for creating a function in R is as follows:

# function_name <- function(arg1, arg2, ...) {
#   # Code to perform the task
#   return(output)
# }

# - function_name: The name of the function.
# - arg1, arg2, ... : Arguments (inputs) that the function accepts.
# - return(output): The value that the function returns after execution (optional).

# Example 1: Function to Add Two Numbers
add_two_numbers <- function(a, b) {
  sum <- a + b  # Add the two numbers
  return(sum)   # Return the result
}

# Usage of the function:
result <- add_two_numbers(5, 7)
print(result)  # Output: 12


# Introduction to Conditional Statements in R

# Conditional statements allow you to execute different blocks of code based on certain conditions.
# In R, the most common conditional statements are `if`, `else`, and `nested if else`.

# 1. Syntax for `if`, `else`, and `nested if else`:

# Basic `if` statement:
# if (condition) {
#   # Code to execute if the condition is TRUE
# }

# `if else` statement:
# if (condition) {
#   # Code to execute if the condition is TRUE
# } else {
#   # Code to execute if the condition is FALSE
# }

# Nested `if else` statement:
# if (condition1) {
#   # Code to execute if condition1 is TRUE
# } else if (condition2) {
#   # Code to execute if condition1 is FALSE and condition2 is TRUE
# } else {
#   # Code to execute if both condition1 and condition2 are FALSE
# }

# 2. Examples:

# Example 1: Checking if a Number is Positive, Negative, or Zero
check_number <- function(x) {
  if (x > 0) {
    return("Positive")
  } else if (x < 0) {
    return("Negative")
  } else {
    return("Zero")
  }
}

# Usage of the function:
result <- check_number(5)
print(result)  # Output: "Positive"

result <- check_number(-3)
print(result)  # Output: "Negative"

result <- check_number(0)
print(result)  # Output: "Zero"

# Example 2: Determining the Grade Based on a Score
determine_grade <- function(score) {
  if (score >= 90) {
    return("A")
  } else if (score >= 80) {
    return("B")
  } else if (score >= 70) {
    return("C")
  } else if (score >= 60) {
    return("D")
  } else {
    return("F")
  }
}

# Usage of the function:
grade <- determine_grade(85)
print(grade)  # Output: "B"

grade <- determine_grade(72)
print(grade)  # Output: "C"

grade <- determine_grade(50)
print(grade)  # Output: "F"

# Example 3: Custom Function to Categorize Age Groups
categorize_age <- function(age) {
  if (age < 13) {
    return("Child")
  } else if (age >= 13 && age < 20) {
    return("Teenager")
  } else if (age >= 20 && age < 65) {
    return("Adult")
  } else {
    return("Senior")
  }
}

# Usage of the function:
age_category <- categorize_age(10)
print(age_category)  # Output: "Child"

age_category <- categorize_age(17)
print(age_category)  # Output: "Teenager"

age_category <- categorize_age(30)
print(age_category)  # Output: "Adult"

age_category <- categorize_age(70)
print(age_category)  # Output: "Senior"

# This script introduces the basic structure of conditional statements in R.
# It includes various examples to help students understand how to apply these statements in different scenarios.


# R code to  sum of all even numbers between 1 and 50 using a for loop and an if statement. 

# Initialize sum variable
sum_even_numbers <- 0

# Loop through numbers from 1 to 50
for (i in 1:50) {
  # Check if the number is even
  if (i %% 2 == 0) {
    # Add the even number to the sum
    sum_even_numbers <- sum_even_numbers + i
  }
}

# Print the result
print(sum_even_numbers)  # Output should be 650

# Extend for sum odd numbers

# Initialize sum variables
sum_even_numbers <- 0
sum_odd_numbers <- 0

# Loop through numbers from 1 to 50
for (i in 1:50) {
  # Check if the number is even
  if (i %% 2 == 0) {
    sum_even_numbers <- sum_even_numbers + i
  } else {
    # If the number is odd
    sum_odd_numbers <- sum_odd_numbers + i
  }
}

# Print the results
print(paste("Sum of even numbers:", sum_even_numbers))  # Output: 650
print(paste("Sum of odd numbers:", sum_odd_numbers))    # Output: 625


# Extend for sum odd numbers and even number for any value not just 50. function???

# Function to calculate the sum of even and odd numbers in a given range
sum_even_odd_numbers <- function(start_value, end_value) {
  # Initialize sum variables
  sum_even_numbers <- 0
  sum_odd_numbers <- 0
  
  # Loop through numbers from start_value to end_value
  for (i in start_value:end_value) {
    if (i %% 2 == 0) {
      sum_even_numbers <- sum_even_numbers + i
    } else {
      sum_odd_numbers <- sum_odd_numbers + i
    }
  }
  
  # Return the results as a list
  return(list(sum_even = sum_even_numbers, sum_odd = sum_odd_numbers))
}

# Usage of the function:
result <- sum_even_odd_numbers(1, 50)
print(paste("Sum of even numbers:", result$sum_even))  # Output: 650
print(paste("Sum of odd numbers:", result$sum_odd))    # Output: 625

# Example with a different range:
result <- sum_even_odd_numbers(1, 100)
print(paste("Sum of even numbers:", result$sum_even))  # Output: 2550
print(paste("Sum of odd numbers:", result$sum_odd))    # Output: 2500


# 4th of September

# R Script to Teach Loops and Control Statements

# ---------------------------------------
# For Loops
# ---------------------------------------
# Syntax of for loop:
# for (variable in sequence) {
#   statements
# }
# Description:
# A for loop is used to iterate over a sequence (like a vector or a list).
# It executes a block of code repeatedly for each element in the sequence.

# Example of a for loop:
print("For Loop Example:")
for (i in 1:5) {
  print(paste("Iteration", i))
}

# Summation Example:
numbers <- c(1, 2, 3, 4, 5)  # Create a vector of numbers
sum <- 0  # Initialize sum
for (i in numbers) {
  sum <- sum + i
}
print(paste("Total Sum:", sum))


# ---------------------------------------
# While Loops
# ---------------------------------------
# Syntax of while loop:
# while (condition) {
#   statements
# }
# Description:
# A while loop continues to execute as long as the specified condition remains true.
# It's useful when the number of iterations is not known before the loop starts.

# Example of a while loop:
print("While Loop Example:")
counter <- 1
while (counter <= 5) {
  print(paste("Count at", counter))
  counter <- counter + 1
}


# Factorial Calculation Example:
number <- 5  # Change this to compute factorial of any number
factorial <- 1
i <- 1
while (i <= number) {
  factorial <- factorial * i
  i <- i + 1
}
print(paste("Factorial of", number, "is:", factorial))



# ---------------------------------------
# Break Statement
# ---------------------------------------
# Syntax of break statement:
# if (condition) {
#   break
# }
# Description:
# The break statement is used to exit a loop immediately, regardless of the loop condition.
# It is typically used to stop the execution of the loop when a specific condition is met.

# Example of using break in a loop:
print("Break Statement Example:")
for (i in 1:10) {
  if (i == 6) {
    break
  }
  print(paste("Iteration", i))
}

# R script to find and print the first even and the first odd number in the range 1 to 10

# Find the first even number
for (i in 1:10) {
  if (i %% 2 == 0) {  # Checks if the number is even
    print(paste("The first even number is:", i))
    break  # Exits the loop after finding the first even number
  }
}

# Find the first odd number
for (i in 1:10) {
  if (i %% 2 != 0) {  # Checks if the number is odd
    print(paste("The first odd number is:", i))
    break  # Exits the loop after finding the first odd number
  }
}

# ---------------------------------------
# Next Statement
# ---------------------------------------
# Syntax of next statement:
# if (condition) {
#   next
# }
# Description:
# The next statement skips the remaining code in the current iteration and moves to the next iteration of the loop.
# It's useful for skipping certain elements in a loop based on a condition.

# Example of using next in a loop:
print("Next Statement Example:")
for (i in 1:10) {
  if (i %% 2 == 0) { # Skip even numbers
    next
  }
  print(paste("Iteration", i))
}



