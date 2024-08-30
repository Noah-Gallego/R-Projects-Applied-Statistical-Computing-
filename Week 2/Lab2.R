# Lab 2: Functions and Loops
# Question 1

# Even/Odd Number Sum (1 & 2)
odd_even_calc = function(choice, start, end) {
  sum = 0
  if (tolower(choice) == "even") {
    for (i in start:end) {
      if (i %% 2 == 0) {
        sum = sum + i
      }
    }
  } else {
    for (i in start:end) {
      if (i %% 2 != 0) {
        sum = sum + i
      }
    }
  }

  return(sum)
}

odd_even_calc("odd", 1, 50)
odd_even_calc("even", 1, 50)

# Third Step
generalized_sum = function(start_value, end_value) {
  odd = odd_even_calc("odd", start_value, end_value)
  even = odd_even_calc("even", start_value, end_value)
  sum = odd + even
  
  print(paste("The odd sum between the range (", start_value, ",", end_value, ") is", odd, "."))
  print(paste("The even sum between the range (", start_value, ",", end_value, ") is", even, "."))
  print(paste("The TOTAL sum between the range (", start_value, ",", end_value, ") is", sum, "."))
}

generalized_sum(1, 50)
generalized_sum(1, 100)