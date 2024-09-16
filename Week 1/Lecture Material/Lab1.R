# Step One
city_names = c("Vienna", "Paris", "Berlin")
class(city_names)
is.character(city_names)

# Step Two
avg_bookings = c(125.5, 98.7, 145.9)
class(avg_bookings)
is.numeric(avg_bookings)

# Step Three
successful_tours = c(10L, 20L, 30L)
class(successful_tours)
is.integer(successful_tours)

# Step Four
is_available = c(TRUE, FALSE, TRUE)
class(is_available)
is.logical(is_available)

# Step Five
currency_exchange = c(1+2i, 3+4i)
class(currency_exchange)
is.complex(currency_exchange)

# Step Six
tour_data = list(
  ID = c(101, 102, 103),
  guide_names = c("Alice", "Bob", "Charlie"),
  active = c(TRUE, FALSE, TRUE)
)

print(tour_data$ID[2])
tour_data$total_earnings = c(1000, 1001, 1002)

# Step Seven
ratings = c(4, 3, 5, 5, 4, 4, 3, 5, 4)
city_ratings = matrix(ratings, nrow = 3, ncol = 3, byrow = TRUE)
colnames(city_ratings) = tour_data$guide_names
rownames(city_ratings) = city_names
print(city_ratings)

# Data Frames
df <- data.frame(
  ID = 1:5,
  tour_guides = c("Alice", "Bob", "Charlie", "David", "Eva"),
  age = c(23, 35, 28, 45, 30)
)

df$gender <- c("Female", "Male", "Male", "Male", "Female")

# Filter the DF to show only the guides older than 30
df_filtered <- df[df$age > 30, ]

# Handling Missing Values
missing_vector = c(100, NA, 85, 90, NaN)
is.na(missing_vector)
print(paste("Before:", missing_vector))
mean_value = mean(missing_vector, na.rm = TRUE)
missing_vector[is.na(missing_vector)] = mean_value
print(paste("After: ", missing_vector))

# Sequences and Repetitions Section
dates = seq(1, 5, by = 1)
print(dates)

temps = seq(20.5, 25.5, length.out = 5)
print(temps)

target_sales = seq(1000L, 5000L, by = 1000L)
print(target_sales)

feedback_scores = seq(10, 50, by = 10)
print(feedback_scores)

num_bookings = seq(8, 0, by = -1)
print(num_bookings)

simulate_num = rep(5, times = 10)
print(simulate_num)

booking_numbers = c(3, 6, 9)
repeated_booking_numbers = rep(booking_numbers, times = 3)
print(repeated_booking_numbers)

c_sat = c("High", "Medium", "Low")
rep_c_sat = rep(c_sat, each = 3)
print(rep_c_sat)

strategies = c(rep("Strategy A", 4), rep("Strategy B", 2), rep("Strategy C", 6))
print(strategies)

customer_ids = rep_len(c(1, 2, 3), length.out = 10)
print(customer_ids)

# Sub-setting Matrices, and Data Frames Section
print(city_ratings[1, 2])
print(city_ratings[2,])
print(city_ratings[, 1:2])
print(city_ratings[2:3, ])

# Data Frame Operations
print(df$tour_guides)
print(df[3, ])
print(df)
print(df[2:4,c("tour_guides", "age")])
print(df[df$gender == "Female", ])
print(df$tour_guides[df$age > 30])