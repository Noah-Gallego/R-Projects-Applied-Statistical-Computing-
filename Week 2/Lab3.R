# Generate Two Random Vectors 
set.seed(50)
sensor_reading = sample(0:100, 1440, replace = TRUE)
day_phases = rep(c("Morning", "Afternoon", "Evening", "Night"), each = 360)

# Store into DataFrame 
df = data.frame(sensor_reading, day_phases)
df
# Problem 1
i = 1
high_reading = 0
while(i < length(sensor_reading)) {
  if (sensor_reading[i] > 80) {
    high_reading = sensor_reading[i]
    break;
  }
  i = i + 1
}

print(paste("The first high sensor reading is:", high_reading, ". At position: ", i))

# Problem 2 -- NEED CLARIFICATION
count = 0
for (i in 2:length(sensor_reading)) {
  if (sensor_reading[i - 1] < 20 & sensor_reading[i] > 50) {
    count = count + 1
  }
}
print(paste("The number of readings that are less than 20 and have a value greater than 50 following it is:", count))

# Problem 3
num_readings = 0
sum = 0
for (i in 1:length(sensor_reading)) {
  if (sensor_reading[i] >= 20 & sensor_reading[i] <= 50) {
    num_readings = num_readings + 1
    sum = sensor_reading[i] + sum
  }
}

average = sum / num_readings
print(paste("The average of values above 20 and below 50 are:", average))

# Problem 4
i = 1
while(i < length(day_phases)) {
  if (day_phases[i] != "Night") {
    print(day_phases[i])
  } else {
    print(paste("Night has begun at position", i, ". 🌙"))
    break
  }
  i = i + 1
}

