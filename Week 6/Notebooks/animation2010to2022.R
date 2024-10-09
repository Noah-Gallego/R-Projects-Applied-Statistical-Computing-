#Now lets animate for all the years from 2010 to 2022


# Load necessary packages
library(dplyr)      # For data manipulation
library(tidyr)      # For data reshaping
library(ggplot2)    # For plotting
library(gganimate)  # For animation
library(maps)       # For map data
library(transformr) # For animating polygons


# Load necessary packages
library(tidyverse)

# Generate a list of file names for the years 2010 to 2022
file_names <- paste0("daily_42101_", 2010:2022, ".csv")

# Read and combine datasets into one data frame
data_list <- lapply(file_names, read.csv)
data <- do.call("rbind", data_list)

# Select only the important variables
data <- select(data, Date.Local, State.Name, County.Name, Arithmetic.Mean)

# Group by Date.Local and State.Name to ensure one Arithmetic.Mean per unique combination
data <- data %>%
  group_by(Date.Local, State.Name) %>%
  summarize(Arithmetic.Mean = mean(Arithmetic.Mean, na.rm = TRUE))


# Convert Date.Local to Date format
data <- data %>%
  mutate(Date.Local = as.Date(Date.Local))

# Extract Year from Date.Local
data <- data %>%
  mutate(Year = format(Date.Local, "%Y"))


# Calculate the yearly mean Arithmetic.Mean for each state
state_year_mean <- data %>%
  group_by(State.Name, Year) %>%
  summarize(YearlyMean = mean(Arithmetic.Mean, na.rm = TRUE))

# Reshape data to have one row per state and columns for each year's mean
state_mean_wide <- state_year_mean %>%
  spread(key = Year, value = YearlyMean)

# Prepare map data
us_states <- map_data("state")

# Prepare the data for mapping
state_mean_wide <- state_mean_wide %>%
  mutate(region = tolower(State.Name))  # Convert State.Name to lowercase for matching

# Merge the map data with the yearly mean data using left_join()
map_data_all_years <- left_join(us_states, state_mean_wide, by = "region")


# Reshape the data from wide to long format using 'gather()' from 'tidyr'
map_data_long <- map_data_all_years %>%
  gather(key = "Year", value = "YearlyMean", `2010`:`2022`)

# Convert 'Year' column to a factor with levels from 2010 to 2022 to maintain chronological order
map_data_long <- map_data_long %>%
  mutate(Year = factor(Year, levels = as.character(2010:2022)))

# Create the animated map showing mean values from 2010 to 2022
animated_map <- ggplot(map_data_long, aes(x = long, y = lat, group = group, fill = YearlyMean)) +
  geom_polygon(color = "black") +                                       # Draw state boundaries
  scale_fill_gradient(name = "Mean", low = "white", high = "red") +     # Set color gradient
  theme_minimal() +                                                     # Apply a minimal theme
  labs(title = "Mean Across US States in {closest_state}", x = "", y = "") +  # Add dynamic title
  transition_states(Year, transition_length = 1, state_length = 1) +    # Animate over years
  ease_aes('linear')                                                    # Smooth transitions

animated_map

# Render and save the animation as a GIF
animation <- animate(animated_map, nframes = 13 * 5, fps = 5, width = 800, height = 600)
anim_save("mean_across_states.gif", animation = animation)

