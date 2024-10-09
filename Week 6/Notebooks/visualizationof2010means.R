# Load necessary packages
library(tidyverse)

# Generate a list of file names for the years 2010 to 2022
file_names <- paste0("daily_42101_", 2010:2022, ".csv")

# Read and combine datasets into one data frame

#lapply() Function:
#Apply a function over a list or vector and return a list of results.
#Takes each file name from file_names.
#Applies read.csv() to read the CSV file into a data frame.
#Collects all data frames into a list.

data_list <- lapply(file_names, read.csv)




#rbind() Function:
#Combine data frames or matrices by rows.

#do.call() Function
#Calls the rbind() function.
#Uses the elements of data_list as arguments to rbind().
#Effectively performs rbind(data_list[[1]], data_list[[2]], ..., data_list[[n]]).

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

# Rename columns to include "mean" in the year columns
colnames(state_mean_wide)[-1] <- paste0(colnames(state_mean_wide)[-1], "mean")


# Draw a map of US and first lets visualize mean values of 2010 across all states

# Load necessary packages
library(tidyverse)  # Includes ggplot2, dplyr, tidyr, etc.
library(maps)       # Provides the map_data() function
library(dplyr)

# Get US states map data
us_states <- map_data("state")

# Prepare the data for the year 2010
state_2010_mean <- state_mean_wide %>%
  select(State.Name, `2010mean`) %>%      # Select the necessary columns
  mutate(region = tolower(State.Name))    # Convert state names to lowercase to match map data

# Merge the map data with the 2010 mean data
map_data <- left_join(us_states, state_2010_mean, by = "region")

# Plot the US map showing the variation of 2010 mean across states
ggplot(map_data, aes(x = long, y = lat, group = group, fill = `2010mean`)) +
  geom_polygon(color = "black") +                                  # Draw state boundaries
  scale_fill_gradient(name = "2010 Mean", low = "white", high = "red") +  # Set color gradient
  theme_minimal() +                                                # Apply a minimal theme
  labs(title = "2010 Mean Across US States", x = "", y = "")       # Add title and remove axis labels


