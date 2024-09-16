# Load Libraries
library(dplyr)
library(ggplot2)

# Read-In Data
df = read.csv("clean_data.csv")
head(df)

# Fetch Quantile 1-3 & Calculate IQR
q1 = quantile(df$median.income, 0.25)
q3 = quantile(df$median.income, 0.75)
iqr_data = IQR(df$median.income)

# Drop Irrelevant Cols
df = df %>% select(-poverty_class)
df = df %>% select(-below.povery.level)
df = df %>% select(-income_class)

# Create new categorical variable: income.classes based on median.income
df$income.classes = cut(df$median.income,
    breaks = c(-Inf, q1, q3, Inf),
    labels = c("Poor", "Middle", "Rich"))

head(df$income.classes)

# Create Frequency Table for income.classes
ggplot(df, aes(x = income.classes)) +
  geom_bar(fill = "skyblue", color = "blue") +
  labs(title = "Income Classes Frequencies", x = "Income Classes", y = "Frequencies") +
  theme_minimal()

head(df)

# Get Average Pollutant Data for Each Income Class
ozone_avg = aggregate(df$Ozone, by = list(df$income.classes), FUN = mean)
pm_avg = aggregate(df$Pm2.5, by = list(df$income.classes), FUN = mean)
co_avg = aggregate(df$CO, by = list(df$income.classes), FUN = mean)
no2_avg = aggregate(df$NO2, by = list(df$income.classes), FUN = mean)

ozone_avg
pm_avg
co_avg
no2_avg