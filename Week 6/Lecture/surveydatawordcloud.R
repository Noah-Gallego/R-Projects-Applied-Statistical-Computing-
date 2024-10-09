# lapply(): The lapply() function is used to apply a function to each 
#element of a list or vector. 
#It returns a list of the same length as the input, 
#where each element is the result of applying the function 
#to the corresponding list element.



#rbind(): This function is used to combine data frames, 
#matrices, or vectors by row. 
#It is useful when you have multiple datasets that share the 
#same structure and you want to stack them on top of each other.


#do.call(): do.call() is a versatile function that allows you to 
#call a function with a list of arguments. 
#It is especially useful when you want to apply a function to a
#list of objects and need flexibility in the function inputs.

sales_list <- list(
  store_A = c(150, 200, 250, 300),
  store_B = c(100, 130, 170, 210),
  store_C = c(90, 110, 140, 160)
)


# Use the lapply() function to calculate the sum 
#of sales for each store in sales_list.

total_sales <- lapply(sales_list, sum)
print(total_sales)


df1 <- data.frame(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(25, 30, 22),
  City = c("New York", "Los Angeles", "Chicago")
)

df2 <- data.frame(
  Name = c("David", "Eva", "Frank"),
  Age = c(28, 26, 32),
  City = c("San Francisco", "Boston", "Miami")
)

combined_data <- rbind(df1, df2)
print(combined_data)


sales_matrices <- list(
  electronics = matrix(c(100, 200, 150, 250), nrow = 2, byrow = TRUE),
  furniture = matrix(c(80, 120, 110, 160), nrow = 2, byrow = TRUE),
  clothing = matrix(c(50, 90, 70, 110), nrow = 2, byrow = TRUE)
)

# Combine sales matrices for different product categories.
combined_sales_matrix <- do.call(rbind, sales_matrices)
print(combined_sales_matrix)



library(dplyr)
library(tidyr)
library(mosaic)
library(ggplot2)

data <- read.csv("../Data/Updated_College_Students_Survey_Data.csv", stringsAsFactors = FALSE)

# Inspect the data structure
str(data)

# Check for missing values
sum(is.na(data))

#Age 
# Summary statistics for Age
summary(data$Age)

mean(~Age,data = data )
median(~Age,data = data )
sd(~Age,data = data )
var(~Age,data = data )

# Histogram fro age
ggplot(data, aes(x = Age)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +  # Add a density curve
  theme_minimal() +
  labs(title = "Age Distribution of Respondents", x = "Age", y = "Density") +
  theme(plot.title = element_text(hjust = 0.5))


# Gender 

# Frequency table for Gender
gender_counts <- data %>%
  group_by(Gender) %>%
  summarise(Count = n())

print(gender_counts)


# Bar chart for Gender distribution
ggplot(gender_counts, aes(x = Gender, y = Count, fill = Gender)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Gender Distribution", x = "Gender", y = "Number of Students") +
  theme(legend.position = "none")


# Major

# Frequency table for Major
major_counts <- data %>%
  group_by(Major) %>%
  summarise(Count = n())

print(major_counts)

# Bar chart for Major distribution
ggplot(major_counts, aes(x = Major, y = Count, fill = Major)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribution of Majors", x = "Major", y = "Number of Students") +
  theme(legend.position = "none")


# Pie chart for Major distribution
ggplot(major_counts, aes(x = "", y = Count, fill = Major)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_minimal() +
  labs(title = "Distribution of Majors") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank())




# List of categorical variables
categorical_vars <- c("Gender", "Major", "Year_of_Study", "Study_Hours_per_Week",
                      "Preferred_Study_Time", "Outings_per_Week", 
                      "Free_Time_Activity", "Career_Preparedness", "Life_Description")

# Function to create frequency tables and bar plots
create_freq_plot <- function(var) {
  freq_table <- data %>%
    group_by(.data[[var]]) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count))
  
  print(freq_table)
  
  ggplot(freq_table, aes(x = reorder(.data[[var]], -Count), y = Count, fill = .data[[var]])) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(title = paste("Distribution of", var), x = var, y = "Number of Students") +
    theme(legend.position = "none") +
    coord_flip()
}

# Apply the function to all categorical variables
for (var in categorical_vars) {
  print(create_freq_plot(var))
}


# Age vs. Career Preparedness

gf_boxplot(~Age|Career_Preparedness, data = data)

# Box Plot of Age by Career Preparedness
ggplot(data, aes(x = Career_Preparedness, y = Age, fill = Career_Preparedness)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Age Distribution by Career Preparedness", x = "Career Preparedness", y = "Age") +
  theme(legend.position = "none")

#Gender vs. Career Preparedness
# Contingency Table
gender_career <- table(data$Gender, data$Career_Preparedness)
print(gender_career)

# Install and load plotly
install.packages("plotly")
library(plotly)

# Interactive Bar Chart for Major Distribution
major_counts <- data %>%
  group_by(Major) %>%
  summarise(Count = n())

plot_ly(major_counts, x = ~Major, y = ~Count, type = 'bar', 
        marker = list(color = 'rgba(55, 128, 191, 0.7)',
                      line = list(color = 'rgba(55, 128, 191, 1.0)', width = 2))) %>%
  layout(title = "Interactive Distribution of Majors",
         xaxis = list(title = "Major"),
         yaxis = list(title = "Number of Students"))

# Mosaic Plot
mosaicplot(~ Major + Career_Preparedness, data = data, 
           main = "Mosaic Plot of Major vs. Career Preparedness",
           color = TRUE)


# Cleaning up text and creating a word cloud

# Install the required packages
install.packages(c("tm", "wordcloud", "SnowballC"))

# Load the libraries
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)

# Load required libraries
library(tm)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)

# Load the text file as a character vector
file_path <- "sample_speech.txt"  # Replace with your actual file path
text_data <- readLines(file_path)

# Step 1: Convert the character vector to a VCorpus
mooncloud <- VCorpus(VectorSource(text_data))

# Step 2: Clean the text
# Remove extra whitespace
mooncloud <- tm_map(mooncloud, stripWhitespace)

# Convert to lowercase (using content_transformer to avoid issues with tolower)
mooncloud <- tm_map(mooncloud, content_transformer(tolower))

# Remove common stopwords
mooncloud <- tm_map(mooncloud, removeWords, stopwords("english"))

# Stem the words
mooncloud <- tm_map(mooncloud, stemDocument)

# Remove punctuation
mooncloud <- tm_map(mooncloud, removePunctuation)

# Step 3: Create a term-document matrix
dtm <- TermDocumentMatrix(mooncloud)

# Convert the term-document matrix into a matrix
matrix <- as.matrix(dtm)

# Sum the occurrences of each word
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)

# Create a data frame with words and their frequencies
df <- data.frame(word = names(word_freqs), freq = word_freqs)

# Step 4: Generate the word cloud using the frequency data
wordcloud(words = df$word, freq = df$freq, 
          scale = c(5, 0.5), 
          max.words = 100, 
          random.order = FALSE, 
          rot.per = 0.35, 
          use.r.layout = FALSE, 
          colors = brewer.pal(8, "Dark2"))


# Assume you repeat for the survey data now

# Extract your text responses column
text_responses <- data$Life_Description

# Step 1: Convert the character vector to a VCorpus
mooncloud <- VCorpus(VectorSource(text_responses))

# Step 2: Clean the text
# Remove extra whitespace
mooncloud <- tm_map(mooncloud, stripWhitespace)

# Convert to lowercase (using content_transformer to avoid issues with tolower)
mooncloud <- tm_map(mooncloud, content_transformer(tolower))

# Remove common stopwords
mooncloud <- tm_map(mooncloud, removeWords, stopwords("english"))

# Stem the words
mooncloud <- tm_map(mooncloud, stemDocument)

# Remove punctuation
mooncloud <- tm_map(mooncloud, removePunctuation)

# Step 3: Create a term-document matrix
dtm <- TermDocumentMatrix(mooncloud)

# Convert the term-document matrix into a matrix
matrix <- as.matrix(dtm)

# Sum the occurrences of each word
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)

# Create a data frame with words and their frequencies
df <- data.frame(word = names(word_freqs), freq = word_freqs)

# Step 4: Generate the word cloud using the frequency data
wordcloud(words = df$word, freq = df$freq, 
          scale = c(5, 0.5), 
          max.words = 100, 
          random.order = FALSE, 
          rot.per = 0.35, 
          use.r.layout = FALSE, 
          colors = brewer.pal(8, "Dark2"))


