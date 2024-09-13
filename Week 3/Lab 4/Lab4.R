# Import Packages
library(plotrix)
library(readxl)
library(dplyr)
library(tidyr)
library(VennDiagram)

# Read-In Excel File
df = read_xlsx("survey.xlsx")

# Convert All Text in df to lowercase
for (i in colnames(df)) {
  df[[i]] = tolower(df[[i]])
}

# Split expectation column into two separate cols
df = separate(df, expectation, into = c("expectation_1", "expectation_2"), sep = ",")

# Remove blank spaces and na
df$expectation_2 = trimws(df$expectation_2)
df = mutate(df, mutate(across(everything(), ~ na_if(., "na"))))

# Create a Frequency Table for motivation
plot_freq = function(plotted_df, title, x, y = "Frequency") {
  barplot(plotted_df,
          main = title,
          xlab = x,
          ylab = y,
          col = "lightblue"
  )
}

motivation_table = table(df$motivation)
plot_freq(motivation_table,
          "Motivation for Taking the Class",
          "Motivation"
          )

# Create a Frequency Table for Knowledge Levels
k_table = table(df$r_level)
plot_freq(k_table,
          "Self-Assessed Knowledge Prior to Class",
          "Knowledge Levels"
)

# Create Frequency Table for both Expectations
df$expectation_1 = gsub("_", " ", df$expectation_1, fixed=TRUE)
e_1 = table(df$expectation_1)
e_2 = table(df$expectation_2)
data = rbind(e_1, e_2)

barplot(data,
        beside = TRUE,
        main = "Expectations for Taking the Class",
        xlab = "Expectation",
        ylab = "Frequency",
        col = c("lightblue", "lightgreen"),
        names.arg = c("Data Analyst", "R Coder"),
        legend.text = c("Data Analyst", "R Coder")
)

data_analyst_students = !is.na(df$expectation_1) & df$expectation_1 == "data analyst"
r_coder_students = !is.na(df$expectation_2) & df$expectation_2 == "r_coder"

venn.plot = venn.diagram(
  x = list(
    "Data Analyst" = which(data_analyst_students),
    "R Coder" = which(r_coder_students)
  ),
  category.names = c("Data Analyst", "R Coder"),
  fill = c("blue", "green"),
  alpha = 0.5,
  filename = NULL
)
df
grid.draw(venn.plot)

# Number 9 --> Displayed in Number 6

# Display Counts of R_Levels as a Pie Chart
pie3D(k_table,
      labels=unique(df$r_level),
      explode=0.5,
      main="Knowledge Levels Prior to Start Date",
      labelcex = 1.5
)

# Cross-Tabulation
cross_tab <- table(df$r_level, df$motivation)
print(cross_tab)

# Generate a Stacked-Bar-Chart to visualize the cross-tabulation between R Level and Motivation
barplot(cross_tab, 
        main = "Stacked Bar Chart of R Level and Motivation", 
        col = c("red", "green", "blue"), 
        legend = rownames(cross_tab), 
        xlab = "Motivation", 
        ylab = "Frequency"
)

# Number 13 --> Showed in Number 5

# Calculate the proportion of each motivation value and show in a pie chart
pie3D(motivation_table,
      labels=unique(df$motivation),
      explode=0.5,
      main="Reasons of Motivation For Taking Class",
      labelcex = 1.5
)

# Number 15 --> showed in Number 12

