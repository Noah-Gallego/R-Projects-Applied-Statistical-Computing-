# Load the necessary libraries
library(ggplot2)    # For advanced plotting
library(dplyr)      # For data manipulation
library(ggfortify)  # For autoplotting PCA results
library(FactoMineR)  # For PCA
library(factoextra)  # For visualization
library(plotly) # For 3D interactive plots

# 1. Load and Explore the Iris Dataset
data(iris)  # Load the iris dataset
head(iris)  # View the first few rows of the dataset
str(iris)   # Check the structure of the dataset
summary(iris)  # Get summary statistics

# The dataset contains 150 observations of 5 variables:
# - Sepal.Length (numeric)
# - Sepal.Width (numeric)
# - Petal.Length (numeric)
# - Petal.Width (numeric)
# - Species (factor)

# 2. Standardize the Numerical Variables
# Extract the numerical features
iris_numeric <- iris %>% select(-Species)

# Standardize the numerical features
iris_scaled <- scale(iris_numeric)

# Check the means and standard deviations after scaling
colMeans(iris_scaled)  # Should be close to 0
apply(iris_scaled, 2, sd)  # Should be 1

# Standardization is necessary because PCA is sensitive to the scale of the variables.

# 3. Perform PCA on the Standardized Data
# Perform PCA
pca_result <- prcomp(iris_scaled)

# View the PCA result
summary(pca_result)

# The summary shows the standard deviation, proportion of variance,
# and cumulative proportion of variance explained by each principal component.

# 4. Interpret the Principal Components
# Examine the loadings (rotation) of the principal components
pca_result$rotation

# The loadings indicate the weight of each original variable in each principal component.

# 5. Create Diagnostic Plots
# Scree plot to visualize the variance explained
plot(pca_result, type = "l", main = "Scree Plot")

# Alternatively, using ggplot2 for a nicer scree plot
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
qplot(y = variance_explained, x = 1:length(variance_explained), geom = "line") +
  geom_point() +
  xlab("Principal Component") +
  ylab("Proportion of Variance Explained") +
  ggtitle("Scree Plot")

# Create a Scree Plot
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100))  # Scree plot showing variance explained by components



# Biplot of the first two principal components
biplot(pca_result, scale = 0)

# Interpretation of the biplot:
# - The x-axis represents the first principal component (PC1), which captures the largest amount of variance in the data.
# - The y-axis represents the second principal component (PC2), which captures the second-largest amount of variance.
# - Each point on the biplot represents an individual observation (iris flower) projected onto the PC1-PC2 space.
#   - Points close to each other are more similar in terms of their original measurements, while points far apart are dissimilar.
# - Arrows represent the original variables (Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) and their contributions to PC1 and PC2:
#   - The direction of an arrow indicates the contribution of that variable to the respective principal component.
#   - Longer arrows indicate stronger contributions to the principal components, while shorter arrows represent weaker contributions.
# - The angle between two arrows indicates the correlation between the original variables:
#   - Small angles (< 90 degrees) suggest that the variables are positively correlated.
#   - Angles close to 90 degrees suggest that the variables are uncorrelated.
#   - Angles greater than 90 degrees indicate negative correlation between variables.
# - In the iris dataset:
#   - Petal.Length and Petal.Width arrows are usually long and point in the direction of PC1, meaning they contribute significantly to PC1.
#   - Sepal.Width may point in a different direction, suggesting it has a different relationship with the principal components compared to the petal measurements.
# - Observations of Setosa species tend to cluster together and are separated from Versicolor and Virginica along PC1, 
#   indicating that petal measurements help in distinguishing Setosa from the other species.
# - Versicolor and Virginica might show overlap along PC1 but are somewhat separated along PC2, which is influenced by sepal measurements.

# 6. Visualize the Data in Principal Component Space
# Create a data frame with the principal components and species
pca_data <- data.frame(pca_result$x, Species = iris$Species)

# Plot the observations in the space of the first two principal components
ggplot(pca_data, aes(x = PC1, y = PC2, color = Species)) +
  geom_point(size = 2) +
  ggtitle("PCA of Iris Dataset") +
  xlab(paste0("PC1 (", round(variance_explained[1] * 100, 1), "% Variance)")) +
  ylab(paste0("PC2 (", round(variance_explained[2] * 100, 1), "% Variance)")) +
  theme_minimal()

# 7. Discuss the Results
# Interpretation:
# - PC1 has high positive loadings for petal length and petal width, and negative loadings for sepal width.
# - PC2 is dominated by sepal length and sepal width.
# - The scatter plot shows that Setosa species are well-separated from Versicolor and Virginica along PC1.
# - Versicolor and Virginica show some overlap but are somewhat separated along PC2.

# The PCA has effectively reduced the dimensionality of the dataset while retaining the ability to distinguish between species.


# 3d plot PC1 PC2 PC3

# 3. Extract the first three principal components
pca_data <- as.data.frame(pca_result$x[, 1:3])  # Get PC1, PC2, and PC3
pca_data$Species <- iris$Species  # Add the species back for coloring

# 4. Create a 3D scatter plot using plotly
plot_ly(data = pca_data, 
        x = ~PC1, y = ~PC2, z = ~PC3, 
        color = ~Species, 
        colors = c('red', 'green', 'blue')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')),
         title = '3D Visualization of PCA (Iris Dataset)')


#Outlier Detection

#Outliers are typically observations that are far from the center of the data in 
#the new principal component space. The center of the data is generally considered 
#to be the origin in PCA space because PCA centers the data by subtracting 
#the mean during standardization.



# Outlier Detection Explanation:

# 1. Calculate the distance of each point from the origin in PCA space.
#    The first two principal components (PC1 and PC2) capture the most variance in the data, 
#    so we measure how far each point is from the origin (which represents the center of the data after PCA).
#    The Euclidean distance is calculated as sqrt(PC1^2 + PC2^2) for each observation.

pca_distances <- sqrt(rowSums(pca_result$x[, 1:2]^2))

# 2. Set a threshold for outliers based on the 95th percentile of the distances.
#    We define outliers as observations that are farther from the origin than 95% of the data points.
#    This means any point with a distance greater than the 95th percentile of distances is flagged as an outlier.
#    The top 5% of points, based on distance, are considered outliers.

outlier_threshold <- quantile(pca_distances, 0.95)

# 3. Identify which observations are outliers.
#    A logical vector is created where 'TRUE' represents an outlier and 'FALSE' represents a non-outlier.

outliers <- pca_distances > outlier_threshold

# 4. Visualize the outliers in a 2D PCA plot.
#    We create a scatter plot of the first two principal components (PC1 and PC2), 
#    with points colored based on whether they are outliers.
#    Outliers will be highlighted in a different color to visualize how far they are from the main data cluster.

ggplot(iris_pca, aes(x = PC1, y = PC2, color = outliers)) +
  geom_point(size = 2) +
  labs(title = "Outliers Detected Using PCA")

