library(ggplot2)
library(factoextra)

wine <- read.table("../Data/wine.txt", sep = ",")

wine_data <- wine[, -1]  # Assuming the first column is the cultivar type

wine_scaled <- scale(wine_data)
pca_result <- prcomp(wine_scaled, center = TRUE, scale. = TRUE)

#Eigenvalues (Standard Deviations of Principal Components):
pca_result$sdev

#Variance Explained:
variance_explained <- (pca_result$sdev)^2 / sum(pca_result$sdev^2)
cumsum(variance_explained)  # Cumulative variance

#Scree Plot:
#Scree Plot: One of the most common methods to decide how many principal 
#components to retain is by looking at the scree plot. A scree plot shows 
#the proportion of total variance explained by each principal component. 
#The x-axis represents the principal component number, and the y-axis 
#shows the corresponding eigenvalue (or percentage of variance explained).

fviz_eig(pca_result)

# How to pick using the Scree Plot:
#  look for an "elbow" in the scree plot, 
# which is a point where the variance explained by each 
# subsequent component starts to flatten out.

# The rule of thumb is to retain components that are before the elbow, 
# as they contribute substantially to the total variance.


# Another common approach is the Kaiser criterion, where you retain components with eigenvalues greater than 1. However, this is usually 
# more applicable for exploratory factor analysis.


install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)  # For PCA
library(factoextra)  # For visualization


# Perform PCA
pca_result <- PCA(wine_scaled, graph = FALSE)  # Assuming my_data is your dataset

# Create a Scree Plot
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))  # Scree plot showing variance explained by components



