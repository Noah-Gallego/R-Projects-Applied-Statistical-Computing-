# Palmer Penguins: Statistical Analysis and Shiny App Development

## Overview

This project provides an interactive analysis and visualization tool for the Palmer Penguins dataset using R and Shiny. The application allows users to perform exploratory data analysis, add new columns, build predictive models, conduct principal component analysis (PCA), and apply K-means clustering on the dataset. The goal is to provide a hands-on approach to data analysis and statistical modeling with an easy-to-use interface.

## Features

- **Exploratory Data Analysis (EDA)**: Users can explore relationships between different features of the Palmer Penguins dataset using various plots such as scatter plots, box plots, and density plots.

- **Add New Columns**: An interactive feature to allow users to create new columns from existing data. For instance, users can calculate and add a BMI column.

- **Predictive Modeling**: Users can build predictive models using Random Forest to predict the value of a selected response variable based on other features. The model's accuracy and key visualizations are also provided.

- **Principal Component Analysis (PCA)**: PCA is available to visualize the main components that explain the variance in the dataset, with an interactive biplot to explore data patterns.

- **K-Means Clustering**: Users can perform K-means clustering on selected numerical features to identify clusters among the penguins, visualized with an appealing clustering plot.

## Getting Started

### Installation

1. Clone this repository to your local machine:
   ```sh
   git clone https://github.com/noahgallego/palmer_penguins_shiny_app.git
   ```

2. Install the required R packages:
   ```r
   install.packages(c("shiny", "shinythemes", "tidyverse", "palmerpenguins", "randomForest", "cluster", "caret", "factoextra", "gridExtra"))
   ```

3. Run the app:
   ```r
   library(shiny)
   runApp("path/to/palmer_penguins_shiny_app")
   ```

### Usage

Once the Shiny app is running, you can interact with the interface to:
- Explore the dataset using various visualization tools.
- Add new columns based on existing features.
- Build a predictive model to understand relationships within the data.
- Perform dimensionality reduction using PCA.
- Identify natural groupings among the penguins using K-means clustering.

## Project Structure

- **app.R**: The main Shiny app script that contains the UI and server components.
- **README.md**: Project documentation.
- **data/** (optional): Include any additional data files used for analysis if not using the `palmerpenguins` package.

## Deployment

The Shiny app has been deployed using [ShinyApps.io](https://www.shinyapps.io). You can view the live app here: [Live Shiny App](https://noahgallego.shinyapps.io/final_project/).


**Disclaimer**: This project is for educational purposes, exploring statistical analysis and visualization techniques using Shiny in R.

