library(shiny)
library(shinythemes)
library(tidyverse)
library(palmerpenguins)
library(ggplot2)
library(randomForest)
library(cluster)
library(caret)
library(factoextra)
library(gridExtra)

rsconnect::setAccountInfo(name='noahgallego', token='5E310EBE3E76012755440E79B71FCE9D', secret='iRy5bffYyt3T6CxA8FEEKMQ7RwdFnFpNrHWEoxRu')

# Load and preprocess the data
data <- penguins

# Handle missing values
mode_function <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

data <- data %>%
  group_by(species) %>%
  mutate(
    bill_length_mm = ifelse(is.na(bill_length_mm), median(bill_length_mm, na.rm = TRUE), bill_length_mm),
    bill_depth_mm = ifelse(is.na(bill_depth_mm), median(bill_depth_mm, na.rm = TRUE), bill_depth_mm),
    flipper_length_mm = ifelse(is.na(flipper_length_mm), median(flipper_length_mm, na.rm = TRUE), flipper_length_mm),
    body_mass_g = ifelse(is.na(body_mass_g), median(body_mass_g, na.rm = TRUE), body_mass_g)
  ) %>%
  ungroup()

data$island[is.na(data$island)] <- mode_function(data$island)
data$sex[is.na(data$sex)] <- mode_function(data$sex)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(tags$style(HTML(".main-header { text-align: center; color: #004c99; } .btn-primary { background-color: #0073e6; border-color: #0073e6; } .panel-body { background-color: #f5f5f5; padding: 15px; border-radius: 10px; } .analysis-output { margin-top: 20px; }"))),
  titlePanel(tags$h1("Palmer Penguins: Statistical Analysis and Shiny App Development", class = "main-header")),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("analysis_type", "Select Analysis Type:",
                  choices = c("Exploratory Data Analysis", "Add New Column", "Build Predictive Model", "Principal Component Analysis (PCA)", "K-Means Clustering"),
                  selected = "Exploratory Data Analysis"),
      conditionalPanel(
        condition = "input.analysis_type == 'Exploratory Data Analysis'",
        selectInput("eda_plot_type", "Select Plot Type:",
                    choices = c("Bill Length vs. Flipper Length", "Body Mass Distribution by Species", "Bill Depth vs. Flipper Length", "Density Plot of Bill Length", "Density Plot of Flipper Length"))
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Add New Column'",
        textInput("new_column", "New Column Name:", "BMI"),
        actionButton("add_column", "Add Column", class = "btn btn-primary")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Build Predictive Model'",
        selectInput("response_var", "Select Response Variable:",
                    choices = names(data)[sapply(data, is.numeric)], selected = "body_mass_g"),
        selectInput("predictor_vars", "Select Predictor Variables:",
                    choices = names(data)[sapply(data, is.numeric)], multiple = TRUE, selected = c("bill_length_mm", "flipper_length_mm")),
        actionButton("build_model", "Build Model", class = "btn btn-primary")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'Principal Component Analysis (PCA)'",
        actionButton("run_pca", "Run PCA", class = "btn btn-primary")
      ),
      conditionalPanel(
        condition = "input.analysis_type == 'K-Means Clustering'",
        numericInput("num_clusters", "Number of Clusters:", value = 3, min = 1),
        actionButton("run_kmeans", "Run K-Means", class = "btn btn-primary")
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Data Table", tableOutput("data_table")),
        tabPanel("Analysis Output", 
                 div(class = "analysis-output",
                     plotOutput("analysis_plot", height = "500px"),
                     verbatimTextOutput("model_accuracy"),
                     plotOutput("model_plots", height = "500px")
                 )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  penguin_data <- reactiveVal(data)
  
  observeEvent(input$add_column, {
    updated_data <- penguin_data()
    if (input$new_column == "BMI") {
      updated_data <- updated_data %>%
        mutate(BMI = body_mass_g / (flipper_length_mm * 0.01)^2)
    }
    penguin_data(updated_data)
  })
  
  filtered_data <- reactive({
    penguin_data()
  })
  
  output$data_table <- renderTable({
    head(filtered_data())
  })
  
  output$analysis_plot <- renderPlot({
    req(input$analysis_type == "Exploratory Data Analysis")
    plot_type <- input$eda_plot_type
    df <- filtered_data()
    
    if (plot_type == "Bill Length vs. Flipper Length") {
      ggplot(df, aes(x = bill_length_mm, y = flipper_length_mm, color = species)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(title = "Bill Length vs. Flipper Length", x = "Bill Length (mm)", y = "Flipper Length (mm)") +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
    } else if (plot_type == "Body Mass Distribution by Species") {
      ggplot(df, aes(x = body_mass_g, fill = species)) +
        geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
        labs(title = "Body Mass Distribution by Species", x = "Body Mass (g)", y = "Count") +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
    } else if (plot_type == "Bill Depth vs. Flipper Length") {
      ggplot(df, aes(x = bill_depth_mm, y = flipper_length_mm, color = species)) +
        geom_point(size = 3, alpha = 0.7) +
        labs(title = "Bill Depth vs. Flipper Length", x = "Bill Depth (mm)", y = "Flipper Length (mm)") +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
    } else if (plot_type == "Density Plot of Bill Length") {
      ggplot(df, aes(x = bill_length_mm, fill = species)) +
        geom_density(alpha = 0.5) +
        labs(title = "Density Plot of Bill Length by Species", x = "Bill Length (mm)", y = "Density") +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
    } else if (plot_type == "Density Plot of Flipper Length") {
      ggplot(df, aes(x = flipper_length_mm, fill = species)) +
        geom_density(alpha = 0.5) +
        labs(title = "Density Plot of Flipper Length by Species", x = "Flipper Length (mm)", y = "Density") +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
    }
  })
  
  observeEvent(input$build_model, {
    response <- input$response_var
    predictors <- input$predictor_vars
    formula <- as.formula(paste(response, "~", paste(predictors, collapse = "+")))
    model <- tryCatch({
      randomForest(formula, data = filtered_data(), ntree = 100)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(model)) {
      output$model_accuracy <- renderPrint({
        cat("Model Accuracy:\n")
        # Predict on training data
        predictions <- predict(model, filtered_data())
        actual <- filtered_data()[[response]]
        
        # Check if the response variable is a factor for classification
        if (is.factor(actual)) {
          actual <- factor(actual, levels = unique(actual))
          predictions <- factor(predictions, levels = levels(actual))
          confusion <- confusionMatrix(predictions, actual)
          cat("Accuracy:", round(confusion$overall["Accuracy"], 3), "\n")
        } else {
          # For regression, calculate R-squared
          ss_total <- sum((actual - mean(actual))^2)
          ss_residual <- sum((actual - predictions)^2)
          r_squared <- 1 - (ss_residual / ss_total)
          cat("R-squared:", round(r_squared, 3), "\n")
        }
      })
      
      output$model_plots <- renderPlot({
        # Plot actual vs predicted values
        df <- filtered_data() %>% mutate(predicted = predict(model, filtered_data()))
        p1 <- ggplot(df, aes_string(x = response, y = "predicted")) +
          geom_point(alpha = 0.7, color = "#2c3e50") +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
          labs(title = "Actual vs Predicted", x = "Actual Values", y = "Predicted Values") +
          theme_minimal(base_size = 16) +
          theme(plot.title = element_text(hjust = 0.5))
        
        # Residual plot
        df <- df %>% mutate(residual = as.numeric(df[[response]]) - as.numeric(predicted))
        p2 <- ggplot(df, aes(x = predicted, y = residual)) +
          geom_point(alpha = 0.7, color = "#e74c3c") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
          labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals") +
          theme_minimal(base_size = 16) +
          theme(plot.title = element_text(hjust = 0.5))
        
        grid.arrange(p1, p2, ncol = 2)
      })
    } else {
      output$model_accuracy <- renderPrint({
        cat("Error: Unable to build model. Please check the selected response and predictor variables.")
      })
      output$model_plots <- renderPlot(NULL)
    }
  })
  
  observeEvent(input$run_pca, {
    df <- filtered_data() %>% select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>% na.omit()
    scaled_data <- scale(df)
    pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
    output$analysis_plot <- renderPlot({
      fviz_pca_biplot(pca_result, repel = TRUE, title = "PCA Biplot of Palmer Penguins Data") +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
    })
  })
  
  observeEvent(input$run_kmeans, {
    df <- filtered_data() %>% select(bill_length_mm, flipper_length_mm) %>% na.omit()
    scaled_data <- scale(df)
    kmeans_result <- kmeans(scaled_data, centers = input$num_clusters, nstart = 25)
    output$analysis_plot <- renderPlot({
      fviz_cluster(kmeans_result, data = scaled_data, geom = "point", ellipse.type = "convex", ggtheme = theme_minimal(), main = "K-Means Clustering of Palmer Penguins") +
        theme_minimal(base_size = 16) +
        theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
