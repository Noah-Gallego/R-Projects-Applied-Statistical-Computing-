# Load required packages
library(shiny)
library(ggplot2)
library(dplyr)
library(caret)
library(stats)
library(DT)
library(shinythemes)

# Define UI layout
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(tags$style(HTML(".navbar {background-color: #2c3e50; color: #ecf0f1;} .tab-content {background-color: #ffffff; padding: 20px; border-radius: 10px;} .well {background-color: #ecf0f1; border: none;}"))),
  
  titlePanel(title = div(img(src = 'https://www.r-project.org/logo/Rlogo.png', height = '50px', width = '50px'), "Interactive Data Analysis App"), windowTitle = "Interactive Data Analysis App"),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #f7f7f9; border-radius: 10px;",
      fileInput("file", "Upload CSV File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                buttonLabel = "Browse", placeholder = "No file selected"),
      uiOutput("variableUI"),
      uiOutput("numericVariablesUI"),
      uiOutput("responseVariableUI"),
      actionButton("regressionButton", "Run Linear Regression", class = "btn-primary btn-lg", icon = icon("chart-line")),
      actionButton("pcaButton", "Run PCA", class = "btn-success btn-lg", icon = icon("project-diagram"))
    ),
    
    mainPanel(
      tabsetPanel(
        type = "pills",
        tabPanel("Dataset", dataTableOutput("dataTable")),
        tabPanel("EDA", plotOutput("plotOutput", height = "400px"), tableOutput("tableOutput")),
        tabPanel("Linear Regression", verbatimTextOutput("regressionSummary"), plotOutput("regressionPlot", height = "500px")),
        tabPanel("PCA", plotOutput("pcaPlot", height = "500px"), tableOutput("pcaTable"))
      ),
      style = "background-color: #ffffff; border-radius: 10px; padding: 20px;"
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  dataset <- reactiveVal()
  
  observeEvent(input$file, {
    req(input$file)
    dataset(read.csv(input$file$datapath))
  })
  
  output$dataTable <- renderDataTable({
    req(dataset())
    datatable(dataset(), options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  output$variableUI <- renderUI({
    req(dataset())
    selectInput("variable", "Choose a variable for EDA:", choices = names(dataset()), selected = names(dataset())[1])
  })
  
  output$numericVariablesUI <- renderUI({
    req(dataset())
    selectInput("numericVars", "Choose predictor variables (for Regression & PCA):", choices = names(dataset()), multiple = TRUE)
  })
  
  output$responseVariableUI <- renderUI({
    req(dataset())
    selectInput("responseVar", "Choose response variable (for Regression):", choices = names(dataset()))
  })
  
  output$plotOutput <- renderPlot({
    req(dataset(), input$variable)
    var <- input$variable
    
    if (is.factor(dataset()[[var]]) || length(unique(dataset()[[var]])) < 10) {
      ggplot(dataset(), aes_string(x = var)) +
        geom_bar(fill = "#3498db", color = "#2c3e50") +
        labs(title = paste("Bar Chart of", var), x = var, y = "Frequency") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
    } else {
      ggplot(dataset(), aes_string(x = var)) +
        geom_histogram(fill = "#2ecc71", color = "#27ae60", bins = 10) +
        labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
    }
  })
  
  output$tableOutput <- renderTable({
    req(dataset(), input$variable)
    var <- input$variable
    
    if (is.factor(dataset()[[var]]) || length(unique(dataset()[[var]])) < 10) {
      as.data.frame(table(dataset()[[var]])) %>%
        setNames(c("Category", "Frequency"))
    } else {
      summary_data <- summary(dataset()[[var]])
      data.frame(Statistic = names(summary_data), Value = as.numeric(summary_data))
    }
  })
  
  observeEvent(input$regressionButton, {
    req(dataset(), input$numericVars, input$responseVar)
    numericVars <- input$numericVars
    responseVar <- input$responseVar
    
    formula <- as.formula(paste(responseVar, "~", paste(numericVars, collapse = "+")))
    model_data <- dataset()[, c(responseVar, numericVars), drop = FALSE]
    model_data <- na.omit(model_data)
    
    if (any(!sapply(model_data, is.numeric))) {
      showNotification("All selected variables must be numeric for regression.", type = "error")
      return()
    }
    
    model <- lm(formula, data = model_data)
    
    output$regressionSummary <- renderPrint({
      summary(model)
    })
    
    output$regressionPlot <- renderPlot({
      par(mfrow = c(2, 2))
      plot(model)
    })
  })
  
  observeEvent(input$pcaButton, {
    req(dataset(), input$numericVars)
    numericVars <- input$numericVars
    
    if (length(numericVars) < 2) {
      showNotification("Please select at least two numeric variables for PCA.", type = "error")
      return()
    }
    
    pca_data <- dataset()[, numericVars, drop = FALSE]
    pca_data <- na.omit(pca_data)
    
    if (any(!sapply(pca_data, is.numeric))) {
      showNotification("All selected variables must be numeric for PCA.", type = "error")
      return()
    }
    
    pca_model <- prcomp(pca_data, center = TRUE, scale. = TRUE)
    
    output$pcaPlot <- renderPlot({
      biplot(pca_model, main = "PCA Biplot", col = c("#3498db", "#e74c3c"))
    })
    
    output$pcaTable <- renderTable({
      as.data.frame(pca_model$x)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
