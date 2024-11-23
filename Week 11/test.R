# Load necessary libraries
library(shiny)
library(plotly)
library(stats)
library(shinythemes)

# Define UI for Shiny app
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Interactive PCA Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = ".csv"),
      uiOutput("var_select"),
      actionButton("run_pca", "Perform PCA", class = "btn btn-primary"),
      numericInput("n_comp", "Number of Components to Retain", value = 2, min = 1),
      actionButton("generate_plot", "Generate 3D Plot", class = "btn btn-success")
    ),
    mainPanel(
      verbatimTextOutput("pca_summary"),
      plotOutput("scree_plot"),
      plotlyOutput("pca_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive expression to read the uploaded data
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Render UI for variable selection
  output$var_select <- renderUI({
    req(data())
    checkboxGroupInput("selected_vars", "Select Variables for PCA", choices = names(data()))
  })
  
  # Perform PCA when button is clicked
  pca_result <- eventReactive(input$run_pca, {
    req(input$selected_vars)
    selected_data <- data()[, input$selected_vars]
    prcomp(selected_data, scale. = TRUE)
  })
  
  # Display PCA summary
  output$pca_summary <- renderPrint({
    req(pca_result())
    summary(pca_result())
  })
  
  # Generate Scree Plot
  output$scree_plot <- renderPlot({
    req(pca_result())
    variances <- pca_result()$sdev^2
    variance_explained <- variances / sum(variances)
    
    barplot(variance_explained,
            main = "Scree Plot",
            xlab = "Principal Components",
            ylab = "Proportion of Variance Explained",
            col = "steelblue")
  })
  
  # Generate 3D scatter plot using plotly
  output$pca_plot <- renderPlotly({
    req(pca_result(), input$generate_plot)
    pca_data <- pca_result()$x[, 1:3]
    
    plot_ly(
      x = pca_data[, 1],
      y = pca_data[, 2],
      z = pca_data[, 3],
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 5, color = pca_data[, 1], colorscale = "Viridis")
    ) %>% layout(scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)