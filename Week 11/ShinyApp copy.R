library(shiny)
library(shinythemes)
library(bslib)

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  titlePanel(div(h1("Multiple Linear Regression Shiny App", style = "color: #2C3E50; text-align: center;"))),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Dataset", accept = c(".csv", ".txt"), buttonLabel = "Browse", placeholder = "No file selected"),
      uiOutput("variable_select"),
      actionButton("fit_model", "Fit Model", class = "btn btn-primary"),
      actionButton("refine_model", "Refine Model", class = "btn btn-warning"),
      actionButton("predict", "Make Prediction", class = "btn btn-success"),
      tags$style(HTML(".btn { margin-top: 10px; }"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Summary", 
                 h4("Dataset Summary"),
                 tableOutput("data_summary")
        ),
        tabPanel("Model Summary",
                 h4("Model Summary"),
                 tableOutput("model_summary")
        ),
        tabPanel("Residual Plots",
                 h4("Residual Plots"),
                 plotOutput("residual_plot")
        ),
        tabPanel("Prediction",
                 h4("Make a Prediction"),
                 uiOutput("prediction_input"),
                 h4("Prediction Result"),
                 textOutput("prediction_result")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  dataset <- reactive({
    req(input$file)
    tryCatch({
      read.csv(input$file$datapath)
    }, error = function(e) {
      showNotification("Error in loading dataset: Please check the file format.", type = "error")
      NULL
    })
  })
  
  output$data_summary <- renderTable({
    req(dataset())
    head(dataset(), 10)
  })
  
  output$variable_select <- renderUI({
    req(dataset())
    tagList(
      selectInput("y_var", "Select Dependent Variable (Y)", names(dataset())),
      selectInput("x_vars", "Select Independent Variables (X)", choices = names(dataset()), multiple = TRUE)
    )
  })
  
  model <- reactiveVal()
  
  observeEvent(input$fit_model, {
    req(input$y_var, input$x_vars)
    tryCatch({
      formula <- as.formula(paste(input$y_var, "~", paste(input$x_vars, collapse = "+")))
      fitted_model <- lm(formula, data = dataset())
      model(fitted_model)
      showNotification("Model fitted successfully!", type = "message")
    }, error = function(e) {
      showNotification("Error in fitting model: Please check your variable selection.", type = "error")
    })
  })
  
  output$model_summary <- renderTable({
    req(model())
    summary_df <- as.data.frame(summary(model())$coefficients)
    summary_df
  })
  
  output$residual_plot <- renderPlot({
    req(model())
    par(mfrow = c(2, 2))
    plot(model(), col = "#2C3E50", pch = 19)
  })
  
  output$prediction_input <- renderUI({
    req(input$x_vars)
    tagList(
      lapply(input$x_vars, function(var) {
        numericInput(var, paste("Enter value for", var), value = 0)
      }),
      actionButton("predict", "Make Prediction", class = "btn btn-success")
    )
  })
  
  observeEvent(input$predict, {
    req(model(), input$x_vars)
    new_data <- as.data.frame(t(sapply(input$x_vars, function(var) input[[var]])))
    names(new_data) <- input$x_vars
    tryCatch({
      prediction <- predict(model(), newdata = new_data)
      output$prediction_result <- renderText({ paste("Predicted Value:", round(prediction, 2)) })
      showNotification("Prediction made successfully!", type = "message")
    }, error = function(e) {
      showNotification("Error in making prediction: Please check input values.", type = "error")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)