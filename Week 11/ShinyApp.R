# Install required packages if the app is not already installed --------------------------------

if (!require("shiny")) install.packages("shiny")
if (!require("bslib")) install.packages("bslib")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tools")) install.packages("tools")
if (!require("viridis")) install.packages("viridis")
if (!require("ggthemes")) install.packages("ggthemes")
if (!require("shiny.fluent")) install.packages("shiny.fluent")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("shinyjs")) install.packages("shinyjs")
if (!require("DT")) install.packages("DT")
if (!require("plotly")) install.packages("plotly")
if (!require("shinycssloaders")) install.packages("shinycssloaders")
if (!require("caret")) install.packages("caret")
if (!require("randomForest")) install.packages("randomForest")

# Load packages ----------------------------------------------------------------

library(shiny)
library(bslib)
library(ggplot2)
library(tools)
library(viridis)
library(ggthemes)
library(shiny.fluent)  # For a modern UI
library(shinyWidgets)  # For interactive input widgets
library(shinyjs)       # For adding JavaScript functionality
library(DT)            # For modern tables
library(plotly)        # For interactive plots
library(shinycssloaders) # For adding loading animations
library(caret)         # For machine learning
library(randomForest)  # For prediction

# Load data --------------------------------------------------------------------

load("movies.RData")

# Define UI --------------------------------------------------------------------

genres <- unique(movies$genre)

theme <- bs_theme(
  version = 4,
  bootswatch = "cyborg",  # Use an advanced, dark, and modern theme
  primary = "#17a2b8",
  secondary = "#6c757d",
  success = "#28a745",
  info = "#17a2b8",
  warning = "#ffc107",
  danger = "#dc3545",
  base_font = font_google("Roboto Mono"),
  heading_font = font_google("Raleway")
)

ui <- fluidPage(
  theme = theme,
  useShinyjs(),
  tags$head(
    tags$style(HTML(
      "
      .btn-primary:hover {
        background-color: #138496;
        transition: background-color 0.3s ease;
      }
      .sidebarPanel:hover {
        box-shadow: 0 0 15px rgba(23, 162, 184, 0.6);
        transition: box-shadow 0.3s ease;
      }
      .card:hover {
        box-shadow: 0 0 15px rgba(23, 162, 184, 0.8);
        transition: box-shadow 0.3s ease;
      }
      .geom_smooth {
        color: rgba(255, 255, 255, 0.2) !important;
      }
      "
    ))
  ),
  
  titlePanel("Movie Data Visualization & Prediction App"),
  sidebarLayout(
    sidebarPanel(
      tags$h3("Customize Your Plot", style = "color: #17a2b8;"),
      selectInput(
        inputId = "x_axis",
        label = "X-axis:",
        choices = c("IMDB Rating" = "imdb_rating", "Critics Score" = "critics_score", "Audience Score" = "audience_score", "Runtime" = "runtime"),
        selected = "critics_score"
      ),
      selectInput(
        inputId = "y_axis",
        label = "Y-axis:",
        choices = c("IMDB Rating" = "imdb_rating", "Critics Score" = "critics_score", "Audience Score" = "audience_score", "Runtime" = "runtime"),
        selected = "audience_score"
      ),
      selectInput(
        inputId = "color_by",
        label = "Color By:",
        choices = c("Genre" = "genre", "MPAA Rating" = "mpaa_rating", "Critics Rating" = "critics_rating", "Audience Rating" = "audience_rating"),
        selected = "genre"
      ),
      sliderInput(
        inputId = "alpha",
        label = "Point Transparency (Alpha):",
        min = 0, max = 1,
        value = 0.7
      ),
      sliderInput(
        inputId = "size",
        label = "Point Size:",
        min = 1, max = 10,
        value = 3
      ),
      selectInput(
        inputId = "point_shape",
        label = "Point Shape:",
        choices = c("Circle" = 16, "Triangle" = 17, "Square" = 15, "Diamond" = 18, "Plus" = 3, "Cross" = 4),
        selected = 16
      ),
      awesomeCheckbox(
        inputId = "show_smooth",
        label = "Show Smoothing Line",
        value = TRUE,
        status = "primary"
      ),
      pickerInput(
        inputId = "selected_genres",
        label = "Filter by Genre:",
        choices = genres,
        multiple = TRUE,
        options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE)
      ),
      pickerInput(
        inputId = "color_palette",
        label = "Color Palette:",
        choices = c("Default", "Viridis", "Plasma", "Inferno", "Magma", "Cividis"),
        selected = "Viridis",
        options = pickerOptions(style = "btn-primary")
      ),
      pickerInput(
        inputId = "plot_theme",
        label = "Plot Theme:",
        choices = c(
          "Minimal" = "theme_minimal",
          "Classic" = "theme_classic",
          "Gray" = "theme_gray",
          "Light" = "theme_light",
          "Dark" = "theme_dark",
          "Void" = "theme_void"
        ),
        selected = "theme_minimal",
        options = pickerOptions(style = "btn-primary")
      ),
      textInput(
        inputId = "plot_title",
        label = "Plot Title:",
        placeholder = "Enter plot title here"
      ),
      actionButton(
        inputId = "update_plot_title",
        label = "Update Plot Title",
        icon = icon("refresh"),
        class = "btn btn-primary"
      ),
      tags$hr(),
      tags$h3("Predict Movie Audience Score", style = "color: #17a2b8;"),
      numericInput(
        inputId = "predict_runtime",
        label = "Enter Runtime (minutes):",
        value = 120,
        min = 30,
        max = 300
      ),
      selectInput(
        inputId = "predict_genre",
        label = "Select Genre:",
        choices = genres
      ),
      actionButton(
        inputId = "predict_button",
        label = "Predict Audience Score",
        icon = icon("magic"),
        class = "btn btn-success"
      ),
      verbatimTextOutput("prediction_output")
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Scatterplot",
          br(),
          withSpinner(plotlyOutput(outputId = "scatterplot", height = "600px"), type = 6, color = "#17a2b8"),
          br(),
          downloadButton(
            outputId = "download_plot",
            label = "Save Plot",
            class = "btn btn-info"
          )
        ),
        tabPanel(
          "Data Table",
          br(),
          withSpinner(DT::dataTableOutput("movies_table"), type = 6, color = "#17a2b8")
        ),
        tabPanel(
          "Details",
          br(),
          markdown(
            "These data were obtained from [IMDB](http://www.imdb.com/) and [Rotten Tomatoes](https://www.rottentomatoes.com/). 

            The data represent 651 randomly sampled movies released between 1972 to 2014 in the United States."
          )
        )
      )
    )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  new_plot_title <- eventReactive(
    eventExpr = input$update_plot_title,
    valueExpr = {
      toTitleCase(input$plot_title)
    }
  )
  
  filtered_data <- reactive({
    data <- movies
    if (!is.null(input$selected_genres) && length(input$selected_genres) > 0) {
      data <- data[data$genre %in% input$selected_genres, ]
    }
    data
  })
  
  output$scatterplot <- renderPlotly({
    p <- ggplot(data = filtered_data(), aes_string(x = input$x_axis, y = input$y_axis, color = input$color_by)) +
      geom_point(alpha = input$alpha, size = input$size, shape = as.numeric(input$point_shape)) +
      labs(title = new_plot_title(), x = input$x_axis, y = input$y_axis)
    
    if (input$show_smooth) {
      p <- p + geom_smooth()
    }
    
    # Apply selected color palette
    if (input$color_palette != "Default") {
      p <- p + scale_color_viridis_d(option = tolower(input$color_palette))
    }
    
    # Apply selected theme
    p <- p + do.call(get(input$plot_theme), list())
    
    ggplotly(p) %>% layout(hoverlabel = list(font = list(size = 16)))
  })
  
  output$movies_table <- DT::renderDataTable({
    datatable(
      filtered_data(),
      options = list(pageLength = 10, autoWidth = TRUE),
      class = "display cell-border stripe hover",
      style = "bootstrap4"
    )
  })
  
  observeEvent(input$predict_button, {
    genre <- input$predict_genre
    runtime <- input$predict_runtime
    
    model_data <- na.omit(movies[, c("runtime", "audience_score", "genre")])
    model_data$genre <- as.factor(model_data$genre)
    new_data <- data.frame(runtime = runtime, genre = factor(genre, levels = levels(model_data$genre)))
    
    set.seed(123)
    model <- randomForest(audience_score ~ runtime + genre, data = model_data, ntree = 100)
    
    prediction <- predict(model, newdata = new_data)
    
    output$prediction_output <- renderText({
      paste("Predicted Audience Score:", round(prediction, 2))
    })
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("scatterplot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", width = 10, height = 6)
    }
  )
}

# Create the Shiny app object --------------------------------------------------

shinyApp(ui = ui, server = server)
