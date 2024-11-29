# Load the necessary packages
library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(jsonlite)
library(xml2)
library(yaml)
library(readxl)
library(readODS)
library(officer)
library(flextable)
library(shinycssloaders) 
library(lubridate)
library(DT)
library(plotly)
library(gganimate)
library(shinyWidgets)
library(shinythemes)

# Define the User Interface (UI)
ui <- fluidPage(
  theme = shinytheme("cosmo"),  # Add a theme to enhance the UI design
  
  # Application title
  titlePanel(
    div(
      style = "background-color:#2c3e50; color:white; padding: 20px; text-align: center; border-radius: 10px;",
      h1("Enhanced Data Analysis Tool")
    )
  ),
  
  # Sidebar layout with input controls
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #f7f7f7; border-radius: 10px; padding: 15px;",
      
      # Input: File upload button with supported file types
      fileInput("fileUpload", "Choose a Data File",
                multiple = FALSE,
                accept = c(
                  ".csv", ".tsv", ".txt",
                  ".json", ".xml", ".yaml", ".yml",
                  ".xlsx", ".xls", ".ods"
                )),
      
      # Conditional panel: Display variable selection and analysis options after data is loaded
      conditionalPanel(
        condition = "output.fileLoaded == true",
        h4("Select Analysis", style = "margin-top: 20px; color: #2c3e50;"),
        
        # Dropdown menu to select analysis type
        selectInput("analysisType", "Choose Analysis Type:",
                    choices = c("EDA", "Individual Plots", "Two-Way Plots", "Fit Model"),
                    selected = "EDA"),
        
        # UI Outputs for analysis-specific inputs
        uiOutput("analysisInputs"),
        
        # Action Button to Trigger Analysis
        actionButton("analyzeBtn", "Analyze", class = "btn-primary btn-lg", style = "width: 100%; margin-top: 20px;")
      )
    ),
    
    # Main panel to display outputs
    mainPanel(
      style = "background-color: #ffffff; border-radius: 10px; padding: 15px;",
      
      # Conditional panel: Show analysis results after Analyze button is clicked
      conditionalPanel(
        condition = "output.analysisReady == true",
        h4("Data Analysis Results", style = "color: #2c3e50; margin-top: 20px;"),
        uiOutput("analysisOutputs"),
        br(),
        uiOutput("downloadLink")
      )
    )
  )
)

# Define the Server Logic
server <- function(input, output, session) {
  
  # Reactive expression to load the data when the file is uploaded
  loadedData <- reactive({
    req(input$fileUpload)
    
    # Get the uploaded file
    file <- input$fileUpload
    
    # Determine file extension
    ext <- tools::file_ext(file$name)
    
    # Initialize an empty data frame
    data <- NULL
    
    # Try to load the file based on its extension
    tryCatch({
      if (ext %in% c("csv", "tsv", "txt")) {
        delimiter <- switch(ext,
                            "csv" = ",",
                            "tsv" = "\t",
                            "txt" = "\t") # Assuming tab-delimited for txt
        data <- read_delim(file$datapath, delim = delimiter, guess_max = 10000)
      } else if (ext == "json") {
        json_data <- fromJSON(file$datapath, flatten = TRUE)
        if (is.data.frame(json_data)) {
          data <- json_data
        } else if (is.list(json_data)) {
          data <- as.data.frame(json_data)
        } else {
          stop("JSON file does not contain a valid data frame.")
        }
      } else if (ext == "xml") {
        xml_data <- read_xml(file$datapath)
        xml_list <- as_list(xml_data)
        # Attempt to convert to data frame; may require customization based on XML structure
        data <- as.data.frame(xml_list)
      } else if (ext %in% c("yaml", "yml")) {
        yaml_data <- read_yaml(file$datapath)
        if (is.data.frame(yaml_data)) {
          data <- yaml_data
        } else if (is.list(yaml_data)) {
          data <- as.data.frame(yaml_data)
        } else {
          stop("YAML file does not contain a valid data frame.")
        }
      } else if (ext %in% c("xlsx", "xls")) {
        data <- read_excel(file$datapath)
      } else if (ext == "ods") {
        data <- read_ods(file$datapath)
      } else {
        stop("Unsupported file extension.")
      }
      
      # Validate that data is a data frame
      if (!is.data.frame(data)) {
        stop("Uploaded file does not contain a valid data frame.")
      }
      
      return(data)
      
    }, error = function(e) {
      # Show error message
      showNotification(paste("Error:", e$message), type = "error")
      return(NULL)
    })
  })
  
  # Output: Boolean indicating whether the file was successfully loaded
  output$fileLoaded <- reactive({
    return(!is.null(loadedData()))
  })
  outputOptions(output, "fileLoaded", suspendWhenHidden = FALSE)
  
  # Reactive expression to process the data and detect variable types
  processedData <- reactive({
    data <- loadedData()
    req(data)
    
    vars <- names(data)
    n_total <- nrow(data)
    
    for (var in vars) {
      var_data <- data[[var]]
      
      # Check if variable is numeric or integer
      if (is.numeric(var_data) || is.integer(var_data)) {
        n_unique <- length(unique(var_data))
        
        # If the number of unique values is less than 5% of total rows and less than 10 unique values,
        # consider it categorical
        if ((n_unique / n_total < 0.05) && (n_unique < 10)) {
          data[[var]] <- as.factor(var_data)
        } else {
          data[[var]] <- var_data
        }
        
      } else if (is.character(var_data)) {
        # Try to parse as date
        parsed_date <- parse_date_time(var_data, orders = c("ymd", "mdy", "dmy", 
                                                            "ymd HMS", "mdy HMS", "dmy HMS"), exact = FALSE)
        n_parsed <- sum(!is.na(parsed_date))
        n_non_na <- sum(!is.na(var_data))
        
        if ((n_parsed / n_non_na) > 0.8) {
          data[[var]] <- parsed_date
        } else {
          n_unique <- length(unique(var_data))
          if (n_unique / n_total < 0.6) {
            data[[var]] <- as.factor(var_data)
          } else {
            data[[var]] <- var_data
          }
        }
      } else if (is.factor(var_data)) {
        data[[var]] <- var_data
      } else if (inherits(var_data, "Date") || inherits(var_data, "POSIXct")) {
        data[[var]] <- var_data
      } else {
        data[[var]] <- var_data
      }
    }
    
    return(data)
  })
  
  # Output the data table for preview
  output$dataTable <- renderDataTable({
    req(processedData())
    datatable(processedData(), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # UI Outputs for analysis-specific inputs
  output$analysisInputs <- renderUI({
    req(processedData())
    data <- processedData()
    vars <- names(data)
    
    analysis_type <- input$analysisType
    
    if (analysis_type == "EDA") {
      # No additional inputs needed for EDA
      return(NULL)
    } else if (analysis_type == "Individual Plots") {
      # Input to select variable and plot type
      fluidPage(
        selectInput("individualVar", "Select Variable:", choices = vars),
        selectInput("individualPlotType", "Select Plot Type:",
                    choices = c("Histogram", "Bar Plot", "Box Plot", "Density Plot"))
      )
    } else if (analysis_type == "Two-Way Plots") {
      # Inputs to select variables and plot type
      fluidPage(
        selectInput("twoWayVarX", "Select X Variable:", choices = vars),
        selectInput("twoWayVarY", "Select Y Variable:", choices = vars),
        selectInput("twoWayPlotType", "Select Plot Type:",
                    choices = c("Scatter Plot", "Box Plot", "Violin Plot", "Line Plot"))
      )
    } else if (analysis_type == "Fit Model") {
      # Inputs to select response and predictors
      fluidPage(
        selectInput("modelResponse", "Select Response Variable (Y):", choices = vars),
        selectInput("modelPredictors", "Select Predictor Variables (X):",
                    choices = vars, multiple = TRUE)
      )
    }
  })
  
  # Determine if analysis is ready (analyze button is pressed)
  analysisData <- eventReactive(input$analyzeBtn, {
    req(processedData())
    data <- processedData()
    analysis_type <- input$analysisType
    list(
      data = data,
      analysisType = analysis_type
    )
  })
  
  output$analysisReady <- reactive({
    return(!is.null(analysisData()))
  })
  outputOptions(output, "analysisReady", suspendWhenHidden = FALSE)
  
  # Generate Analysis Outputs
  output$analysisOutputs <- renderUI({
    analysis <- analysisData()
    req(analysis)
    data <- analysis$data
    analysis_type <- analysis$analysisType
    
    if (analysis_type == "EDA") {
      # Complete exploratory analysis for all variables
      vars <- names(data)
      eda_elements <- list()
      
      for (var in vars) {
        var_data <- data[[var]]
        missing_count <- sum(is.na(var_data))
        missing_percent <- round((missing_count / nrow(data)) * 100, 2)
        
        summary_stats <- summary(var_data)
        
        # Determine plot based on variable type
        if (is.numeric(var_data)) {
          plot <- ggplot(data, aes_string(x = var)) +
            geom_histogram(fill = "#3498db", color = "black", bins = 30) +
            labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        } else if (is.factor(var_data)) {
          plot <- ggplot(data, aes_string(x = var)) +
            geom_bar(fill = "#3498db", color = "black") +
            labs(title = paste("Bar Plot of", var), x = var, y = "Count") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        } else if (inherits(var_data, "Date") || inherits(var_data, "POSIXct")) {
          plot <- ggplot(data, aes_string(x = var)) +
            geom_histogram(fill = "#3498db", color = "black", bins = 30) +
            labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
            theme_minimal() +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        } else {
          plot <- NULL
        }
        
        # Add elements to UI
        eda_elements <- append(eda_elements, list(
          h3(paste("Variable:", var), style = "color: #2c3e50;"),
          verbatimTextOutput(paste0("summary_", var)),
          verbatimTextOutput(paste0("missing_", var))
        ))
        
        if (!is.null(plot)) {
          eda_elements <- append(eda_elements, list(
            plotlyOutput(paste0("plot_", var)) %>% withSpinner()
          ))
          local({
            local_var <- var
            local_plot <- ggplotly(plot)
            output[[paste0("plot_", local_var)]] <- renderPlotly({
              local_plot
            })
          })
        }
        
        # Render summary and missing values
        local({
          local_var <- var
          local_summary <- summary_stats
          local_missing <- paste("Missing Values:", missing_count,
                                 "(", missing_percent, "% )")
          
          output[[paste0("summary_", local_var)]] <- renderPrint({
            local_summary
          })
          
          output[[paste0("missing_", local_var)]] <- renderPrint({
            local_missing
          })
        })
      }
      
      do.call(tagList, eda_elements)
      
    } else if (analysis_type == "Individual Plots") {
      req(input$individualVar)
      req(input$individualPlotType)
      
      var <- input$individualVar
      plot_type <- input$individualPlotType
      var_data <- data[[var]]
      
      # Generate plot based on selected type
      if (plot_type == "Histogram" && is.numeric(var_data)) {
        plot <- ggplot(data, aes_string(x = var)) +
          geom_histogram(fill = "#3498db", color = "black", bins = 30) +
          labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      } else if (plot_type == "Bar Plot" && is.factor(var_data)) {
        plot <- ggplot(data, aes_string(x = var)) +
          geom_bar(fill = "#3498db", color = "black") +
          labs(title = paste("Bar Plot of", var), x = var, y = "Count") +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      } else if (plot_type == "Box Plot" && is.numeric(var_data)) {
        plot <- ggplot(data, aes_string(y = var)) +
          geom_boxplot(fill = "#3498db", color = "black") +
          labs(title = paste("Box Plot of", var), y = var) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      } else if (plot_type == "Density Plot" && is.numeric(var_data)) {
        plot <- ggplot(data, aes_string(x = var)) +
          geom_density(fill = "#3498db", color = "black") +
          labs(title = paste("Density Plot of", var), x = var) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      } else {
        plot <- NULL
        showNotification("Selected plot type is not compatible with the variable type.", type = "error")
      }
      
      if (!is.null(plot)) {
        output$individualPlotOutput <- renderPlotly({
          ggplotly(plot)
        })
        plotlyOutput("individualPlotOutput") %>% withSpinner()
      }
      
    } else if (analysis_type == "Two-Way Plots") {
      req(input$twoWayVarX)
      req(input$twoWayVarY)
      req(input$twoWayPlotType)
      
      var_x <- input$twoWayVarX
      var_y <- input$twoWayVarY
      plot_type <- input$twoWayPlotType
      
      var_x_data <- data[[var_x]]
      var_y_data <- data[[var_y]]
      
      # Generate plot based on selected type and variable types
      if (plot_type == "Scatter Plot" && is.numeric(var_x_data) && is.numeric(var_y_data)) {
        plot <- ggplot(data, aes_string(x = var_x, y = var_y)) +
          geom_point(color = "#2ecc71") +
          labs(title = paste("Scatter Plot of", var_y, "vs", var_x),
               x = var_x, y = var_y) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      } else if (plot_type == "Box Plot" && is.factor(var_x_data) && is.numeric(var_y_data)) {
        plot <- ggplot(data, aes_string(x = var_x, y = var_y)) +
          geom_boxplot(fill = "#2ecc71", color = "black") +
          labs(title = paste("Box Plot of", var_y, "by", var_x),
               x = var_x, y = var_y) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      } else if (plot_type == "Violin Plot" && is.factor(var_x_data) && is.numeric(var_y_data)) {
        plot <- ggplot(data, aes_string(x = var_x, y = var_y)) +
          geom_violin(fill = "#2ecc71", color = "black") +
          labs(title = paste("Violin Plot of", var_y, "by", var_x),
               x = var_x, y = var_y) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      } else if (plot_type == "Line Plot" && (is.numeric(var_x_data) || inherits(var_x_data, "Date") || inherits(var_x_data, "POSIXct")) && is.numeric(var_y_data)) {
        plot <- ggplot(data, aes_string(x = var_x, y = var_y)) +
          geom_line(color = "#e74c3c") +
          labs(title = paste("Line Plot of", var_y, "over", var_x),
               x = var_x, y = var_y) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      } else {
        plot <- NULL
        showNotification("Selected plot type is not compatible with the variable types.", type = "error")
      }
      
      if (!is.null(plot)) {
        output$twoWayPlotOutput <- renderPlotly({
          ggplotly(plot)
        })
        plotlyOutput("twoWayPlotOutput") %>% withSpinner()
      }
      
    } else if (analysis_type == "Fit Model") {
      req(input$modelResponse)
      req(input$modelPredictors)
      
      response <- input$modelResponse
      predictors <- input$modelPredictors
      
      # Build formula
      formula_text <- paste(response, "~", paste(predictors, collapse = " + "))
      model_formula <- as.formula(formula_text)
      
      # Fit linear model
      model <- lm(model_formula, data = data)
      
      # Model summary
      model_summary <- summary(model)
      
      # Diagnostic plots
      diagnostic_plots <- list()
      
      # Residuals vs Fitted
      plot1 <- ggplot(model, aes(.fitted, .resid)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "#e74c3c") +
        labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
      # Normal Q-Q
      plot2 <- ggplot(model, aes(sample = .stdresid)) +
        stat_qq() +
        stat_qq_line() +
        labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
      # Scale-Location Plot
      plot3 <- ggplot(model, aes(.fitted, sqrt(abs(.stdresid)))) +
        geom_point() +
        geom_smooth(se = FALSE) +
        labs(title = "Scale-Location", x = "Fitted values", y = "Sqrt(|Standardized Residuals|)") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
      # Residuals vs Leverage
      plot4 <- ggplot(model, aes(.hat, .stdresid)) +
        geom_point() +
        geom_smooth(se = FALSE) +
        labs(title = "Residuals vs Leverage", x = "Leverage", y = "Standardized Residuals") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
      # Render plots
      output$modelPlot1 <- renderPlot({ plot1 })
      output$modelPlot2 <- renderPlot({ plot2 })
      output$modelPlot3 <- renderPlot({ plot3 })
      output$modelPlot4 <- renderPlot({ plot4 })
      
      # Render model summary
      output$modelSummary <- renderPrint({ model_summary })
      
      # Return UI elements
      tagList(
        h3("Model Summary", style = "color: #2c3e50;"),
        verbatimTextOutput("modelSummary"),
        h3("Diagnostic Plots", style = "color: #2c3e50;"),
        fluidRow(
          column(6, plotOutput("modelPlot1") %>% withSpinner()),
          column(6, plotOutput("modelPlot2") %>% withSpinner())
        ),
        fluidRow(
          column(6, plotOutput("modelPlot3") %>% withSpinner()),
          column(6, plotOutput("modelPlot4") %>% withSpinner())
        )
      )
    }
  })
  
  # Download report
  output$downloadLink <- renderUI({
    req(analysisData())
    downloadButton("downloadReport", "Download Analysis Report", class = "btn-success btn-lg", style = "width: 100%; margin-top: 20px;")
  })
  
  # Download Handler
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("Analysis_Report_", Sys.Date(), ".docx")
    },
    content = function(file) {
      # Implement report generation using officer and flextable
      # This requires building the report based on the selected analysis
      
      analysis <- analysisData()
      req(analysis)
      data <- analysis$data
      analysis_type <- analysis$analysisType
      
      # Initialize a new Word document
      doc <- read_docx()
      
      # Add content based on analysis type
      if (analysis_type == "EDA") {
        doc <- doc %>%
          body_add_par("Exploratory Data Analysis", style = "heading 1")
        vars <- names(data)
        
        for (var in vars) {
          var_data <- data[[var]]
          missing_count <- sum(is.na(var_data))
          missing_percent <- round((missing_count / nrow(data)) * 100, 2)
          
          summary_stats <- summary(var_data)
          
          # Create summary table
          summary_df <- data.frame(Statistic = names(summary_stats),
                                   Value = as.character(summary_stats),
                                   stringsAsFactors = FALSE)
          summary_ft <- flextable(summary_df)
          summary_ft <- autofit(summary_ft)
          
          # Determine plot based on variable type
          if (is.numeric(var_data)) {
            plot <- ggplot(data, aes_string(x = var)) +
              geom_histogram(fill = "#3498db", color = "black", bins = 30) +
              labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
              theme_minimal()
          } else if (is.factor(var_data)) {
            plot <- ggplot(data, aes_string(x = var)) +
              geom_bar(fill = "#3498db", color = "black") +
              labs(title = paste("Bar Plot of", var), x = var, y = "Count") +
              theme_minimal()
          } else if (inherits(var_data, "Date") || inherits(var_data, "POSIXct")) {
            plot <- ggplot(data, aes_string(x = var)) +
              geom_histogram(fill = "#3498db", color = "black", bins = 30) +
              labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
              theme_minimal()
          } else {
            plot <- NULL
          }
          
          # Save plot to temporary file
          if (!is.null(plot)) {
            plot_path <- tempfile(fileext = ".png")
            ggsave(plot_path, plot = plot, width = 6, height = 4, dpi = 300)
          }
          
          # Add to Word Document
          doc <- doc %>%
            body_add_par(paste("Variable:", var), style = "heading 2") %>%
            body_add_par(paste("Missing Values:", missing_count, 
                               "(", missing_percent, "% )"), style = "Normal") %>%
            body_add_par("Summary Statistics:", style = "heading 3") %>%
            body_add_flextable(summary_ft)
          
          if (!is.null(plot)) {
            doc <- doc %>%
              body_add_par("Plot:", style = "heading 3") %>%
              body_add_img(plot_path, width = 6, height = 4, style = "centered")
          }
        }
      }
      # Similar logic for Individual Plots, Two-Way Plots, Fit Model
      
      # Save the Word document to the specified file
      print(doc, target = file)
    }
  )
}

# Run the Shiny application
shinyApp(ui = ui, server = server)