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
library(shinycssloaders) # Optional: For adding loading animations
library(lubridate)
library(DT)
library(shinythemes) # New library for themes

# Define the User Interface (UI)
ui <- fluidPage(
  theme = shinytheme("cerulean"), # New theme added
  
  # Application title
  titlePanel("Advanced Data Analysis Tool Inspired by JMP"),
  
  # Sidebar layout with input controls
  sidebarLayout(
    sidebarPanel(
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
        h4("Select Analysis"),
        
        # Dropdown menu to select analysis type
        selectInput("analysisType", "Choose Analysis Type:",
                    choices = c("EDA", "Individual Plots", "Two-Way Plots", "Fit Model", "Correlation Matrix", "Summary Statistics"),
                    selected = "EDA"),
        
        # UI Outputs for analysis-specific inputs
        uiOutput("analysisInputs"),
        
        # Action Button to Trigger Analysis
        actionButton("analyzeBtn", "Analyze")
      )
    ),
    
    # Main panel to display outputs
    mainPanel(
      tabsetPanel( # Added tabsetPanel to organize outputs better
        tabPanel("Data Preview", DTOutput("dataTable")),
        tabPanel("Analysis Results", 
                 conditionalPanel(
                   condition = "output.analysisReady == true",
                   h4("Data Analysis Results"),
                   uiOutput("analysisOutputs"),
                   br(),
                   uiOutput("downloadLink")
                 ))
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
          # Keep as numeric
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
        # Keep as factor
        data[[var]] <- var_data
      } else if (inherits(var_data, "Date") || inherits(var_data, "POSIXct")) {
        data[[var]] <- var_data
      } else {
        # For other types, keep as is
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
      return(NULL)
    } else if (analysis_type == "Individual Plots") {
      fluidPage(
        selectInput("individualVar", "Select Variable:", choices = vars),
        selectInput("individualPlotType", "Select Plot Type:",
                    choices = c("Histogram", "Bar Plot", "Box Plot", "Density Plot"))
      )
    } else if (analysis_type == "Two-Way Plots") {
      fluidPage(
        selectInput("twoWayVarX", "Select X Variable:", choices = vars),
        selectInput("twoWayVarY", "Select Y Variable:", choices = vars),
        selectInput("twoWayPlotType", "Select Plot Type:",
                    choices = c("Scatter Plot", "Box Plot", "Violin Plot", "Line Plot"))
      )
    } else if (analysis_type == "Fit Model") {
      fluidPage(
        selectInput("modelResponse", "Select Response Variable (Y):", choices = vars),
        selectInput("modelPredictors", "Select Predictor Variables (X):",
                    choices = vars, multiple = TRUE)
      )
    } else if (analysis_type == "Correlation Matrix") {
      return(NULL) # No additional inputs needed
    } else if (analysis_type == "Summary Statistics") {
      return(NULL) # No additional inputs needed
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
      vars <- names(data)
      eda_elements <- list()
      
      for (var in vars) {
        var_data <- data[[var]]
        missing_count <- sum(is.na(var_data))
        missing_percent <- round((missing_count / nrow(data)) * 100, 2)
        summary_stats <- summary(var_data)
        
        if (is.numeric(var_data)) {
          plot <- ggplot(data, aes_string(x = var)) +
            geom_histogram(fill = "lightblue", color = "black", bins = 30) +
            labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
            theme_minimal()
        } else if (is.factor(var_data)) {
          plot <- ggplot(data, aes_string(x = var)) +
            geom_bar(fill = "lightblue", color = "black") +
            labs(title = paste("Bar Plot of", var), x = var, y = "Count") +
            theme_minimal()
        } else if (inherits(var_data, "Date") || inherits(var_data, "POSIXct")) {
          plot <- ggplot(data, aes_string(x = var)) +
            geom_histogram(fill = "lightblue", color = "black", bins = 30) +
            labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
            theme_minimal()
        } else {
          plot <- NULL
        }
        
        eda_elements <- append(eda_elements, list(
          h3(paste("Variable:", var)),
          verbatimTextOutput(paste0("summary_", var)),
          verbatimTextOutput(paste0("missing_", var))
        ))
        
        if (!is.null(plot)) {
          eda_elements <- append(eda_elements, list(
            plotOutput(paste0("plot_", var)) %>% withSpinner()
          ))
          local({
            local_var <- var
            local_plot <- plot
            output[[paste0("plot_", local_var)]] <- renderPlot({
              local_plot
            })
          })
        }
        
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
      
    } else if (analysis_type == "Correlation Matrix") {
      numeric_data <- dplyr::select_if(data, is.numeric)
      corr_matrix <- cor(numeric_data, use = "complete.obs")
      corr_plot <- ggcorrplot::ggcorrplot(corr_matrix, lab = TRUE, lab_size = 3, colors = c("#6D9EC1", "white", "#E46726"))
      
      output$corrMatrixPlot <- renderPlot({
        corr_plot
      })
      
      tagList(
        h3("Correlation Matrix"),
        plotOutput("corrMatrixPlot") %>% withSpinner()
      )
    } else if (analysis_type == "Summary Statistics") {
      summary_stats <- summary(data)
      
      output$summaryStatsOutput <- renderPrint({
        summary_stats
      })
      
      tagList(
        h3("Summary Statistics"),
        verbatimTextOutput("summaryStatsOutput")
      )
    }
  })
  
  # Download report
  output$downloadLink <- renderUI({
    req(analysisData())
    downloadButton("downloadReport", "Download Analysis Report")
  })
  
  # Download Handler
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("Analysis_Report_", Sys.Date(), ".docx")
    },
    content = function(file) {
      analysis <- analysisData()
      req(analysis)
      data <- analysis$data
      analysis_type <- analysis$analysisType
      
      doc <- read_docx()
      
      if (analysis_type == "EDA") {
        doc <- doc %>%
          body_add_par("Exploratory Data Analysis", style = "heading 1")
        vars <- names(data)
        
        for (var in vars) {
          var_data <- data[[var]]
          missing_count <- sum(is.na(var_data))
          missing_percent <- round((missing_count / nrow(data)) * 100, 2)
          summary_stats <- summary(var_data)
          
          summary_df <- data.frame(Statistic = names(summary_stats),
                                   Value = as.character(summary_stats),
                                   stringsAsFactors = FALSE)
          summary_ft <- flextable(summary_df)
          summary_ft <- autofit(summary_ft)
          
          if (is.numeric(var_data)) {
            plot <- ggplot(data, aes_string(x = var)) +
              geom_histogram(fill = "lightblue", color = "black", bins = 30) +
              labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
              theme_minimal()
          } else if (is.factor(var_data)) {
            plot <- ggplot(data, aes_string(x = var)) +
              geom_bar(fill = "lightblue", color = "black") +
              labs(title = paste("Bar Plot of", var), x = var, y = "Count") +
              theme_minimal()
          } else if (inherits(var_data, "Date") || inherits(var_data, "POSIXct")) {
            plot <- ggplot(data, aes_string(x = var)) +
              geom_histogram(fill = "lightblue", color = "black", bins = 30) +
              labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
              theme_minimal()
          } else {
            plot <- NULL
          }
          
          if (!is.null(plot)) {
            plot_path <- tempfile(fileext = ".png")
            ggsave(plot_path, plot = plot, width = 6, height = 4, dpi = 300)
          }
          
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
      
      print(doc, target = file)
    }
  )
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
