# Load required libraries
library(shiny)
library(shinyWidgets)
library(quantmod)
library(plotly)
library(DT)
library(dplyr)
library(shinycssloaders) # For adding spinners during loading

# Define a list of common stock symbols for autocomplete
common_symbols <- data.frame(
  Symbol = c("AAPL", "MSFT", "GOOGL", "AMZN", "FB", "TSLA", "JPM", "V", "PG", "NVDA"),
  Name = c("Apple Inc.", "Microsoft Corporation", "Alphabet Inc.", "Amazon.com Inc.",
           "Facebook, Inc.", "Tesla, Inc.", "JPMorgan Chase & Co.", "Visa Inc.",
           "Procter & Gamble Co.", "NVIDIA Corporation"),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  titlePanel("Enhanced Stock Market Analysis App"),
  
  sidebarLayout(
    sidebarPanel(
      # Ticker Input with Autocomplete
      pickerInput(
        inputId = "tickers",
        label = "Select Stock Ticker Symbol(s):",
        choices = common_symbols$Symbol,
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE,
          `selected-text-format` = "count > 3"
        ),
        multiple = TRUE
      ),
      
      # Date Range Selection
      dateRangeInput(
        "dateRange",
        "Select Date Range:",
        start = Sys.Date() - 365,  # Default to past year
        end = Sys.Date()
      ),
      
      # Price Type Selection
      selectInput(
        "priceType",
        "Select Price Type to Plot:",
        choices = c("Open" = "Open", "High" = "High", "Low" = "Low", "Close" = "Close"),
        selected = "Close"
      ),
      
      # Get Data Button
      actionButton(
        "getData",
        "Get Data",
        icon = icon("download"),
        class = "btn-primary"
      ),
      
      hr(),
      
      # Download Data Button
      downloadButton(
        outputId = "downloadData",
        label = "Download Data (CSV)",
        icon = icon("download")
      ),
      
      # Download Plot Button
      downloadButton(
        outputId = "downloadPlot",
        label = "Download Plot (PNG)",
        icon = icon("image")
      )
    ),
    
    mainPanel(
      # Stock Chart with spinner
      withSpinner(plotlyOutput("stockChart"), type = 4),
      
      hr(),
      
      # Stock Data Table with spinner
      withSpinner(dataTableOutput("stockTable"), type = 4)
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive value to store stock data
  stock_data <- reactiveVal(NULL)
  
  # Progress indicator
  progress <- Progress$new(session, min=1, max=100)
  on.exit(progress$close())
  
  # Observe the Get Data button click
  observeEvent(input$getData, {
    # Validate inputs
    req(input$tickers, input$dateRange)
    
    # Convert tickers to uppercase and trim any whitespace
    tickers <- toupper(trimws(input$tickers))
    
    # Initialize an empty list to store data
    data_list <- list()
    
    # Update progress bar
    progress$set(message = "Fetching data...", value = 10)
    
    # Iterate over each ticker and fetch data
    for (i in seq_along(tickers)) {
      ticker <- tickers[i]
      progress$inc(80/length(tickers), detail = paste("Fetching", ticker))
      
      data <- tryCatch({
        suppressWarnings(getSymbols(ticker, src = "yahoo", from = input$dateRange[1],
                                    to = input$dateRange[2], auto.assign = FALSE))
      }, error = function(e) {
        showNotification(paste("Error fetching data for symbol:", ticker), type = "error")
        return(NULL)
      })
      
      # Check if data retrieval was successful
      if (!is.null(data) && nrow(data) > 0) {
        # Convert to data frame and add Ticker column
        data_df <- data.frame(Date = index(data), coredata(data))
        data_df <- data_df %>% select(Date, everything())
        data_df$Ticker <- ticker
        data_list[[ticker]] <- data_df
      } else {
        showNotification(paste("No data available for symbol:", ticker), type = "error")
      }
    }
    
    # Check if any data was retrieved
    if (length(data_list) > 0) {
      combined_data <- bind_rows(data_list)
      stock_data(combined_data)
      showNotification("Data successfully retrieved.", type = "message")
    } else {
      stock_data(NULL)
      showNotification("No data retrieved for the selected symbols and date range.", type = "error")
    }
    
    # Finalize progress
    progress$set(value = 100)
    
  })
  
  # Render the stock chart
  output$stockChart <- renderPlotly({
    req(stock_data())
    
    data <- stock_data()
    
    # Identify the selected Price Type column (e.g., 'AAPL.Close')
    price_col <- paste0(input$tickers, ".", input$priceType)
    available_price_cols <- intersect(price_col, names(data))
    
    if (length(available_price_cols) == 0) {
      showNotification("Selected price type data not found.", type = "error")
      return(NULL)
    }
    
    # Reshape data to long format for easier plotting
    plot_data <- data %>%
      select(Date, Ticker, all_of(available_price_cols)) %>%
      pivot_longer(
        cols = all_of(available_price_cols),
        names_to = "Ticker_Price",
        values_to = "Price"
      ) %>%
      mutate(Ticker = gsub(paste0("\\.", input$priceType), "", Ticker_Price)) %>%
      select(Date, Ticker, Price)
    
    # Create the plotly object
    p <- plot_ly(plot_data, x = ~Date, y = ~Price, color = ~Ticker, colors = "Set1",
                 type = 'scatter', mode = 'lines', name = ~Ticker) %>%
      layout(title = paste(input$priceType, "Prices"),
             xaxis = list(title = "Date"),
             yaxis = list(title = paste(input$priceType, "Price (USD)")))
    
    return(p)
  })
  
  # Render the stock data table
  output$stockTable <- renderDataTable({
    req(stock_data())
    
    data <- stock_data()
    
    # Identify the selected Price Type column
    price_col <- paste0(input$tickers, ".", input$priceType)
    available_price_cols <- intersect(price_col, names(data))
    
    if (length(available_price_cols) == 0) {
      showNotification("Selected price type data not found.", type = "error")
      return(NULL)
    }
    
    # Select relevant columns (Date, Ticker, and selected Price Type)
    display_data <- data %>%
      select(Date, Ticker, all_of(available_price_cols)) %>%
      rename(`Price` = all_of(available_price_cols))
    
    # Arrange data by Date and Ticker
    display_data <- display_data %>% arrange(Ticker, Date)
    
    datatable(display_data, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  
  # Download Data as CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("stock_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(stock_data())
      write.csv(stock_data(), file, row.names = FALSE)
    }
  )
  
  # Download Plot as PNG
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("stock_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(stock_data())
      
      # Save the plotly object as a static image
      export(p = last_plotly(), file = file)
    }
  )
  
}

# Run the App
shinyApp(ui, server)
