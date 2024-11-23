# Load required libraries
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(quantmod)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(prophet)
library(TTR)

# Define a list of common stock symbols for autocomplete
common_symbols <- data.frame(
  Symbol = c("AAPL", "MSFT", "GOOGL", "AMZN", "FB", "TSLA", "JPM", "V", "PG", "NVDA"),
  Name = c("Apple Inc.", "Microsoft Corporation", "Alphabet Inc.", "Amazon.com Inc.",
           "Facebook, Inc.", "Tesla, Inc.", "JPMorgan Chase & Co.", "Visa Inc.",
           "Procter & Gamble Co.", "NVIDIA Corporation"),
  stringsAsFactors = FALSE
)

# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Enhanced Stock Market Analysis App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("chart-line")),
      menuItem("Stock Predictions", tabName = "predictions", icon = icon("chart-bar")),
      menuItem("Data Analysis", tabName = "analysis", icon = icon("table")),
      menuItem("Technical Indicators", tabName = "indicators", icon = icon("chart-area"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 12, status = "primary",
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
                    dateRangeInput(
                      "dateRange",
                      "Select Date Range:",
                      start = Sys.Date() - 365,
                      end = Sys.Date()
                    ),
                    actionButton(
                      "getData",
                      "Get Data",
                      icon = icon("download"),
                      class = "btn-primary"
                    )
                )
              ),
              fluidRow(
                box(plotlyOutput("stockChart"), width = 12)
              ),
              fluidRow(
                box(dataTableOutput("stockTable"), width = 12)
              )
      ),
      
      tabItem(tabName = "predictions",
              fluidRow(
                box(width = 12, status = "primary",
                    pickerInput(
                      inputId = "pred_ticker",
                      label = "Select Stock for Prediction:",
                      choices = common_symbols$Symbol,
                      options = list(`live-search` = TRUE),
                      multiple = FALSE
                    ),
                    dateRangeInput(
                      "pred_dateRange",
                      "Select Date Range for Prediction:",
                      start = Sys.Date() - 365,
                      end = Sys.Date()
                    ),
                    actionButton(
                      "predictData",
                      "Predict Stock Price",
                      icon = icon("chart-bar"),
                      class = "btn-success"
                    )
                )
              ),
              fluidRow(
                box(plotlyOutput("predictionPlot"), width = 12)
              )
      ),
      
      tabItem(tabName = "analysis",
              fluidRow(
                box(width = 12, status = "primary",
                    selectInput(
                      "analysis_metric",
                      "Select Analysis Metric:",
                      choices = c("Daily Returns", "Cumulative Returns", "Moving Average")
                    ),
                    actionButton(
                      "analyzeData",
                      "Analyze",
                      icon = icon("chart-line"),
                      class = "btn-info"
                    )
                )
              ),
              fluidRow(
                box(plotlyOutput("analysisPlot"), width = 12)
              )
      ),
      
      tabItem(tabName = "indicators",
              fluidRow(
                box(width = 12, status = "primary",
                    pickerInput(
                      inputId = "indicator_ticker",
                      label = "Select Stock for Technical Indicators:",
                      choices = common_symbols$Symbol,
                      options = list(`live-search` = TRUE),
                      multiple = FALSE
                    ),
                    dateRangeInput(
                      "indicator_dateRange",
                      "Select Date Range for Indicators:",
                      start = Sys.Date() - 365,
                      end = Sys.Date()
                    ),
                    actionButton(
                      "getIndicators",
                      "Get Indicators",
                      icon = icon("chart-area"),
                      class = "btn-warning"
                    )
                )
              ),
              fluidRow(
                box(plotlyOutput("indicatorPlot"), width = 12)
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value to store stock data
  stock_data <- reactiveVal(NULL)
  
  # Observe the Get Data button click
  observeEvent(input$getData, {
    req(input$tickers, input$dateRange)
    tickers <- toupper(trimws(input$tickers))
    showModal(modalDialog("Fetching data, please wait...", footer = NULL))
    data_list <- list()
    
    for (ticker in tickers) {
      data <- tryCatch({
        suppressWarnings(getSymbols(ticker, src = "yahoo", from = input$dateRange[1],
                                    to = input$dateRange[2], auto.assign = FALSE))
      }, error = function(e) {
        showNotification(paste("Error fetching data for symbol:", ticker), type = "error")
        return(NULL)
      })
      
      if (!is.null(data) && nrow(data) > 0) {
        data_df <- data.frame(Date = index(data), coredata(data))
        data_df <- data_df %>% select(Date, everything())
        data_df$Ticker <- ticker
        data_list[[ticker]] <- data_df
      } else {
        showNotification(paste("No data available for symbol:", ticker), type = "error")
      }
    }
    
    if (length(data_list) > 0) {
      combined_data <- bind_rows(data_list)
      stock_data(combined_data)
      showNotification("Data successfully retrieved.", type = "message")
    } else {
      stock_data(NULL)
      showNotification("No data retrieved for the selected symbols and date range.", type = "error")
    }
    removeModal()
  })
  
  # Render the stock chart
  output$stockChart <- renderPlotly({
    req(stock_data())
    data <- stock_data()
    close_cols <- grep("\\.Close", names(data), value = TRUE)
    if (length(close_cols) == 0) {
      showNotification("Close price data not found.", type = "error")
      return(NULL)
    }
    plot_data <- data %>%
      select(Date, Ticker, all_of(close_cols)) %>%
      pivot_longer(
        cols = all_of(close_cols),
        names_to = "Ticker_Close",
        values_to = "Close"
      ) %>%
      mutate(Ticker = gsub("\\.Close", "", Ticker_Close)) %>%
      select(Date, Ticker, Close)
    
    p <- plot_ly(data_df, x = ~Date) %>%
      add_lines(y = ~Close, name = "Close Price", line = list(color = 'black')) %>%
      add_lines(y = ~BBand_up, name = "Upper Band", line = list(color = 'blue')) %>%
      add_lines(y = ~BBand_dn, name = "Lower Band", line = list(color = 'red')) %>%
      add_lines(y = ~BBand_mavg, name = "Moving Average", line = list(color = 'green')) %>%
      add_lines(y = ~RSI, name = "RSI", line = list(color = 'purple'), yaxis = "y2") %>%
      layout(title = "Technical Indicators",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Price (USD)"),
             yaxis2 = list(title = "RSI", overlaying = "y", side = "right"))
    
    return(p)
  })
  
  # Render the stock data table
  output$stockTable <- renderDataTable({
    req(stock_data())
    data <- stock_data()
    close_cols <- grep("\\.Close", names(data), value = TRUE)
    if (length(close_cols) == 0) {
      showNotification("Close price data not found.", type = "error")
      return(NULL)
    }
    display_data <- data %>%
      select(Date, Ticker, all_of(close_cols)) %>%
      rename(`Close Price` = all_of(close_cols))
    datatable(display_data, options = list(pageLength = 10), rownames = FALSE)
  })
  
  # Stock Prediction
  observeEvent(input$predictData, {
    req(input$pred_ticker, input$pred_dateRange)
    showModal(modalDialog("Predicting data, please wait...", footer = NULL))
    ticker <- toupper(trimws(input$pred_ticker))
    data <- tryCatch({
      suppressWarnings(getSymbols(ticker, src = "yahoo", from = input$pred_dateRange[1],
                                  to = input$pred_dateRange[2], auto.assign = FALSE))
    }, error = function(e) {
      showNotification(paste("Error fetching data for symbol:", ticker), type = "error")
      return(NULL)
    })
    
    if (!is.null(data) && nrow(data) > 0) {
      data_df <- data.frame(Date = index(data), coredata(data))
      data_df <- data_df %>% select(Date, Close = contains("Close"))
      df <- data_df %>% rename(ds = Date, y = Close)
      
      m <- prophet(df)
      future <- make_future_dataframe(m, periods = 30)
      forecast <- predict(m, future)
      
      p <- plot_ly() %>%
        add_lines(x = ~forecast$ds, y = ~forecast$yhat, name = "Prediction",
                  line = list(color = 'blue')) %>%
        add_lines(x = ~df$ds, y = ~df$y, name = "Actual", line = list(color = 'red')) %>%
        layout(title = "Stock Price Prediction",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Price (USD)"))
      
      output$predictionPlot <- renderPlotly({ p })
      showNotification("Prediction successful.", type = "message")
    } else {
      showNotification("No data available for symbol.", type = "error")
      output$predictionPlot <- renderPlotly({ NULL })
    }
    removeModal()
  })
  
  # Data Analysis
  observeEvent(input$analyzeData, {
    req(stock_data(), input$analysis_metric)
    data <- stock_data()
    close_cols <- grep("\\.Close", names(data), value = TRUE)
    if (length(close_cols) == 0) {
      showNotification("Close price data not found.", type = "error")
      return(NULL)
    }
    
    metric <- input$analysis_metric
    analysis_data <- data %>% select(Date, Ticker, all_of(close_cols)) %>% rename(Close = all_of(close_cols))
    
    if (metric == "Daily Returns") {
      analysis_data <- analysis_data %>% group_by(Ticker) %>% mutate(Return = (Close / lag(Close) - 1) * 100)
      p <- plot_ly(analysis_data, x = ~Date, y = ~Return, color = ~Ticker, colors = "Set1",
                   type = 'scatter', mode = 'lines', name = ~Ticker) %>%
        layout(title = "Daily Returns",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Return (%)"))
    } else if (metric == "Cumulative Returns") {
      analysis_data <- analysis_data %>% group_by(Ticker) %>% mutate(Cumulative = cumprod(1 + (Close / lag(Close) - 1)))
      p <- plot_ly(analysis_data, x = ~Date, y = ~Cumulative, color = ~Ticker, colors = "Set1",
                   type = 'scatter', mode = 'lines', name = ~Ticker) %>%
        layout(title = "Cumulative Returns",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Cumulative Return"))
    } else if (metric == "Moving Average") {
      analysis_data <- analysis_data %>% group_by(Ticker) %>% mutate(Moving_Avg = zoo::rollmean(Close, 30, fill = NA))
      p <- plot_ly(analysis_data, x = ~Date, y = ~Moving_Avg, color = ~Ticker, colors = "Set1",
                   type = 'scatter', mode = 'lines', name = ~Ticker) %>%
        layout(title = "30-Day Moving Average",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Moving Average Price (USD)"))
    }
    
    output$analysisPlot <- renderPlotly({ p })
  })
  
  # Technical Indicators
  observeEvent(input$getIndicators, {
    req(input$indicator_ticker, input$indicator_dateRange)
    showModal(modalDialog("Calculating indicators, please wait...", footer = NULL))
    ticker <- toupper(trimws(input$indicator_ticker))
    data <- tryCatch({
      suppressWarnings(getSymbols(ticker, src = "yahoo", from = input$indicator_dateRange[1],
                                  to = input$indicator_dateRange[2], auto.assign = FALSE))
    }, error = function(e) {
      showNotification(paste("Error fetching data for symbol:", ticker), type = "error")
      return(NULL)
    })
    
    if (!is.null(data) && nrow(data) > 0) {
      data_df <- data.frame(Date = index(data), coredata(data))
      data_df <- data_df %>% select(Date, Close = contains("Close"), High = contains("High"), Low = contains("Low"), Volume = contains("Volume"))
      
      # Calculate Bollinger Bands
      data_df <- data_df %>% mutate(BBands = BBands(Close))
      
      # Calculate RSI
      data_df <- data_df %>% mutate(RSI = RSI(Close))
      
      # Plot Indicators
      p <- plot_ly(data_df, x = ~Date) %>%
        add_lines(y = ~Close, name = "Close Price", line = list(color = 'black')) %>%
        add_lines(y = ~BBands$up, name = "Upper Band", line = list(color = 'blue')) %>%
        add_lines(y = ~BBands$dn, name = "Lower Band", line = list(color = 'red')) %>%
        add_lines(y = ~BBands$mavg, name = "Moving Average", line = list(color = 'green')) %>%
        add_lines(y = ~RSI, name = "RSI", line = list(color = 'purple'), yaxis = "y2") %>%
        layout(title = "Technical Indicators",
               xaxis = list(title = "Date"),
               yaxis = list(title = "Price (USD)"),
               yaxis2 = list(title = "RSI", overlaying = "y", side = "right"))
      
      output$indicatorPlot <- renderPlotly({ p })
      showNotification("Indicators calculated successfully.", type = "message")
    } else {
      showNotification("No data available for symbol.", type = "error")
      output$indicatorPlot <- renderPlotly({ NULL })
    }
    removeModal()
  })
}

# Run the App
shinyApp(ui, server)
