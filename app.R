library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(lubridate)

safe_load_data <- function() {
  data_path <- file.path(getwd(), "data", "stock_data.csv")

  if (!file.exists(data_path)) {
    if (file.exists(file.path(getwd(), "generate_data.R"))) {
      tryCatch({
        source(file.path(getwd(), "generate_data.R"), local = FALSE)
      }, error = function(err) {
        warning("Unable to generate stock dataset: ", err$message)
      })
    }
  }

  if (!file.exists(data_path)) {
    return(tibble())
  }

  df <- read.csv(data_path, stringsAsFactors = FALSE)
  if (nrow(df) == 0) {
    return(df)
  }

  df$Date <- as.Date(df$Date)
  df$Open <- as.numeric(df$Open)
  df$High <- as.numeric(df$High)
  df$Low <- as.numeric(df$Low)
  df$Close <- as.numeric(df$Close)
  df$Volume <- as.numeric(df$Volume)
  df$Daily_Return <- as.numeric(df$Daily_Return)
  df$Market_Cap <- as.numeric(df$Market_Cap)

  df
}

stock_data <- safe_load_data()
all_stocks <- sort(unique(stock_data$Stock))
if (length(all_stocks) == 0) {
  all_stocks <- c("TCS", "INFY", "RELIANCE", "HDFCBANK", "ITC")
}

if (nrow(stock_data) == 0) {
  fallback_date_min <- as.Date("2022-01-03")
  fallback_date_max <- as.Date("2024-01-31")
} else {
  fallback_date_min <- min(stock_data$Date, na.rm = TRUE)
  fallback_date_max <- max(stock_data$Date, na.rm = TRUE)
}

format_currency <- function(x, digits = 2) {
  if (length(x) == 0 || is.na(x)) return("?0")
  paste0("?", formatC(x, format = "f", big.mark = ",", digits = digits, drop0trailing = FALSE))
}

format_percentage <- function(x, digits = 2) {
  if (length(x) == 0 || is.na(x)) return("0.00%")
  paste0(round(x, digits), "%")
}

market_trend_label <- function(avg_return) {
  if (is.na(avg_return) || length(avg_return) == 0) {
    return("Neutral")
  }
  if (avg_return > 0.0025) return("Bullish")
  if (avg_return < -0.0025) return("Bearish")
  "Neutral"
}

base_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1d4ed8",
  secondary = "#0f172a",
  success = "#16a34a",
  warning = "#f59e0b",
  danger = "#ef4444"
)

kpi_card <- function(icon, label, value, accent = "#2563eb") {
  div(
    class = "kpi-card",
    div(
      style = "display:flex; align-items:center; justify-content:space-between;",
      div(class = "kpi-icon", style = paste0("background: rgba(37,99,235,0.08); color: ", accent, ";"), icon)
    ),
    div(class = "kpi-label", label),
    div(class = "kpi-value", value)
  )
}

dashboard_ui <- div(
  class = "page-shell",
  tags$h1("Stock Market Analytics", class = "main-title"),
  tags$p("Interactive Market Intelligence & Forecasting Dashboard", class = "subtitle"),
  tags$p(
    "Explore stock price trends, performance, volatility, trading volume and simplified forecasts using interactive analytics.",
    class = "lead-text"
  ),
  div(
    class = "filter-bar",
    div(class = "form-group", selectInput("dashboard_stock", "Stock", choices = c("All", all_stocks), selected = "All")),
    div(class = "form-group", dateRangeInput("dashboard_dates", "Date Range", start = fallback_date_min, end = fallback_date_max, min = fallback_date_min, max = fallback_date_max))
  ),
  fluidRow(
    column(3, kpi_card("??", "Total Stocks", length(all_stocks))),
    column(3, uiOutput("latestMarketValueCard")),
    column(3, uiOutput("bestPerformerCard")),
    column(3, uiOutput("avgDailyReturnCard"))
  ),
  br(),
  fluidRow(column(12, uiOutput("marketTrendCard"))),
  br(),
  fluidRow(
    column(8, card(full_screen = TRUE, card_header("Stock Price Trend"), plotlyOutput("dashboardPriceTrend"))),
    column(4, card(full_screen = TRUE, card_header("Trading Volume"), plotlyOutput("dashboardVolumePlot")))
  ),
  br(),
  fluidRow(
    column(6, card(full_screen = TRUE, card_header("Stock Performance Comparison"), plotlyOutput("dashboardPerformanceBar"))),
    column(6, card(full_screen = TRUE, card_header("Market Distribution"), plotlyOutput("dashboardMarketDistribution")))
  )
)

explorer_ui <- div(
  class = "page-shell",
  tags$h2("Stock Explorer", class = "section-title"),
  div(
    class = "filter-bar",
    div(class = "form-group", selectInput("explorer_stock", "Stock Selector", choices = all_stocks, selected = all_stocks[1])),
    div(class = "form-group", dateRangeInput("explorer_dates", "Date Range", start = fallback_date_min, end = fallback_date_max, min = fallback_date_min, max = fallback_date_max))
  ),
  fluidRow(
    column(2, uiOutput("openingPriceCard")),
    column(2, uiOutput("highestPriceCard")),
    column(2, uiOutput("lowestPriceCard")),
    column(2, uiOutput("latestPriceCard")),
    column(2, uiOutput("totalReturnCard")),
    column(2, uiOutput("avgVolumeCard"))
  ),
  br(),
  card(full_screen = TRUE, card_header("Stock Price Overview"), plotlyOutput("explorerCandlestick"))
)

performance_ui <- div(
  class = "page-shell",
  tags$h2("Performance Analysis", class = "section-title"),
  fluidRow(column(12, card(full_screen = TRUE, card_header("Returns Comparison"), DTOutput("returnsTable")))),
  br(),
  fluidRow(
    column(6, card(full_screen = TRUE, card_header("Risk vs Return"), plotlyOutput("riskReturnPlot"))),
    column(6, card(full_screen = TRUE, card_header("Volatility Comparison"), plotlyOutput("volatilityPlot")))
  )
)

forecast_ui <- div(
  class = "page-shell",
  tags$h2("Forecast", class = "section-title"),
  div(
    class = "filter-bar",
    div(class = "form-group", selectInput("forecast_stock", "Stock", choices = all_stocks, selected = all_stocks[1])),
    div(class = "form-group", selectInput("forecast_days", "Forecast Days", choices = c(7, 14, 30), selected = 7))
  ),
  card(full_screen = TRUE, card_header("Historical vs Forecast"), plotlyOutput("forecastPlot")),
  br(),
  fluidRow(
    column(3, uiOutput("forecastCurrentCard")),
    column(3, uiOutput("forecastPriceCard")),
    column(3, uiOutput("forecastChangeCard")),
    column(3, uiOutput("forecastTrendCard"))
  ),
  div(
    class = "forecast-disclaimer",
    HTML("Forecasts shown in this project are educational demonstrations based on simplified statistical methods and synthetic data. They are not financial advice and should not be used for actual investment decisions.")
  )
)

dataset_ui <- div(
  class = "page-shell",
  tags$h2("Dataset", class = "section-title"),
  div(class = "filter-bar", div(class = "form-group", selectInput("dataset_stock", "Stock", choices = c("All", all_stocks), selected = "All"))),
  card(full_screen = TRUE, card_header("Market Data Table"), DTOutput("datasetTable")),
  br(),
  downloadButton("downloadDataset", "Download CSV", class = "btn btn-primary")
)

about_ui <- div(
  class = "page-shell",
  tags$h2("About", class = "section-title"),
  div(
    class = "about-card",
    tags$h3("Project Overview"),
    tags$p("This project demonstrates how R can be used for financial data analysis, stock performance visualization, risk analysis, return analysis, interactive dashboards and basic forecasting."),
    tags$h4("Technologies"),
    tags$div(
      class = "tech-stack",
      tags$span(class = "tech-badge", "R"),
      tags$span(class = "tech-badge", "Shiny"),
      tags$span(class = "tech-badge", "Plotly"),
      tags$span(class = "tech-badge", "ggplot2"),
      tags$span(class = "tech-badge", "dplyr"),
      tags$span(class = "tech-badge", "DT"),
      tags$span(class = "tech-badge", "bslib")
    ),
    br(),
    tags$h4("Important Note"),
    tags$p("The project uses synthetic data and simplified forecasting for educational purposes only."),
    tags$p("It is designed to help students understand how dashboards, analytics, and market visuals can be built in R without relying on live APIs or real financial data.")
  )
)

ui <- page_navbar(
  title = "Stock Market Analytics",
  theme = base_theme,
  nav_panel("Dashboard", dashboard_ui),
  nav_panel("Stock Explorer", explorer_ui),
  nav_panel("Performance Analysis", performance_ui),
  nav_panel("Forecast", forecast_ui),
  nav_panel("Dataset", dataset_ui),
  nav_panel("About", about_ui),
  header = tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css"))
)

server <- function(input, output, session) {
  get_filtered_data <- function(stock_input, date_input, df = stock_data) {
    if (nrow(df) == 0) {
      return(df)
    }

    result <- df
    if (!is.null(stock_input) && !identical(stock_input, "All") && length(stock_input) > 0) {
      result <- result %>% filter(Stock == stock_input)
    }

    if (!is.null(date_input) && length(date_input) == 2 && !any(is.na(date_input))) {
      result <- result %>% filter(Date >= date_input[1], Date <= date_input[2])
    }

    result
  }

  stock_summary <- reactive({
    df <- get_filtered_data(input$dashboard_stock, input$dashboard_dates)
    if (nrow(df) == 0) {
      return(tibble())
    }

    df %>%
      group_by(Stock) %>%
      summarise(
        total_return = if (first(Close) == 0) 0 else ((last(Close) - first(Close)) / first(Close)) * 100,
        avg_daily_return = mean(Daily_Return, na.rm = TRUE),
        .groups = "drop"
      )
  })

  output$latestMarketValueCard <- renderUI({
    df <- get_filtered_data(input$dashboard_stock, input$dashboard_dates)
    if (nrow(df) == 0) {
      return(kpi_card("??", "Latest Market Value", "?0"))
    }
    latest_df <- df %>%
      group_by(Stock) %>%
      arrange(Date) %>%
      slice_tail(n = 1) %>%
      summarise(value = sum(Market_Cap, na.rm = TRUE), .groups = "drop")
    total_value <- sum(latest_df$value, na.rm = TRUE)
    kpi_card("??", "Latest Market Value", format_currency(total_value))
  })

  output$bestPerformerCard <- renderUI({
    summary <- stock_summary()
    if (nrow(summary) == 0) {
      return(kpi_card("??", "Best Performing Stock", "-"))
    }

    best_stock <- summary %>% arrange(desc(total_return)) %>% slice(1)
    kpi_card("??", "Best Performing Stock", paste0(best_stock$Stock[1], " (", format_percentage(best_stock$total_return[1]), ")"), "#16a34a")
  })

  output$avgDailyReturnCard <- renderUI({
    df <- get_filtered_data(input$dashboard_stock, input$dashboard_dates)
    if (nrow(df) == 0) {
      return(kpi_card("??", "Average Daily Return", "0.00%"))
    }
    avg_return <- mean(df$Daily_Return, na.rm = TRUE) * 100
    kpi_card("??", "Average Daily Return", format_percentage(avg_return, 2), if (avg_return >= 0) "#16a34a" else "#ef4444")
  })

  output$marketTrendCard <- renderUI({
    df <- get_filtered_data(input$dashboard_stock, input$dashboard_dates)
    if (nrow(df) == 0) {
      return(div(class = "card-panel", tags$h4("Market Trend", style = "margin-bottom:0.5rem;"), tags$p("Neutral")))
    }

    trend_value <- mean(df$Daily_Return, na.rm = TRUE)
    label <- market_trend_label(trend_value)
    color <- if (label == "Bullish") "#16a34a" else if (label == "Bearish") "#ef4444" else "#64748b"
    div(
      class = "card-panel",
      style = paste0("background: linear-gradient(135deg, rgba(255,255,255,0.9), rgba(248,250,252,0.9)); border-left: 6px solid ", color, ";"),
      tags$h4("Market Trend", style = "margin-bottom:0.5rem; color:#0f172a;"),
      tags$div(style = paste0("font-size:1.4rem; font-weight:700; color:", color, ";"), label)
    )
  })

  output$dashboardPriceTrend <- renderPlotly({
    df <- get_filtered_data(input$dashboard_stock, input$dashboard_dates)
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }

    plot_ly(df, x = ~Date, y = ~Close, color = ~Stock, type = "scatter", mode = "lines", line = list(width = 3), hovertemplate = "%{x|%b %d, %Y}<br>?%{y:,.2f}<extra>%{fullData.name}</extra>") %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Close Price"),
        template = "plotly_white",
        legend = list(title = list(text = "Stock")),
        hovermode = "x unified"
      )
  })

  output$dashboardVolumePlot <- renderPlotly({
    df <- get_filtered_data(input$dashboard_stock, input$dashboard_dates)
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }

    volume_df <- df %>%
      group_by(Date) %>%
      summarise(Volume = sum(Volume, na.rm = TRUE), .groups = "drop")

    plot_ly(volume_df, x = ~Date, y = ~Volume, type = "bar", marker = list(color = "#60a5fa"), hovertemplate = "%{x|%b %d, %Y}<br>Volume: %{y:,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "Volume"), template = "plotly_white")
  })

  output$dashboardPerformanceBar <- renderPlotly({
    df <- get_filtered_data(input$dashboard_stock, input$dashboard_dates)
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }

    performance_df <- df %>%
      group_by(Stock) %>%
      summarise(
        return_pct = if (first(Close) == 0) 0 else ((last(Close) - first(Close)) / first(Close)) * 100,
        .groups = "drop"
      ) %>%
      arrange(desc(return_pct))

    plot_ly(performance_df, x = ~reorder(Stock, return_pct), y = ~return_pct, type = "bar", marker = list(color = ~ifelse(return_pct >= 0, "#22c55e", "#f87171")), hovertemplate = "%{x}<br>%{y:.2f}%<extra></extra>") %>%
      layout(xaxis = list(title = "Stock"), yaxis = list(title = "Total Return (%)"), template = "plotly_white", margin = list(b = 100)) %>%
      add_annotations(
        text = ~paste0(round(return_pct, 1), "%"),
        x = ~reorder(Stock, return_pct),
        y = ~return_pct,
        showarrow = FALSE,
        yshift = 10
      )
  })

  output$dashboardMarketDistribution <- renderPlotly({
    df <- get_filtered_data(input$dashboard_stock, input$dashboard_dates)
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }

    market_distribution <- df %>%
      group_by(Stock) %>%
      summarise(volume = sum(Volume, na.rm = TRUE), .groups = "drop")

    plot_ly(market_distribution, labels = ~Stock, values = ~volume, type = "pie", hole = 0.45, textinfo = "label+percent", hoverinfo = "label+value") %>%
      layout(template = "plotly_white")
  })

  explorer_metric_values <- reactive({
    df <- get_filtered_data(input$explorer_stock, input$explorer_dates)
    if (nrow(df) == 0) {
      return(NULL)
    }

    list(
      opening = first(df$Open),
      highest = max(df$High),
      lowest = min(df$Low),
      latest = last(df$Close),
      total_return = if (first(df$Close) == 0) 0 else ((last(df$Close) - first(df$Close)) / first(df$Close)) * 100,
      avg_volume = mean(df$Volume, na.rm = TRUE)
    )
  })

  output$openingPriceCard <- renderUI({
    metrics <- explorer_metric_values()
    if (is.null(metrics)) return(kpi_card("??", "Opening Price", "?0"))
    kpi_card("??", "Opening Price", format_currency(metrics$opening))
  })

  output$highestPriceCard <- renderUI({
    metrics <- explorer_metric_values()
    if (is.null(metrics)) return(kpi_card("??", "Highest Price", "?0"))
    kpi_card("??", "Highest Price", format_currency(metrics$highest))
  })

  output$lowestPriceCard <- renderUI({
    metrics <- explorer_metric_values()
    if (is.null(metrics)) return(kpi_card("??", "Lowest Price", "?0"))
    kpi_card("??", "Lowest Price", format_currency(metrics$lowest))
  })

  output$latestPriceCard <- renderUI({
    metrics <- explorer_metric_values()
    if (is.null(metrics)) return(kpi_card("??", "Latest Price", "?0"))
    kpi_card("??", "Latest Price", format_currency(metrics$latest))
  })

  output$totalReturnCard <- renderUI({
    metrics <- explorer_metric_values()
    if (is.null(metrics)) return(kpi_card("??", "Total Return", "0.00%"))
    kpi_card("??", "Total Return", format_percentage(metrics$total_return), if (metrics$total_return >= 0) "#16a34a" else "#ef4444")
  })

  output$avgVolumeCard <- renderUI({
    metrics <- explorer_metric_values()
    if (is.null(metrics)) return(kpi_card("??", "Average Volume", "0"))
    kpi_card("??", "Average Volume", format(as.integer(metrics$avg_volume), big.mark = ","))
  })

  output$explorerCandlestick <- renderPlotly({
    df <- get_filtered_data(input$explorer_stock, input$explorer_dates)
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }

    plot_ly(
      df,
      x = ~Date,
      open = ~Open,
      high = ~High,
      low = ~Low,
      close = ~Close,
      type = "candlestick",
      increasing = list(line = list(color = "#16a34a")),
      decreasing = list(line = list(color = "#ef4444")),
      hoverlabel = list(bgcolor = "#0f172a"),
      hovertemplate = "%{x|%b %d, %Y}<br>Open: ?%{open}<br>High: ?%{high}<br>Low: ?%{low}<br>Close: ?%{close}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Price (?)"),
        template = "plotly_white",
        dragmode = "zoom",
        hovermode = "x unified"
      )
  })

  output$returnsTable <- renderDT({
    df <- get_filtered_data(input$dashboard_stock, input$dashboard_dates)
    if (nrow(df) == 0) {
      return(datatable(tibble(Stock = character(), Total_Return = numeric(), Average_Daily_Return = numeric(), Best_Day = numeric(), Worst_Day = numeric())))
    }

    summary_df <- df %>%
      group_by(Stock) %>%
      summarise(
        Total_Return = if (first(Close) == 0) 0 else ((last(Close) - first(Close)) / first(Close)) * 100,
        Average_Daily_Return = mean(Daily_Return, na.rm = TRUE) * 100,
        Best_Day = max(Daily_Return, na.rm = TRUE) * 100,
        Worst_Day = min(Daily_Return, na.rm = TRUE) * 100,
        .groups = "drop"
      )

    datatable(summary_df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(columns = c("Total_Return", "Average_Daily_Return", "Best_Day", "Worst_Day"), digits = 2)
  })

  output$riskReturnPlot <- renderPlotly({
    df <- get_filtered_data(input$dashboard_stock, input$dashboard_dates)
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }

    risk_df <- df %>%
      group_by(Stock) %>%
      summarise(
        Risk = sd(Daily_Return, na.rm = TRUE) * 100,
        Avg_Return = mean(Daily_Return, na.rm = TRUE) * 100,
        .groups = "drop"
      )

    plot_ly(risk_df, x = ~Risk, y = ~Avg_Return, type = "scatter", mode = "markers+text", text = ~Stock, textposition = "top center", marker = list(size = 14, color = "#2563eb", opacity = 0.8), hovertemplate = "%{text}<br>Risk: %{x:.2f}%<br>Avg Return: %{y:.2f}%<extra></extra>") %>%
      layout(xaxis = list(title = "Risk / Volatility (%)"), yaxis = list(title = "Average Return (%)"), template = "plotly_white")
  })

  output$volatilityPlot <- renderPlotly({
    df <- get_filtered_data(input$dashboard_stock, input$dashboard_dates)
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available"))
    }

    vol_df <- df %>%
      group_by(Stock) %>%
      summarise(Volatility = sd(Daily_Return, na.rm = TRUE) * 100, .groups = "drop")

    plot_ly(vol_df, x = ~reorder(Stock, Volatility), y = ~Volatility, type = "bar", marker = list(color = "#8b5cf6"), hovertemplate = "%{x}<br>Volatility: %{y:.2f}%<extra></extra>") %>%
      layout(xaxis = list(title = "Stock"), yaxis = list(title = "Volatility (%)"), template = "plotly_white")
  })

  forecast_df <- reactive({
    df <- stock_data %>% filter(Stock == input$forecast_stock)
    if (nrow(df) == 0) {
      return(tibble())
    }
    df %>% arrange(Date)
  })

  output$forecastPlot <- renderPlotly({
    df <- forecast_df()
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No forecast data available"))
    }

    model <- lm(Close ~ seq_along(Date), data = df)
    future_index <- (nrow(df) + 1):(nrow(df) + as.integer(input$forecast_days))
    future_dates <- seq(max(df$Date) + 1, by = "day", length.out = as.integer(input$forecast_days))
    future_values <- predict(model, newdata = data.frame(seq_along = future_index))
    future_data <- data.frame(Date = future_dates, Close = future_values)

    plot_ly() %>%
      add_lines(data = df, x = ~Date, y = ~Close, name = "Historical Data", line = list(color = "#2563eb", width = 4)) %>%
      add_lines(data = future_data, x = ~Date, y = ~Close, name = "Forecast", line = list(color = "#f59e0b", width = 4, dash = "dash")) %>%
      layout(xaxis = list(title = "Date"), yaxis = list(title = "Price (?)"), template = "plotly_white", legend = list(title = list(text = "Series")))
  })

  forecast_metrics <- reactive({
    df <- forecast_df()
    if (nrow(df) == 0) {
      return(list(current = 0, forecast = 0, change = 0, trend = "Neutral"))
    }

    current_price <- last(df$Close)
    model <- lm(Close ~ seq_along(Date), data = df)
    future_index <- nrow(df) + as.integer(input$forecast_days)
    forecast_price <- predict(model, newdata = data.frame(seq_along = future_index))
    change_pct <- ((forecast_price - current_price) / current_price) * 100
    trend <- if (change_pct > 0) "Positive" else if (change_pct < 0) "Negative" else "Neutral"

    list(current = current_price, forecast = forecast_price, change = change_pct, trend = trend)
  })

  output$forecastCurrentCard <- renderUI({
    metrics <- forecast_metrics()
    kpi_card("??", "Current Price", format_currency(metrics$current))
  })

  output$forecastPriceCard <- renderUI({
    metrics <- forecast_metrics()
    kpi_card("??", "Forecast Price", format_currency(metrics$forecast))
  })

  output$forecastChangeCard <- renderUI({
    metrics <- forecast_metrics()
    kpi_card("??", "Expected Change", format_percentage(metrics$change), if (metrics$change >= 0) "#16a34a" else "#ef4444")
  })

  output$forecastTrendCard <- renderUI({
    metrics <- forecast_metrics()
    kpi_card("??", "Trend", metrics$trend, if (metrics$trend == "Positive") "#16a34a" else if (metrics$trend == "Negative") "#ef4444" else "#64748b")
  })

  output$datasetTable <- renderDT({
    df <- get_filtered_data(input$dataset_stock, NULL)
    if (nrow(df) == 0) {
      return(datatable(tibble(Date = as.Date(character()), Stock = character(), Open = numeric(), High = numeric(), Low = numeric(), Close = numeric(), Volume = numeric(), Daily_Return = numeric(), Market_Cap = numeric())))
    }

    datatable(df, options = list(pageLength = 10, scrollX = TRUE), filter = "top", rownames = FALSE) %>%
      formatRound(columns = c("Open", "High", "Low", "Close", "Daily_Return", "Market_Cap"), digits = 2)
  })

  output$downloadDataset <- downloadHandler(
    filename = function() {
      paste0("stock_market_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- get_filtered_data(input$dataset_stock, NULL)
      write.csv(df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
