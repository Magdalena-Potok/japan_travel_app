library(shiny)
library(ggplot2)
library(plotly)
library(data.table)
library(lubridate)
library(forecast)
library(tseries)
library(zoo)
library(shinycssloaders)
library(scales)

# Load data
japan_dt <- readRDS("data/japan_dt.RDS")
japan_dt_all <- readRDS("data/japan_dt_all.RDS")
japan_dt[, date := as.Date(date)]
japan_dt_all[, date := as.Date(date)]
# Add year and month columns
japan_dt[, `:=`(
  year = lubridate::year(as.Date(date)),
  month = lubridate::month(as.Date(date), label = TRUE, abbr = TRUE)
)]

# UI
ui <- fluidPage(
  titlePanel("Tourism Analysis in Japan"),
  tags$style(HTML(".plot-container { position: relative; margin-bottom: 20px; }")),
  navbarPage("Data Analysis",
             
             tabPanel("Visualization",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("year_range", "Year range:",
                                      min = 1990, max = 2024,
                                      value = c(1990, 2024), step = 1, sep = ""),
                          selectInput("country", "Select country:",
                                      choices = c("All", unique(japan_dt$country)))
                        ),
                        mainPanel(
                          tabsetPanel(id = "plot_tabs",
                                      tabPanel("Time Series",
                                               withSpinner(plotlyOutput("ts_plot")),
                                               div(style = "width: 100%; display: flex; justify-content: center; align-items: center;", 
                                                   div(style = "width: auto;", tableOutput("summary_table")))
                                      ),
                                      tabPanel("Heatmap", withSpinner(plotlyOutput("heatmap_plot"))),
                                      tabPanel("Boxplot", withSpinner(plotlyOutput("boxplot_plot"))),
                                      tabPanel("Country Comparison",
                                               selectizeInput("compare_countries", "Select countries to compare:",
                                                              choices = unique(japan_dt$country),
                                                              multiple = TRUE),
                                               withSpinner(plotlyOutput("compare_plot"))
                                      )
                          )
                        )
                      )
             ),
             
             tabPanel("Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("year_range_analysis", "Year range:",
                                      min = 1990, max = 2024,
                                      value = c(1990, 2024), step = 1, sep = ""),
                          selectInput("country_analysis", "Select country:",
                                      choices = c("All", unique(japan_dt$country)))
                        ),
                        mainPanel(
                          tabsetPanel(id = "analysis_tabs",
                                      tabPanel("ACF", withSpinner(plotOutput("acf_plot"))),
                                      tabPanel("PACF", withSpinner(plotOutput("pacf_plot"))),
                                      tabPanel("Decomposition", withSpinner(plotOutput("decompose_plot"))),
                                      tabPanel("Stationarity Test",
                                               div(
                                                   p("The Augmented Dickey-Fuller (ADF) test checks whether a time series is stationary. ",
                                                     "The null hypothesis (H0) assumes the presence of a unit root (non-stationarity). ",
                                                     "A small p-value (typically < 0.05) indicates that the series is stationary."),
                                                   verbatimTextOutput("adf_test"),
                                                   textOutput("adf_interpretation")
                                               )
                                      )
                                      
                          )
                        )
                      )
             ),
             tabPanel("Forecasting",
                      tabsetPanel(
                        tabPanel("Auto ARIMA", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     sliderInput("year_range_forecast_auto", "Year range:",
                                                 min = 1990, max = 2024, value = c(2000, 2018), step = 1, sep = ""),
                                     selectInput("country_forecast_auto", "Select country:",
                                                 choices = c("All", unique(japan_dt$country))),
                                     numericInput("train_end_year_auto", "Training data ends in year:", value = 2017, min = 1990, max = 2023),
                                     numericInput("horizon_auto", "Forecast horizon (months):", value = 12, min = 1, max = 384)
                                   ),
                                   mainPanel(
                                     withSpinner(plotlyOutput("forecast_plot_auto")),
                                     div(style = "text-align: center;", h4("Selected parameters by auto.arima: SARIMA(p,d,q)(P,D,Q)[s]")),
                                     div(style = "width: 100%; display: flex; justify-content: center;",
                                         div(style = "width: auto;", tableOutput("auto_arima_params"))),
                                     div(style = "text-align: center;", h4("Forecast model evaluation metrics:")),
                                     div(style = "width: 100%; display: flex; justify-content: center; align-items: center;", 
                                         div(style = "width: auto;", tableOutput("metrics_auto"))
                                     )
                                   )
                                 )
                        ),
                        tabPanel("ARIMA",
                                 sidebarLayout(
                                   sidebarPanel(
                                     sliderInput("year_range_forecast_arima", "Year range:",
                                                 min = 1990, max = 2024, value = c(2000, 2018), step = 1, sep = ""),
                                     selectInput("country_forecast_arima", "Select country:",
                                                 choices = c("All", unique(japan_dt$country))),
                                     numericInput("p_arima", "p:", value = 1),
                                     numericInput("d_arima", "d:", value = 1),
                                     numericInput("q_arima", "q:", value = 1),
                                     numericInput("train_end_year_arima", "Training data ends in year:", value = 2017, min = 1990, max = 2023),
                                     numericInput("horizon_arima", "Forecast horizon (months):", value = 12, min = 1, max = 384)
                                   ),
                                   mainPanel(
                                     withSpinner(plotlyOutput("forecast_plot_arima")),
                                     div(style = "text-align: center;", h4("Forecast model evaluation metrics:")),
                                     div(style = "width: 100%; display: flex; justify-content: center; align-items: center;", 
                                         div(style = "width: auto;", tableOutput("metrics_arima"))
                                   ))
                                 )
                        ),
                        tabPanel("SARIMA",
                                 sidebarLayout(
                                   sidebarPanel(
                                     sliderInput("year_range_forecast_sarima", "Year range:",
                                                 min = 1990, max = 2024, value = c(2000, 2018), step = 1, sep = ""),
                                     selectInput("country_forecast_sarima", "Select country:",
                                                 choices = c("All", unique(japan_dt$country))),
                                     numericInput("p_sarima", "p:", value = 1),
                                     numericInput("d_sarima", "d:", value = 1),
                                     numericInput("q_sarima", "q:", value = 1),
                                     numericInput("P_sarima", "P (seasonal):", value = 1),
                                     numericInput("D_sarima", "D (seasonal):", value = 1),
                                     numericInput("Q_sarima", "Q (seasonal):", value = 1),
                                     numericInput("seasonality_sarima", "Seasonality period (s):", value = 12),
                                     numericInput("train_end_year_sarima", "Training data ends in year:", value = 2017, min = 1990, max = 2023),
                                     numericInput("horizon_sarima", "Forecast horizon (months):", value = 12, min = 1, max = 384)
                                   ),
                                   mainPanel(
                                     withSpinner(plotlyOutput("forecast_plot_sarima")),
                                                 div(style = "text-align: center;", h4("Forecast model evaluation metrics:")),
                                                 div(style = "width: 100%; display: flex; justify-content: center; align-items: center;", 
                                                     div(style = "width: auto;", tableOutput("metrics_sarima")))
                                     )
                                 )
                        )
                      )
             )
  )
)

# SERVER
server <- function(input, output, session) {
  
  models <- reactiveValues()
  
  filtered_data <- reactive({
    req(input$year_range)
    data <- if (input$country == "All") {
      japan_dt_all
    } else {
      japan_dt[country == input$country]
    }
    data[year(date) >= input$year_range[1] & year(date) <= input$year_range[2]]
  })
  
  filtered_data_analysis <- reactive({
    req(input$year_range_analysis)
    data <- if (input$country_analysis == "All") {
      japan_dt_all
    } else {
      japan_dt[country == input$country_analysis]
    }
    data[year(date) >= input$year_range_analysis[1] & year(date) <= input$year_range_analysis[2]]
  })
  
  filtered_compare_data <- reactive({
    req(input$compare_countries, input$year_range)
    japan_dt[country %in% input$compare_countries &
               year(date) >= input$year_range[1] &
               year(date) <= input$year_range[2]]
  })
  
  filtered_data_forecast_auto <- reactive({
    req(input$year_range_forecast_auto)
    
    data <- if (input$country_forecast_auto == "All") {
      japan_dt_all
    } else {
      japan_dt[country == input$country_forecast_auto]
    }
    
    data <- data[year(date) >= input$year_range_forecast_auto[1] &
                   year(date) <= input$year_range_forecast_auto[2]]
    
    validate(
      need(nrow(data) > 0, "Brak danych w wybranym przedziale lat.")
    )
    
    data
  })
  
  filtered_data_forecast_arima <- reactive({
    req(input$year_range_forecast_arima)
    
    data <- if (input$country_forecast_arima == "All") {
      japan_dt_all
    } else {
      japan_dt[country == input$country_forecast_arima]
    }
    
    data <- data[year(date) >= input$year_range_forecast_arima[1] &
                   year(date) <= input$year_range_forecast_arima[2]]
    
    validate(
      need(nrow(data) > 0, "Brak danych w wybranym przedziale lat.")
    )
    
    data
  })
  
  filtered_data_forecast_sarima <- reactive({
    req(input$year_range_forecast_sarima)
    
    data <- if (input$country_forecast_sarima == "All") {
      japan_dt_all
    } else {
      japan_dt[country == input$country_forecast_sarima]
    }
    
    data <- data[year(date) >= input$year_range_forecast_sarima[1] &
                   year(date) <= input$year_range_forecast_sarima[2]]
    
    validate(
      need(nrow(data) > 0, "Brak danych w wybranym przedziale lat.")
    )
    
    data
  })
  
  
  output$ts_plot <- renderPlotly({
    ggplotly(ggplot(filtered_data(), aes(x = date, y = visitors)) +
               geom_line(color = "steelblue") +
               scale_y_continuous(labels = comma) +
               theme_minimal() +
               labs(title = "Time Series", x = "Date", y = "Number of Tourists"))
  })
  
  output$summary_table <- renderTable({
    data <- filtered_data()
    
    stats <- data.table(
      `Average Visitors` = round(mean(data$visitors, na.rm = TRUE), 0),
      `Median Visitors` = round(median(data$visitors, na.rm = TRUE), 0),
      `Max Visitors` = max(data$visitors, na.rm = TRUE),
      `Min Visitors` = min(data$visitors, na.rm = TRUE)
    )
    
    stats
  })
  
  output$heatmap_plot <- renderPlotly({
    df <- filtered_data()
    df[, `:=`(year = lubridate::year(date),
              month = lubridate::month(date, label = TRUE, abbr = TRUE)
    )]
    p <- ggplot(df, aes(x = month, y = factor(year), fill = visitors)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red", labels = comma) +
      theme_minimal() +
      labs(title = "Seasonality Heatmap", x = "Month", y = "Year", fill = "Number of Tourists")
    ggplotly(p)
  })
  
  output$boxplot_plot <- renderPlotly({
    df <- filtered_data()
    df[, month := lubridate::month(date, label = TRUE, abbr = FALSE)]
    p <- ggplot(df, aes(x = month, y = visitors)) +
      geom_boxplot(fill = "lightblue") +
      scale_y_continuous(labels = comma) +
      theme_minimal() +
      labs(title = "Monthly Boxplot", x = "Month", y = "Number of Tourists")
    ggplotly(p)
  })
  
  output$compare_plot <- renderPlotly({
    df <- filtered_compare_data()
    ggplotly(
      ggplot(df, aes(x = date, y = visitors, color = country)) +
        geom_line() +
        scale_y_continuous(labels = comma) +
        theme_minimal() +
        labs(
          title = "Comparison of Tourist Arrivals by Country",
          x = "Date",
          y = "Number of Tourists",
          color = "Country"
        )
    )
  })
  
  output$acf_plot <- renderPlot({
    acf(ts(filtered_data_analysis()$visitors, frequency = 12),
        main = "Autocorrelation Function (ACF)",
        xlab = "Lag",
        ylab = "ACF")
  })
  
  output$pacf_plot <- renderPlot({
    pacf(ts(filtered_data_analysis()$visitors, frequency = 12),
         main = "Partial Autocorrelation Function (PACF)",
         xlab = "Lag",
         ylab = "PACF")
  })
  
  output$decompose_plot <- renderPlot({
    ts_data <- ts(filtered_data_analysis()$visitors, frequency = 12)
    if (length(ts_data) < 24) {
      plot.new()
      text(0.5, 0.5, "Not enough data for decomposition", cex = 1.5)
    } else {
      plot(stl(ts_data, s.window = "periodic"))
    }
  })
  
  output$adf_test <- renderPrint({
    ts_data <- ts(filtered_data_analysis()$visitors, frequency = 12)
    adf_result <- adf.test(ts_data)
    print(adf_result)
  })
  
  output$adf_interpretation <- renderText({
    ts_data <- ts(filtered_data_analysis()$visitors, frequency = 12)
    adf_result <- adf.test(ts_data)
    p_val <- adf_result$p.value
    
    if (p_val < 0.01) {
      "Result: Stationary at 1% significance level (p < 0.01). Null hypothesis rejected."
    } else if (p_val < 0.05) {
      "Result: Stationary at 5% significance level (p < 0.05). Null hypothesis rejected."
    } else if (p_val < 0.1) {
      "Result: Stationary at 10% significance level (p < 0.10). Null hypothesis rejected."
    } else {
      "Result: Non-stationary. Null hypothesis not rejected (p > 0.10)."
    }
  })
  
  output$forecast_plot_auto <- renderPlotly({
    full_data <- filtered_data_forecast_auto()
    
    train_data <- full_data[year(date) <= input$train_end_year_auto]
    test_data <- full_data[year(date) > input$train_end_year_auto]
    
    validate(
      need(nrow(train_data) > 2, "Za mało danych treningowych do modelowania.")
    )
    
    ts_train <- ts(train_data$visitors, start = c(year(min(train_data$date)), month(min(train_data$date))), frequency = 12)
    
    models$auto <- auto.arima(ts_train)
    models$fc_auto <- forecast(models$auto, h = input$horizon_auto)
    models$test_auto <- test_data
    
    forecast_df <- data.frame(
      date = seq(as.yearmon(time(models$fc_auto$mean)[1]), by = 1/12, length.out = length(models$fc_auto$mean)),
      forecast = as.numeric(models$fc_auto$mean),
      lower = as.numeric(models$fc_auto$lower[,2]),
      upper = as.numeric(models$fc_auto$upper[,2])
    )
    
    p <- ggplot() +
      geom_line(data = full_data, aes(x = date, y = visitors), color = "black") +
      geom_line(data = forecast_df, aes(x = date, y = forecast), color = "red") +
      geom_ribbon(data = forecast_df, aes(x = date, ymin = lower, ymax = upper), alpha = 0.3, fill = "pink") +
      geom_line(data = test_data, aes(x = date, y = visitors), color = "black", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Forecast: Auto ARIMA", x = "Date", y = "Visitors") +
      scale_x_yearmon(format = "%b %Y") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$auto_arima_params <- renderTable({
    req(models$auto)
    
    params <- data.frame(
      p = models$auto$arma[1],
      d = models$auto$arma[6],
      q = models$auto$arma[2],
      P = models$auto$arma[3],
      D = models$auto$arma[7],
      Q = models$auto$arma[4],
      S = models$auto$arma[5]
    )
    params
  })
  
  
  output$metrics_auto <- renderTable({
    req(models$auto, models$fc_auto, models$test_auto)
    
    n <- min(length(models$fc_auto$mean), nrow(models$test_auto))
    
    actual <- models$test_auto$visitors[1:n]
    forecast <- models$fc_auto$mean[1:n]
    
    mae <- mean(abs(forecast - actual), na.rm = TRUE)
    rmse <- sqrt(mean((forecast - actual)^2, na.rm = TRUE))
    mape <- mean(abs((forecast - actual) / actual), na.rm = TRUE) * 100
    
    df <- data.frame(
      Metric = c("AIC", "MAE", "RMSE", "MAPE (%)"),
      Value = round(c(models$auto$aic, mae, rmse, mape), 2)
    )
    
    df_t <- as.data.frame(t(df$Value))
    colnames(df_t) <- df$Metric
    rownames(df_t) <- NULL
    
    df_t
  })
  
  
  output$forecast_plot_arima <- renderPlotly({
    full_data <- filtered_data_forecast_arima()
    
    train_data <- full_data[year(date) <= input$train_end_year_arima]
    test_data <- full_data[year(date) > input$train_end_year_arima]
    
    validate(
      need(nrow(train_data) > 2, "Za mało danych treningowych do modelowania.")
    )
    
    ts_train <- ts(train_data$visitors, start = c(year(min(train_data$date)), month(min(train_data$date))), frequency = 12)
    
    models$arima <- Arima(ts_train, order = c(input$p_arima, input$d_arima, input$q_arima))
    models$fc_arima <- forecast(models$arima, h = input$horizon_arima)
    models$test_arima <- test_data
    
    forecast_df <- data.frame(
      date = seq(as.yearmon(time(models$fc_arima$mean)[1]), by = 1/12, length.out = length(models$fc_arima$mean)),
      forecast = as.numeric(models$fc_arima$mean),
      lower = as.numeric(models$fc_arima$lower[,2]),
      upper = as.numeric(models$fc_arima$upper[,2])
    )
    
    p <- ggplot() +
      geom_line(data = full_data, aes(x = date, y = visitors), color = "black") +
      geom_line(data = forecast_df, aes(x = date, y = forecast), color = "red") +
      geom_ribbon(data = forecast_df, aes(x = date, ymin = lower, ymax = upper), alpha = 0.3, fill = "pink") +
      geom_line(data = test_data, aes(x = date, y = visitors), color = "black", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Forecast: ARIMA", x = "Date", y = "Visitors") +
      scale_x_yearmon(format = "%b %Y") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$metrics_arima <- renderTable({
    req(models$arima, models$fc_arima, models$test_arima)
    
    n <- min(length(models$fc_arima$mean), nrow(models$test_arima))
    
    actual <- models$test_arima$visitors[1:n]
    forecast <- models$fc_arima$mean[1:n]
    
    mae <- mean(abs(forecast - actual), na.rm = TRUE)
    rmse <- sqrt(mean((forecast - actual)^2, na.rm = TRUE))
    mape <- mean(abs((forecast - actual) / actual), na.rm = TRUE) * 100
    
    df <- data.frame(
      Metric = c("AIC", "MAE", "RMSE", "MAPE (%)"),
      Value = round(c(models$arima$aic, mae, rmse, mape), 2)
    )
    
    df_t <- as.data.frame(t(df$Value))
    colnames(df_t) <- df$Metric
    rownames(df_t) <- NULL
    
    df_t
  })
  
  
  output$forecast_plot_sarima <- renderPlotly({
    full_data <- filtered_data_forecast_sarima()
    
    train_data <- full_data[year(date) <= input$train_end_year_sarima]
    test_data <- full_data[year(date) > input$train_end_year_sarima]
    
    validate(
      need(nrow(train_data) > 2, "Za mało danych treningowych do modelowania.")
    )
    
    ts_train <- ts(train_data$visitors, start = c(year(min(train_data$date)), month(min(train_data$date))), frequency = 12)
    
    models$sarima <- Arima(
      ts_train, 
      order = c(input$p_sarima, input$d_sarima, input$q_sarima),
      seasonal = list(order = c(input$P_sarima, input$D_sarima, input$Q_sarima), period = input$seasonality_sarima)
    )
    models$fc_sarima <- forecast(models$sarima, h = input$horizon_sarima)
    models$test_sarima <- test_data
    
    forecast_df <- data.frame(
      date = seq(as.yearmon(time(models$fc_sarima$mean)[1]), by = 1/12, length.out = length(models$fc_sarima$mean)),
      forecast = as.numeric(models$fc_sarima$mean),
      lower = as.numeric(models$fc_sarima$lower[,2]),
      upper = as.numeric(models$fc_sarima$upper[,2])
    )
    
    p <- ggplot() +
      geom_line(data = full_data, aes(x = date, y = visitors), color = "black") +
      geom_line(data = forecast_df, aes(x = date, y = forecast), color = "red") +
      geom_ribbon(data = forecast_df, aes(x = date, ymin = lower, ymax = upper), alpha = 0.3, fill = "pink") +
      geom_line(data = test_data, aes(x = date, y = visitors), color = "black", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Forecast: SARIMA", x = "Date", y = "Visitors") +
      scale_x_yearmon(format = "%b %Y") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$metrics_sarima <- renderTable({
    req(models$sarima, models$fc_sarima, models$test_sarima)
    
    n <- min(length(models$fc_sarima$mean), nrow(models$test_sarima))
    
    actual <- models$test_sarima$visitors[1:n]
    forecast <- models$fc_sarima$mean[1:n]
    
    mae <- mean(abs(forecast - actual), na.rm = TRUE)
    rmse <- sqrt(mean((forecast - actual)^2, na.rm = TRUE))
    mape <- mean(abs((forecast - actual) / actual), na.rm = TRUE) * 100
    
    df <- data.frame(
      Metric = c("AIC", "MAE", "RMSE", "MAPE (%)"),
      Value = round(c(models$sarima$aic, mae, rmse, mape), 2)
    )
    
    df_t <- as.data.frame(t(df$Value))
    colnames(df_t) <- df$Metric
    rownames(df_t) <- NULL
    
    df_t
  })
  
  
}

# Run the app
shinyApp(ui, server)
