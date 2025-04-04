library(shiny)
library(ggplot2)
library(plotly)
library(data.table)
library(lubridate)
library(forecast)

# Wczytanie danych
japan_dt <- readRDS("~/Desktop/Statistical Learning/Statistical learning/japan_dt.RDS")

# Dodanie kolumny rok i miesiąc dla heatmapy
japan_dt[, `:=`(year = year(date), month = month(date, label = TRUE, abbr = TRUE))]

# UI
ui <- fluidPage(
  titlePanel("Analiza turystyki w Japonii"),
  navbarPage("Analiza danych",
             tabPanel("Wizualizacja",
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("year_range", "Zakres lat:",
                                      min = min(japan_dt$year), max = max(japan_dt$year),
                                      value = c(min(japan_dt$year), max(japan_dt$year)),
                                      step = 1, sep = ""),
                          selectInput("plot_type", "Wybierz typ wykresu:",
                                      choices = c("Szereg czasowy" = "ts_plot", "Heatmapa sezonowości" = "heatmap"))
                        ),
                        mainPanel(
                          plotlyOutput("main_plot"),
                          tableOutput("stats_table")
                        )
                      )
             ),
             tabPanel("Prognozowanie",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("model_type", "Wybierz model:",
                                      choices = c("Auto ARIMA" = "auto_arima", "SARIMA" = "sarima")),
                          numericInput("p", "Parametr p:", value = 1, min = 0, max = 5),
                          numericInput("d", "Parametr d:", value = 1, min = 0, max = 2),
                          numericInput("q", "Parametr q:", value = 1, min = 0, max = 5),
                          numericInput("h", "Horyzont prognozy:", value = 12, min = 1, max = 60)
                        ),
                        mainPanel(
                          plotlyOutput("forecast_plot")
                        )
                      )
             )
  )
)

# SERVER
server <- function(input, output) {
  
  filtered_data <- reactive({
    japan_dt[year >= input$year_range[1] & year <= input$year_range[2]]
  })
  
  output$main_plot <- renderPlotly({
    data <- filtered_data()
    
    if (input$plot_type == "ts_plot") {
      p <- ggplot(data, aes(x = date, y = visitors)) +
        geom_line(color = "steelblue") +
        labs(title = "Liczba turystów w Japonii", x = "Data", y = "Liczba turystów") +
        theme_minimal()
      ggplotly(p)
      
    } else if (input$plot_type == "heatmap") {
      p <- ggplot(data, aes(x = month, y = factor(year), fill = visitors)) +
        geom_tile() +
        scale_fill_gradient(low = "lightblue", high = "red") +
        labs(title = "Sezonowość turystyki w Japonii", x = "Miesiąc", y = "Rok", fill = "Liczba turystów") +
        theme_minimal()
      ggplotly(p)
    }
  })
  
  output$stats_table <- renderTable({
    data <- filtered_data()
    summary_stats <- data.table(
      "Średnia liczba turystów" = mean(data$visitors, na.rm = TRUE),
      "Mediana" = median(data$visitors, na.rm = TRUE),
      "Maksimum" = max(data$visitors, na.rm = TRUE),
      "Minimum" = min(data$visitors, na.rm = TRUE)
    )
    summary_stats
  })
  
  output$forecast_plot <- renderPlotly({
    ts_data <- ts(japan_dt$visitors, start = c(min(japan_dt$year), 1), frequency = 12)
    
    if (input$model_type == "auto_arima") {
      model <- auto.arima(ts_data)
    } else {
      model <- Arima(ts_data, order = c(input$p, input$d, input$q))
    }
    
    forecasted <- forecast(model, h = input$h)
    
    p <- autoplot(forecasted) + theme_minimal() + labs(title = "Prognoza liczby turystów")
    ggplotly(p)
  })
}

# Uruchomienie aplikacji
shinyApp(ui, server)