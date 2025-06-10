ui <- fluidPage(
  titlePanel("Tourism Analysis in Japan"),
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
                                     numericInput("horizon_auto", "Forecast horizon (months):", value = 12, min = 1, max = 384),
                                     actionButton("run_forecast_auto", "Run Forecast", icon = icon("chart-line"))
                                     
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
                                     numericInput("horizon_arima", "Forecast horizon (months):", value = 12, min = 1, max = 384),
                                     actionButton("run_forecast_arima", "Run Forecast", icon = icon("chart-line"))
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
                                     numericInput("horizon_sarima", "Forecast horizon (months):", value = 12, min = 1, max = 384),
                                     actionButton("run_forecast_sarima", "Run Forecast", icon = icon("chart-line"))
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
