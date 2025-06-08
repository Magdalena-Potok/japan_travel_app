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

japan_dt <- readRDS("data/japan_dt.RDS")
japan_dt_all <- readRDS("data/japan_dt_all.RDS")
japan_dt[, date := as.Date(date)]
japan_dt_all[, date := as.Date(date)]

japan_dt[, `:=`(
  year = lubridate::year(as.Date(date)),
  month = lubridate::month(as.Date(date), label = TRUE, abbr = TRUE)
)]
