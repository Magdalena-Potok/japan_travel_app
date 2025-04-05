# dane to importowana tabela z pliku CSV ze strony
# Japan Tourism Statistics

# Przyjazdy odwiedzających do Japonii są obliczane na podstawie liczby podróżnych o obcym obywatelstwie wjeżdżających do Japonii, dostarczonej przez Ministerstwo Sprawiedliwości.
# Liczby te nie obejmują stałych rezydentów mających Japonię jako swoje główne miejsce zamieszkania i obejmują podróżnych przyjeżdżających do Japonii w celu tranzytu.
# Cudzoziemcy wjeżdżający lub ponownie wjeżdżający do Japonii, tacy jak emigranci i ich rodziny oraz studenci zagraniczni, są wliczani do przyjazdów odwiedzających do Japonii

# Dane za lata 1990–2023 są ostateczne, natomiast dane za okres styczeń 2024 – grudzień 2024 są tymczasowe (prowizoryczne)

# unknown to miejsce na dane nieprzypisane do konkretnego kraju

library(readr)
library(data.table)
library(zoo)
library(plotly)
raw_data <- read_csv("data/raw_data.csv")
dim(raw_data)
japan_dt = as.data.table(raw_data)

# nie będziemy się zajmować growth.rate, jest tam też dużo braków w danych
colnames(japan_dt)
japan_dt[,`Growth Rate(%)` := NULL]
colnames(japan_dt) <- c("country", "month", "year", "visitors")
japan_dt[, month := substr(month,1,3)]
japan_dt

japan_dt[, visitors := as.numeric(visitors)]

#usuwanie braków danych
japan_dt = japan_dt[visitors >= 0]
# zamiast month i yaer osobno, zrobimy date
japan_dt[, date := paste(month, year, sep = " ")]
japan_dt[, date := as.yearmon(date, format = "%b %Y")]
setorder(japan_dt, date)
japan_dt[, month := NULL]
japan_dt[, year := NULL]
japan_dt_all = japan_dt[, .(visitors = sum(visitors, na.rm = TRUE)), by = date]

japan_dt_all # tutaj beda zagregowana suma wszystkich wizytorow przez date
japan_dt # tutaj wizytorzy z roznych kraji (mozna zrobic taka analize zeby sprawdzac szereg czasowy dla konkratnego kraju)

# sprawdzenie czy sa wszystkie daty:
# uniqueN(japan_dt_all$date) = (2024 - 1990 + 1)*12 # zgadza sie!


saveRDS(japan_dt_all, file = "data/japan_dt_all.RDS")
saveRDS(japan_dt, file = "data/japan_dt.RDS")

# zmiana w szereg czasowy, dziala tylko dla japan_dt_all gdzie sa zagregowani wizytatorzy - dlatego ze potrzeba 
# unikalnych wartosci date, a jak mamy duzo panstw, no to sie nie da - wniosek: jak chcemy modelowac, to na tych 
# danych co da sie zrobic zoo/ts, więc trzeba w apce Rshiny zrobic filtrowanie tego szeregu mniejwiecej tak jak ponizej
# tylko trzeba dodac filtered_data <- reactive({ japan_dt[country == input$country] }) i potem to zoo i ts
japan_zoo_all = zoo(japan_dt_all$visitors, japan_dt_all$date)
japan_ts_all = ts(japan_zoo_all, start = 1990, frequency = 12)
saveRDS(japan_ts_all, file = "data/japan_ts_all.RDS")
length(japan_zoo_all)
length(japan_ts_all)
# tutaj jakieś ploty do obczajki
plot(japan_ts_all)
plot_ly(japan_dt_all, x = ~date, y = ~visitors, type = 'scatter', mode = 'lines')


