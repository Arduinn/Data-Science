rm(list=ls())

## Libraries
pacman::p_load(tseries, tidyverts, ggpubr,plotly,dplyr, seasonal, forecast)

## Settings
options(scipen=999)
setwd('/media/arduin/Ubuntu HDD/06. Data Science Projects/06. Eletrecity')


# Loading Data ------------------------------------------------------------
dataFrame <- read.csv("./01. Data/eletrecidade.csv")

## Selecting RS state
dataFrameRS <- dataFrame %>% filter(sigla_uf == "RS") %>% filter(tipo_consumo == "Total")

## Converting TS data
timeSeriesRS <-  dataFrameRS %>% select("consumo")
timeSeriesRS <- ts(timeSeriesRS, start =c(2004, 1), end = c(2021,12), frequency = 12)

# Descriptive Analysis ----------------------------------------------------

## Autolot
autoplot(timeSeriesRS,) + ylab("Consumption") + labs( title = "Time Series Eletrecity RS, Brazil")

## Decomposition

### Additive
timeSeriesRS_stl1 <- decompose(timeSeriesRS, type = "additive")
plot(timeSeriesRS_stl1)

### Multiplicative
timeSeriesRS_stl2 <- decompose(timeSeriesRS, type = "multiplicative")
plot(timeSeriesRS_stl2)


### Seasonality
fig <- dataFrameRS %>% plot_ly(x = ~mes, y = ~consumo, type = "box", boxpoints = "all", jitter = 0.3,
                             pointpos = -1.8, 
                             hoverinfo = 'text',
                             text = ~paste('</Mês> BRL= ', dataFrameRS$mes,
                                           '</br> Consumo ', dataFrameRS$consumo
                             ))
fig

ggseasonplot(timeSeriesRS, year.labels = T, year.labels.left = T) + ylab("Electricity Consumption") +
  ggtitle("Seasonal plot: Electricity Consumption")

## Original Data

### Correlograms
par(mfrow = c(1,2))
acf(timeSeriesRS, lag.max = 60)
pacf(timeSeriesRS, lag.max = 60)

gghistogram(dataFrameRS$consumo, bins = 30)

### Augmented Dickey Fuller (ADF)
adf.test(timeSeriesRS)

## First Difference
timeSeriesRS_diff <- diff(timeSeriesRS, 1)

### Correlograms
par(mfrow = c(1,2))
acf(timeSeriesRS_diff, lag.max = 60)
pacf(timeSeriesRS_diff, lag.max = 60)

### Augmented Dickey Fuller (ADF)
adf.test(timeSeriesRS)

# Moving Average ----------------------------------------------------------

## Checking Modelling
par(mfrow = c(2,2))

plot(timeSeriesRS, col="gray", main = "1 Year Moving Average Smoothing")
lines(ma(timeSeriesRS, order = 12), col = "red", lwd=3)

plot(timeSeriesRS, col="gray", main = "3 Year Moving Average Smoothing")
lines(ma(timeSeriesRS, order = 36), col = "blue", lwd=3)

plot(timeSeriesRS, col="gray", main = "5 Year Moving Average Smoothing")
lines(ma(timeSeriesRS, order = 60), col = "green", lwd=3)

plot(timeSeriesRS, col="gray", main = "5 Year Moving Average Smoothing")
lines(ma(timeSeriesRS, order = 120), col = "yellow", lwd=3)

# Arima -------------------------------------------------------------------

## Model

tsModel <- auto.arima(timeSeriesRS, seasonal = T)
summary(tsModel)

# Forecast with Linear Regression -----------------------------------------

## Original Data + Trend
elecConsumptionRS <- tslm(timeSeriesRS~trend)
f <- forecast(elecConsumptionRS,h=12,level=c(80,95)) # Level denotes confidence levels. i.e) 80% and 95% CI.
plot.ts(timeSeriesRS,main = "Electricity Consumption",xlab="Year",ylab="Consuption",xlim=c(2004, 2021))
lines(f$fitted,col="blue")
lines(f$mean,col="blue")

checkresiduals(elecConsumptionRS)

## Original Data + Trend + Seasonal
elecConsumptionRS <- tslm(timeSeriesRS~trend + season)
f <- forecast(elecConsumptionRS,h=12,level=c(80,95)) # Level denotes confidence levels. i.e) 80% and 95% CI.
plot.ts(timeSeriesRS,main = "Electricity Consumption",xlab="Year",ylab="Consuption",xlim=c(2004, 2021))
lines(f$fitted,col="blue")
lines(f$mean,col="blue")

checkresiduals(elecConsumptionRS)


# References --------------------------------------------------------------

## https://rpubs.com/linlinmao/time_series_2
