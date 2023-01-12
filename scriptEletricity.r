rm(list=ls())

## Libraries
pacman::p_load(tseries, tidyverts, ggpubr,plotly,dplyr, seasonal, forecast)

## Settings
options(scipen=999)
setwd("./Statistics/01. Data/")


# Loading Data ------------------------------------------------------------
dataFrame <- read.csv("eletrecidade.csv")

## Selecting RS state
dataFrameRS <- dataFrame %>% filter(sigla_uf == "RS") %>% filter(tipo_consumo == "Total")
dataFrameRS_ts <- dataFrameRS  %>% select("ano","consumo")

## Subsetting Data

### Train
timeSeriesRStrain <- dataFrameRS_ts %>% filter(ano < 2021)
timeSeriesRStrain <- ts(timeSeriesRStrain$consumo, start =c(2004, 1), end = c(2020,12), frequency = 12)

### Test
timeSeriesRStest <- dataFrameRS_ts %>% filter(ano >= 2021)
timeSeriesRStest <- ts(timeSeriesRStest$consumo, start =c(2021, 1), end = c(2021,12), frequency = 12)

# Descriptive Analysis ----------------------------------------------------

## Autolot
autoplot(timeSeriesRStrain) + ylab("Consumption") + labs( title = "Time Series Eletrecity RS, Brazil")

## Decomposition

### Additive
timeSeriesRS_stl1 <- decompose(timeSeriesRStrain, type = "additive")
plot(timeSeriesRS_stl1)

### Multiplicative
timeSeriesRS_stl2 <- decompose(timeSeriesRStrain, type = "multiplicative")
plot(timeSeriesRS_stl2)


### Seasonality
fig <- dataFrameRS %>% plot_ly(x = ~mes, y = ~consumo, type = "box", boxpoints = "all", jitter = 0.3,
                             pointpos = -1.8, 
                             hoverinfo = 'text',
                             text = ~paste('</Mês> BRL= ', dataFrameRS$mes,
                                           '</br> Consumo ', dataFrameRS$consumo
                             ))
fig

ggseasonplot(timeSeriesRStrain, year.labels = T, year.labels.left = T) + ylab("Electricity Consumption") +
  ggtitle("Seasonal plot: Electricity Consumption")

ggsubseriesplot(timeSeriesRStrain)

## Original Data

### Correlograms
par(mfrow = c(1,2))
acf(timeSeriesRStrain, lag.max = 60)
pacf(timeSeriesRStrain, lag.max = 60)

gghistogram(dataFrameRS$consumo, bins = 30)

### Augmented Dickey Fuller (ADF)
adf.test(timeSeriesRStrain)

## First Difference
timeSeriesRS_diff <- diff(timeSeriesRStrain, 1)

### Correlograms
par(mfrow = c(1,2))
acf(timeSeriesRS_diff, lag.max = 60)
pacf(timeSeriesRS_diff, lag.max = 60)

### Augmented Dickey Fuller (ADF)
adf.test(timeSeriesRS_diff)

# Moving Average ----------------------------------------------------------

## Checking Modelling
par(mfrow = c(2,2))

plot(timeSeriesRStrain, col="gray", main = "1 Year Moving Average Smoothing")
lines(ma(timeSeriesRStrain, order = 12), col = "red", lwd=3)

plot(timeSeriesRStrain, col="gray", main = "3 Year Moving Average Smoothing")
lines(ma(timeSeriesRStrain, order = 36), col = "blue", lwd=3)

plot(timeSeriesRStrain, col="gray", main = "5 Year Moving Average Smoothing")
lines(ma(timeSeriesRStrain, order = 60), col = "green", lwd=3)

plot(timeSeriesRStrain, col="gray", main = "5 Year Moving Average Smoothing")
lines(ma(timeSeriesRStrain, order = 120), col = "yellow", lwd=3)

dev.off()

# Arima -------------------------------------------------------------------

## Model

fitArima <- auto.arima(timeSeriesRStrain, d=1, D=1, stepwise = F, approximation = F, trace=T)
summary(fitArima)
checkresiduals(fitArima)

## Forcast
fcast <- forecast(fitArima, h=12)
autoplot(fcast) + autolayer(timeSeriesRStest, series = "Actual 2021") + 
  autolayer(fcast$mean, series="Forecasts")

# Linear Regression -----------------------------------------

## Original Data + Trend
elecConsumptionRS <- tslm(timeSeriesRStrain~trend)
f <- forecast(elecConsumptionRS,h=12,level=c(80,95)) # Level denotes confidence levels. i.e) 80% and 95% CI.
plot.ts(timeSeriesRStrain,main = "Electricity Consumption",xlab="Year",ylab="Consuption",xlim=c(2004, 2021))
lines(f$fitted,col="blue")
lines(f$mean,col="blue")

## Checking Residuals
checkresiduals(elecConsumptionRS)

## Forcast
autoplot(f) + autolayer(timeSeriesRStest, series = "Actual 2021") + 
  autolayer(f$mean, series="Forecasts")

## Original Data + Trend + Seasonal
elecConsumptionRS <- tslm(timeSeriesRStrain~trend + season)
f <- forecast(elecConsumptionRS,h=12,level=c(80,95)) # Level denotes confidence levels. i.e) 80% and 95% CI.
plot.ts(timeSeriesRStrain,main = "Electricity Consumption",xlab="Year",ylab="Consuption",xlim=c(2004, 2021))
lines(f$fitted,col="blue")
lines(f$mean,col="blue")

## Checking Residuals
checkresiduals(elecConsumptionRS)

## Forcast
autoplot(f) + autolayer(timeSeriesRStest, series = "Actual 2021") + 
  autolayer(f$mean, series="Forecasts")
