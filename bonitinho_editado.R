library(tseries)
library(DataExplorer)
library(tidyverse)
library(forecast)
library(lubridate)

setwd("/home/ellmann/Documents/stats/COMP4441FinalProject")

ABQ <- read.csv("ABQ.csv", header = T)
ABQ$SNOW[is.na(ABQ$SNOW)] <- median(ABQ$SNOW, na.rm = T)
ABQTEMP <- data.frame(ABQ$TMAX, ABQ$TMIN)
ind <- which(is.na(ABQ), arr.ind=TRUE)
ABQ[ind] <- round(rowMeans(ABQTEMP, na.rm=TRUE)[ind[,1]],0)
profile_missing(ABQ)
ABQ <- transform(ABQ, DATE = as.Date(DATE))
ABQ$MONTH_YEAR <- floor_date(ABQ$DATE,"month")


ABQ_AGG_PRCP<- ABQ %>%
  group_by(MONTH_YEAR)%>%
  dplyr::summarize(value=sum(PRCP)) %>%
  as.data.frame()

ABQ_AGG_SNOW<- ABQ %>%
  group_by(MONTH_YEAR)%>%
  dplyr::summarize(value=sum(SNOW)) %>%
  as.data.frame()

ABQ_AGG_TAVG<- ABQ %>%
  group_by(MONTH_YEAR)%>%
  dplyr::summarize(value=mean(TAVG)) %>%
  as.data.frame()

ABQ_TS_PRCP <- ts(ABQ_AGG_PRCP[, 2], start= c(1970,1), end= c(2019,12), frequency = 12)
ABQ_TS_SNOW <- ts(ABQ_AGG_SNOW[, 2], start= c(1970,1), end= c(2019,12), frequency = 12)
ABQ_TS_TAVG <- ts(ABQ_AGG_TAVG[, 2], start= c(1970,1), end= c(2019,12), frequency = 12)

adf.test(ABQ_TS_PRCP,alternative = "stationary")
adf.test(ABQ_TS_SNOW,alternative = "stationary")
adf.test(ABQ_TS_TAVG,alternative = "stationary")

model_prcp=auto.arima(ABQ_TS_PRCP)
model_snow=auto.arima(ABQ_TS_SNOW)
model_tavg=auto.arima(ABQ_TS_TAVG)

forecast_prcp=forecast(model_prcp,h=60)
forecast_snow=forecast(model_snow,h=60)
forecast_tavg=forecast(model_tavg,h=120)

forecast_prcp$lower<-apply(forecast_prcp$lower, 2, function(x) ifelse(x < 0, 0, x))
forecast_snow$lower<-apply(forecast_snow$lower, 2, function(x) ifelse(x < 0, 0, x))


plot(forecast_prcp,xlim=c(2000,2024))
plot(forecast_snow,xlim=c(2000,2024))
plot(forecast_tavg,xlim=c(2010,2030))
