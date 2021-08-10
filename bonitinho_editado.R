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
forecast_tavg=forecast(model_tavg,h=60)

forecast_prcp$lower<-apply(forecast_prcp$lower, 2, function(x) ifelse(x < 0, 0, x))
forecast_snow$lower<-apply(forecast_snow$lower, 2, function(x) ifelse(x < 0, 0, x))



plot(forecast_prcp,xlim=c(2019,2024))
plot(forecast_snow,xlim=c(2019,2024))
plot(forecast_tavg,xlim=c(2019,2022))


a=forecast(modelo,h=60)

a$lower # fix this

# DEN
summary(DEN)
# show data types, factors and levels 
str(DEN)
#view the first few rows of data
head(DEN)
# visualization to check for missing data
plot_intro(DEN, title='Denver')
# if data missing get the details 
profile_missing(DEN)
# plot of the missing data 
plot_missing(DEN, title = "Denver", 
             group=c("No Missing Values"=0, 
                    "PCT Missing Values"= 1))
# remove NA's for precipitation 
DEN$PRCP[is.na(DEN$PRCP)] <- median(DEN$PRCP, na.rm = TRUE)
# check to ensure removal 
profile_missing(DEN)
# remove the NA's for SNOW
DEN$SNOW[is.na(DEN$SNOW)] <- median(DEN$SNOW, na.rm = TRUE)
# check removal 
profile_missing(DEN)
# remove the NA's for TMAX
DEN$TMAX[is.na(DEN$TMAX)] <- median(DEN$TMAX, na.rm = TRUE)
# check removal 
profile_missing(DEN)
# remove the NA's for TMIN
DEN$TMIN[is.na(DEN$TMIN)] <- median(DEN$TMIN, na.rm = TRUE)
# check removal 
profile_missing(DEN)
# replace the missing values for TAVG
DENTEMP <- data.frame(DEN$TMAX, DEN$TMIN)
ind <- which(is.na(DEN), arr.ind=TRUE)
DEN[ind] <- round(rowMeans(DENTEMP, na.rm=TRUE)[ind[,1]],0)
#check to ensure replacement 
profile_missing(DEN)
# transform DATE column to date data type
DEN <- transform(DEN, DATE = as.Date(DATE))
#verify changes 
sapply(DEN, class)
# create column month year to aggregate data for time series 
DEN$MONTH_YEAR <- floor_date(DEN$DATE,"month")
# create aggregated dataset using MONTH_YEAR COLUMN from DEN
# This aggeregate will be used to create the time series 
DEN_AGG <- DEN %>%
  group_by(MONTH_YEAR)%>%
  dplyr::summarize(value=sum(PRCP)) %>%
  as.data.frame()
# create a time series from DEN_AGG
DEN_TS <- ts(DEN_AGG[, 2], start= c(1970,1), end= c(2019,12), frequency = 12)
# print the new time series 
DEN_TS
# basic plot of the new time series 
plot(DEN_TS)

# PHX
summary(PHX)
# show data types, factors and levels 
str(PHX)
#view the first few rows of data
head(PHX)
# visualization to check for missing data
plot_intro(PHX, title='Phoenix')
# if data missing get the details 
profile_missing(PHX)
# plot of the missing data 
plot_missing(PHX, title = "Phoenix", group=c("No Missing Values"=0,                                                 "PCT Missing Values"= 1))
# remove the NA's for SNOW
PHX$SNOW[is.na(PHX$SNOW)] <- median(PHX$SNOW,na.rm = TRUE)
# check removal 
profile_missing(PHX)
# replace the missing values for TAVG
PHXTEMP <- data.frame(PHX$TMAX, PHX$TMIN)
ind <- which(is.na(PHX), arr.ind=TRUE)
PHX[ind] <- round(rowMeans(PHXTEMP, na.rm=TRUE)[ind[,1]],0)
#check to ensure replacement 
profile_missing(PHX)
# transform DATE column to date data type
PHX <- transform(PHX, DATE = as.Date(DATE))
#verify changes 
sapply(PHX, class)
# create column month year to aggregate data for time series 
PHX$MONTH_YEAR <- floor_date(PHX$DATE,"month")
# create aggregated dataset using MONTH_YEAR COLUMN from DEN
# This aggeregate will be used to create the time series 
PHX_AGG <- PHX %>%
  group_by(MONTH_YEAR)%>%
  dplyr::summarize(value=sum(PRCP)) %>%
  as.data.frame()
# create a time series from PHX_AGG
PHX_TS <- ts(PHX_AGG[, 2], start= c(1970,1), end= c(2019,12), frequency = 12)
# print the new time series 
PHX_TS
# basic plot of the new time series 
plot(PHX_TS)

# SLC
summary(SLC)
# show data types, factors and levels 
str(SLC)
#view the first few rows of data
head(SLC)

# visualization to check for missing data
plot_intro(SLC, title='Salt Lake City')
# if data missing get the details 
profile_missing(SLC)
# plot of the missing data 
plot_missing(SLC, title = "Salt Lake City ", group=c("No Missing Values"=0, 
                                                "PCT Missing Values"= 1))
# remove the NA's for SNOW
SLC$SNOW[is.na(SLC$SNOW)] <- median(SLC$SNOW,                                   na.rm = TRUE)
# check removal 
profile_missing(SLC)

# replace the missing values for TAVG
SLCTEMP <- data.frame(SLC$TMAX, SLC$TMIN)
ind <- which(is.na(SLC), arr.ind=TRUE)
SLC[ind] <- round(rowMeans(SLCTEMP, na.rm=TRUE)[ind[,1]],0)
#check to ensure replacement 
profile_missing(SLC)
# transform DATE column to date data type
SLC <- transform(SLC, DATE = as.Date(DATE))
#verify changes 
sapply(SLC, class)
# create column month year to aggregate data for time series 
SLC$MONTH_YEAR <- floor_date(SLC$DATE,"month")
# create aggregated dataset using MONTH_YEAR COLUMN from DEN
# This aggeregate will be used to create the time series 
SLC_AGG <- SLC %>%
  group_by(MONTH_YEAR)%>%
  dplyr::summarize(value=sum(PRCP)) %>%
  as.data.frame()
# create a time series from PHX_AGG
SLC_TS <- ts(SLC_AGG[, 2], start= c(1970,1), end= c(2019,12), frequency = 12)
# print the new time series 
SLC_TS
# basic plot of the new time series 
plot(SLC_TS)
