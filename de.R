#initial setup
library(DataExplorer)
library(tidyverse)
library(lubridate)
library(tseries)
library(forecast)
library(ggplot2)
library(dplyr)
library(TTR)
library(fpp2)
library(reader)
library(stats)

adf.

setwd("/home/ellmann/Documents/stats/COMP4441FinalProject")

ABQ <- read.csv("ABQ.csv", header = T)
DEN <- read.csv("DEN.csv", header = T)
PHX <- read.csv("PHX.csv", header = T)
SLC <- read.csv("SLC.csv", header = T)

# replace NA's in the SNOW column with the median snowfall 
ABQ$SNOW[is.na(ABQ$SNOW)] <- median(ABQ$SNOW, na.rm = T)
DEN$SNOW[is.na(DEN$SNOW)] <- median(DEN$SNOW, na.rm = T)
PHX$SNOW[is.na(ABQ$SNOW)] <- median(PHX$SNOW, na.rm = T)
SLC$SNOW[is.na(ABQ$SNOW)] <- median(SLC$SNOW, na.rm = T)

# replace the missing values for TAVG, values replaced by the average of TMAX and TMIN
TEMP <- data.frame(ABQ$TMAX, ABQ$TMIN)
ind <- which(is.na(ABQ), arr.ind=TRUE)
ABQ[ind] <- round(rowMeans(TEMP, na.rm=TRUE)[ind[,1]],0)
profile_missing(ABQ)

TEMP <- data.frame(DEN$TMAX, DEN$TMIN)
ind <- which(is.na(DEN), arr.ind=TRUE)
DEN[ind] <- round(rowMeans(TEMP, na.rm=TRUE)[ind[,1]],0)
profile_missing(DEN)

TEMP <- data.frame(PHX$TMAX, PHX$TMIN)
ind <- which(is.na(PHX), arr.ind=TRUE)
PHX[ind] <- round(rowMeans(TEMP, na.rm=TRUE)[ind[,1]],0)
profile_missing(PHX)

TEMP <- data.frame(SLC$TMAX, SLC$TMIN)
ind <- which(is.na(SLC), arr.ind=TRUE)
SLC[ind] <- round(rowMeans(TEMP, na.rm=TRUE)[ind[,1]],0)
profile_missing(SLC)

glimpse(ABQ)

# transform DATE column to date data type
ABQ <- transform(ABQ, DATE = as.Date(DATE))
DEN <- transform(DEN, DATE = as.Date(DATE))
PHX <- transform(PHX, DATE = as.Date(DATE))
SLC <- transform(SLC, DATE = as.Date(DATE))

# create column month year to aggregate data for time series 
ABQ$MONTH_YEAR <- floor_date(ABQ$DATE,"month")
ABQ$YEAR <- floor_date(ABQ$DATE,"year")

# create aggregated dataset using MONTH_YEAR COLUMN from ABQ
# This aggregate will be used to create the time series 
# ABQ_AGG <- ABQ %>%
#   group_by(MONTH_YEAR)%>%
#   dplyr::summarize(value=sum(PRCP)) %>%
#   as.data.frame()
# # create a time series from ABQ_AGG
# ABQ_TS <- ts(ABQ_AGG[, 2], start= c(1970,1), end= c(2019,12), frequency = 12)
# # print the new time series 
# ABQ_TS
# # basic plot of the new time series 
# plot(ABQ_TS)

# ABQ_AGG <- ABQ %>%
#   group_by(MONTH_YEAR)%>%
#   dplyr::summarize(value=mean((TAVG))) %>%
#   as.data.frame()

ABQ_AGG_PRCP <- ABQ %>%
  group_by(MONTH_YEAR)%>%
  dplyr::summarize(value=mean(PRCP)) %>%
  as.data.frame()

ABQ_AGG_SNOW <- ABQ %>%
  group_by(MONTH_YEAR)%>%
  dplyr::summarize(value=sum(SNOW)) %>%
  as.data.frame()

ABQ_AGG_TAVG <- ABQ %>%
  group_by(MONTH_YEAR)%>%
  dplyr::summarize(value=mean(TAVG)) %>%
  as.data.frame()


# create a time series from ABQ_AGG
# abq_ts <- ts(ABQ_AGG[, 2], start= c(1970,1), end= c(2019,12), frequency = 1)
# summary(abq_ts)
# start(abq_ts)
# end(abq_ts)
# frequency(abq_ts)
# plot(abq_ts)
# linearModel = lm(abq_ts ~ time(abq_ts))
# abline(reg = linearModel) # fit in a Linear Model (Intercept & Slope), and plot the line 
# 
# plot(abq_ts)

ABQ_TS_PRCP <- ts(ABQ_AGG_PRCP[, 2], start= c(1970,1), end= c(2019,12), frequency = 12)
# ABQ_TS_SNOW <- ts(ABQ_AGG_SNOW[, 2], start= c(1970,1), end= c(2019,12), frequency = 12)
# ABQ_TS_TAVG <- ts(ABQ_AGG_TAVG[, 2], start= c(1970,1), end= c(2019,12), frequency = 12)

plot(ABQ_TS_PRCP)
dim(ABQ_TS_PRCP)



plot(diff(log(ABQ_AGG$value)))
summary(abq_ts)
start(abq_ts)
end(abq_ts)
frequency(abq_ts)
plot(abq_ts)
linearModel = lm(abq_ts ~ time(abq_ts))
abline(reg = linearModel) # fit in a Linear Model (Intercept & Slope), and plot the line 
plot(abq_ts)




arima<-auto.arima(abq_ts)
pred <- forecast(arima, h=50)
plot(pred)
summary(arima)

