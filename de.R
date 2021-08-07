# Final project COMP 4441 Data Exploration Ben Karabinus 7/31/2021

#initial setup
library(DataExplorer)
library(tidyverse)
library(lubridate)
library(forecast)
library(ggplot2)
library(dplyr)
library(TTR)
library(fpp2)
library(reader)

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

ABQ_AGG <- ABQ %>%
  group_by(MONTH_YEAR)%>%
  dplyr::summarize(value=mean((TAVG))) %>%
  as.data.frame()


# create a time series from ABQ_AGG
ABQ_TS <- ts(ABQ_AGG[, 2], start= c(1970,1), end= c(2019,12), frequency = 365)
plot(ABQ_TS)
ABQ_TS

arima<-auto.arima(ABQ_TS)
summary(arima)

