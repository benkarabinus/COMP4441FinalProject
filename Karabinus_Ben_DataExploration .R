# Final project COMP 4441 Data Exploration Ben Karabinus 7/31/2021

#initial setup
if(!require(DataExplorer)){install.packages("DataExplorer")}
library(DataExplorer)
if(!require(tidyr)){install.packages("tidyr")}
library(tidyr)

# Load Datasets
# Albequerque
ABQ <- read.csv("ABQ.csv", header = T)
# Denver
DEN <- read.csv("Den.csv", header = T)
# Phoenix
PHX <- read.csv("PHX.csv", header = T)
# Salt Lake City 
SLC <- read.csv("SLC.csv", header = T)

####explore the datasets and replace missing values####

# ABQ
summary(ABQ)
# show data types, factors and levels 
str(ABQ)
#view the first few rows of data
head(ABQ)
# visualization to check for missing data
plot_intro(ABQ, title='Albequerque')
# if data missing get the details 
profile_missing(ABQ)
# plot of the missing data 
plot_missing(ABQ, title = "Albequerque", group=c("No Missing Values"=0, 
                          "PCT Missing Values"= 1))
# replace NA's in the SNOW column 
ABQ$SNOW[is.na(ABQ$SNOW)] <- 0
#check to ensure replacement
profile_missing(ABQ)

# replace the missing values for TAVG
ABQTEMP <- data.frame(ABQ$TMAX, ABQ$TMIN)
ind <- which(is.na(ABQ), arr.ind=TRUE)
ABQ[ind] <- round(rowMeans(ABQTEMP, na.rm=TRUE)[ind[,1]],0)
#check to ensure replacement 
profile_missing(ABQ)

# DEN
summary(DEN)
# show data types, factors and levels 
str(Den)
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
DEN$PRCP[is.na(DEN$PRCP)] <- median(DEN$PRCP, 
                                 na.rm = TRUE)

# check to ensure removal 
profile_missing(DEN)
# remove the NA's for SNOW
DEN$SNOW[is.na(DEN$SNOW)] <- median(DEN$SNOW,                                   na.rm = TRUE)
# check removal 
profile_missing(DEN)

# remove the NA's for TMAX
DEN$TMAX[is.na(DEN$TMAX)] <- median(DEN$TMAX,                                   na.rm = TRUE)
# check removal 
profile_missing(DEN)

# remove the NA's for TMIN
DEN$TMIN[is.na(DEN$TMIN)] <- median(DEN$TMIN,                                   na.rm = TRUE)
# check removal 
profile_missing(DEN)

# replace the missing values for TAVG
DENTEMP <- data.frame(DEN$TMAX, DEN$TMIN)
ind <- which(is.na(DEN), arr.ind=TRUE)
DEN[ind] <- round(rowMeans(DENTEMP, na.rm=TRUE)[ind[,1]],0)
#check to ensure replacement 
profile_missing(DEN)

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
plot_missing(PHX, title = "Phoenix", group=c("No Missing Values"=0, 
                                                 "PCT Missing Values"= 1))

# remove the NA's for SNOW
PHX$SNOW[is.na(PHX$SNOW)] <- median(PHX$SNOW,                                   na.rm = TRUE)
# check removal 
profile_missing(PHX)

# replace the missing values for TAVG
PHXTEMP <- data.frame(PHX$TMAX, PHX$TMIN)
ind <- which(is.na(PHX), arr.ind=TRUE)
PHX[ind] <- round(rowMeans(PHXTEMP, na.rm=TRUE)[ind[,1]],0)
#check to ensure replacement 
profile_missing(PHX)

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
