# Data analysis based on already cleaned and saved to file data

#
# SETUP
#

#Package installations if needed
#install.packages("geosphere")
#install.packages("tidyverse")
#install.packages("mice")
#install.packages("dplyr")
#install.packages("geosphere")
#install.packages("maps")
#install.packages("ggmap")


#Library imports
library(tidyverse)
library(reshape2)
library(geosphere)
library(dplyr)
library(maps)
library(ggmap)
library(mice)

#Set working directory to location where data is stored
#setwd("~/Desktop/MATH 3570 Project/data")

#
#IMPORT DATASETS
#
weatherData <- read.csv("weather_clean.csv")
trafficData <- read.csv("traffic_data.csv")
combData <- read.csv("comb_data.csv")

#
# Do some fun stuff
#

months = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

weatherData <- separate(weatherData, col='DATE', into=c('year','month', 'day'), sep='-')

# Plot average high by month
meanTemp = mean(weatherData$MAX_TEMP...F.)
meanTemp
weatherData$high = weatherData$MAX_TEMP...F.
averageHigh = aggregate(high ~ month, weatherData, mean)
barplot(averageHigh$high, names.arg=months, main="Average Temperature by Month in Chicago", ylab="Degrees F", xlab="Month")

# Plot average precip by month
avgPrecip = aggregate(PRECIPITATION.in. ~ month, weatherData, mean)
avgPrecip
barplot(avgPrecip$PRECIPITATION.in., names.arg=months, main="Average Percipitation by Month in Chicago", ylab="Inches", xlab="Month")

# most common by temperature
aggregate(combData$CRIME_TYPE, list(combData$MAX_TEMP...F.),
          function(x) { 
            ux <- unique(x) 
            ux[which.max(tabulate(match(x, ux)))]})

# mode
aggregate(MAX_TEMP...F. ~ CRIME_TYPE, combData, function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]})

# delta from mean
deltaMean = aggregate(MAX_TEMP...F. ~ CRIME_TYPE, combData, function(v) {
  delta <- mean(v) - meanTemp})

# plot delta mean temp by crime
deltaMean = deltaMean[!grepl('NON-CRIMINAL', deltaMean$CRIME_TYPE),] # remove non criminal crimes
barplot(deltaMean$MAX_TEMP...F., names.arg=deltaMean$CRIME_TYPE, las=2, main="Delta Temeperature Between Annual Average and Average by Crime in Chicago", ylab="Degrees Above Annual Average (~59 F)", xlab="Crime Type")

#Plot all crimes by type within Chicago
chicago <- get_map(location = 'chicago', zoom = 10)
ggmap(chicago) +
  geom_point(data = combData, mapping = aes(x = combData$LONGITUDE, y = combData$LATITUDE, color = combData$CRIME_TYPE))+
  labs(color = "Crime Type", x = "Longitude", y = "Latitude") +
  ggtitle("Locations of Crime within Chicago")