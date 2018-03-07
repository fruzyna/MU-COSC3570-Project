#  Program:               MATH_3570_Project
#  Version:               1.0
#  Author:                David Helminiak
#  Date Created:          6 March 2018
#  Date Last Modified:    6 March 2018
#  Purpose:               Clean and analyze data found for weather, traffic and crime within Chicago for years 2013-2015

#Library imports
library(dplyr)

#Set working directory to location where data is stored
setwd("~/Desktop/MATH 3570 Project/data")

#Import initial datasets
weatherData <- read.csv("Chicago_Midway_Airport_Weather Station.csv")
trafficData <- read.csv("Chicago_Traffic_Tracker_-_Historical_Congestion_Estimates_by_Segment.csv")
crimeData <- read.csv("Crimes_-_2001_to_present.csv")

#Filter out information to just that obtained between Jan 1 2013 through Dec 30 2014 (Overlapping years for all three datasets)
newWeather <- filter(weatherData, (as.Date(weatherData$DATE) > as.Date("2013-1-15")) & (as.Date(weatherData$DATE) < as.Date("2014-12-30")))





