#  Program:               MATH_3570_Project
#  Version:               1.0
#  Author:                Cade Dombrowski, David Helminiak, Reid Holben, Liam Fruzyna
#  Date Created:          6 March 2018
#  Date Last Modified:    6 March 2018
#  Purpose:               Clean and analyze data found for weather, traffic and crime within Chicago for years 2013-2015

#
# SETUP
#

#Library imports
library(tidyverse)
library(reshape2)

#Set working directory to location where data is stored
setwd("~/Desktop/MATH 3570 Project/data")

#
#IMPORT DATASETS
#
weatherData <- read.csv("Chicago_Midway_Airport_Weather Station.csv")
trafficData <- read.csv("Chicago_Traffic_Tracker_-_Historical_Congestion_Estimates_by_Segment.csv")
crimeData <- read.csv("Crimes_-_2001_to_present.csv")

#
#FILTER BY DATE
#

#Ignoring potential discrepencies caused by time zones, as primary interest is on daily information
#Filter out information to just that obtained between Jan 1 2013 through Dec 30 2014 (Overlapping years for all three datasets)
weatherData <- filter(weatherData, (as.Date(weatherData$DATE) >= as.Date("2013-1-15")) & (as.Date(weatherData$DATE) <= as.Date("2014-12-30")))

#Make a copy of the crime data
newCrime <- crimeData
#Split Date column into Date and Time
newCrime$Date <- substring(crimeData$Date,1,10)
newCrime$Time <- substring(crimeData$Date,12)
#Split existing Date into Month Day and Year columns based on delimeter
newCrime <- separate(data = newCrime, col = Date, into = c("Month", "Day", "Year"), sep = "/")
#Recombine into ISO date format
newCrime$Date <- as.Date(ISOdate(newCrime$Year, newCrime$Month, newCrime$Day))
#Remove the temporary and now extranous columns
newCrime$Year = NULL
newCrime$Month = NULL
newCrime$Day = NULL
#Filter out information to just that obtained between Jan 1 2013 through Dec 30 2014 (Overlapping years for all three datasets)
crimeData <- filter(newCrime, (as.Date(newCrime$Date) >= as.Date("2013-1-15")) & (as.Date(newCrime$Date) <= as.Date("2014-12-30")))
newCrime = NULL

#trafficData dates are already in the range specified
#NOTE: CHECK FORMATTING IS IDENTICAL TO THE OTHERS (ISO)


#
#FIX WEATHER DATASET
#

#Fix variable names for weather
#NOTE: TODO

#Remove superfluous weather variables
#NOTE: TODO





#
#FIX TRAFFIC DATASET
#

#Remove traffic observations that have no data
trafficData <- trafficData[!(trafficData$SPEED=="-1"),]

#Remove superfluous traffic variables
#NOTE: TODO





#
#FIX CRIME DATASET
#

#Remove crime observations that have no applicable data (missing location, or type of crime)
#NOTE: TODO

#Remove superfluous crime variables
#NOTE: TODO



#
# COMBINE DATASETS
#


