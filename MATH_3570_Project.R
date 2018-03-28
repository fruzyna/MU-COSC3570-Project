#  Program:               MATH_3570_Project
#  Version:               1.0
#  Author:                Cade Dombrowski, David Helminiak, Reid Holben, Liam Fruzyna
#  Date Created:          6 March 2018
#  Date Last Modified:    28 March 2018
#  Purpose:               Clean and analyze data found for weather, traffic and crime within Chicago for years 2013-2015
#  Versioning Plan:       1.1: All data clean and combined
#                         1.2: Basic visualizations
#                         1.3: Linear modeling
#                         2.0: Final project deliverable
#  External Datasets:     https://data.cityofchicago.org/Transportation/Chicago-Traffic-Tracker-Congestion-Estimates-by-Se/n4j6-wkkf
#
#
# TO_CITE:  https://www.r-bloggers.com/visualising-thefts-using-heatmaps-in-ggplot2/
#           




#
# SETUP
#

#NOTES: Download additional dataset: https://data.cityofchicago.org/Transportation/Chicago-Traffic-Tracker-Congestion-Estimates-by-Se/n4j6-wkkf for segment IDs

#Package installations if needed
#install.packages("geosphere")
#install.packages("tidyverse")
#install.packages("mice")
#install.packages("dplyr")
#install.packages("geosphere")
#install.packages("maps")
#install.packages("ggmap")

#IF USING A MAC PRE OSX SIERRA
# brew unlink gdal
# brew tap osgeo/osgeo4mac && brew tap --repair
# brew install proj
# brew install geos
# brew install udunits
# brew install gdal2 --with-armadillo --with-complete --with-libkml --with-unsupported
# brew link --force gdal2

#install.packages("sf")
#install.packages("sp")
#install.packages("maptools")

#Library imports
library(tidyverse)
library(reshape2)
library(geosphere)
library(dplyr)
library(maps)
library(ggmap)
library(mice)
library(sf)
library(sp)
library(maptools)

#Set working directory to location where data is stored
setwd("~/Desktop/MATH 3570 Project/data")

#
#IMPORT DATASETS
#
weatherData <- read.csv("Chicago_Midway_Airport_Weather Station.csv")
trafficData <- read.csv("Chicago_Traffic_Tracker_-_Historical_Congestion_Estimates_by_Segment.csv")
crimeData <- read.csv("Crimes_-_2001_to_present.csv")
roadSegmentData <- read.csv("Chicago_Traffic_Tracker_-_Congestion_Estimates_by_Segments.csv")

#
#FIX DATE AND TIME FORMATTING
#

#Ignoring potential discrepencies caused by time zones, as primary interest is on daily information
#Filter out information to just that obtained between Jan 1 2013 through Dec 30 2014 (Overlapping years for all three datasets)
weatherData <- filter(weatherData, (as.Date(weatherData$DATE) >= as.Date("2013-1-15")) & (as.Date(weatherData$DATE) <= as.Date("2014-12-30")))
weatherData$DATE <- as.Date(weatherData$DATE) #Place weather date infomration into date object format

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

#Split Date/Time column into solely Date
trafficData$DATE <- substring(trafficData$TIME,1,10)

#Remove the originating column
trafficData$TIME <- NULL

#
#CLEAN WEATHER DATASET
#

#Fix variable names for weather
names(weatherData)[31] <-paste("Fastest 2-minute wind speed")
names(weatherData)[9] <-paste("Time of fastest mile or fastest 1-minute wind")
names(weatherData)[33] <-paste("Fastest 5-second wind speed")
names(weatherData)[15] <-paste("Snowfall")
names(weatherData)[39] <-paste("THUNDER")
names(weatherData)[13] <-paste("PRECIPITATION(in)")
names(weatherData)[41] <-paste("HAIL")
names(weatherData)[43] <-paste("SMOKE_HAZE")
names(weatherData)[17] <-paste("Snow depth")
names(weatherData)[27] <-paste("Direction of fastest 2-minute wind")
names(weatherData)[7] <- paste("AVG_WIND_SPEED(MPH)")
names(weatherData)[29] <-paste("Direction of fastest 5-second wind")
names(weatherData)[45] <-paste("Tornado, waterspout, or funnel cloud")
names(weatherData)[11] <-paste("Peak gust time")
names(weatherData)[35] <-paste("Fog, ice fog, or freezing fog (may include heavy fog)")
names(weatherData)[21] <-paste("MAX_TEMP  (F)")
names(weatherData)[37] <-paste("Heavy fog or heaving freezing fog (not always distinguished from fog)")
names(weatherData)[19] <-paste("AVG_TEMP(F)")
names(weatherData)[23] <-paste("MIN_TEMP(F)")
names(weatherData)[25] <-paste("Total sunshine for the period")

#Remove extraneous/unused/constants/empty columns
weatherData$AWND_ATTRIBUTES <- NULL
weatherData$FMTM_ATTRIBUTES <- NULL
weatherData$PGTM_ATTRIBUTES <- NULL
weatherData$PRCP_ATTRIBUTES <- NULL
weatherData$SNOW_ATTRIBUTES <- NULL
weatherData$SNWD_ATTRIBUTES <- NULL
weatherData$TAVG_ATTRIBUTES <- NULL
weatherData$TMAX_ATTRIBUTES <- NULL
weatherData$TMIN_ATTRIBUTES <- NULL
weatherData$TSUN_ATTRIBUTES <- NULL
weatherData$WDF2_ATTRIBUTES <- NULL
weatherData$WDF5_ATTRIBUTES <- NULL
weatherData$WSF2_ATTRIBUTES <- NULL
weatherData$WSF5_ATTRIBUTES <- NULL
weatherData$WT01_ATTRIBUTES <- NULL
weatherData$WT02_ATTRIBUTES <- NULL
weatherData$WT03_ATTRIBUTES <- NULL
weatherData$WT05_ATTRIBUTES <- NULL
weatherData$WT08_ATTRIBUTES <- NULL
weatherData$WT10_ATTRIBUTES <- NULL
weatherData$STATION <- NULL
weatherData$NAME <- NULL
weatherData$ELEVATION <- NULL
weatherData$"Time of fastest mile or fastest 1-minute wind" <- NULL
weatherData$"Snowfall" <- NULL
weatherData$"Snow depth" <- NULL
weatherData$"Total sunshine for the period" <- NULL
weatherData$"Average Temperature" <- NULL
weatherData$"Tornado, waterspout, or funnel cloud" <- NULL
weatherData$"Direction of fastest 2-minute wind" <- NULL
weatherData$"Direction of fastest 5-second wind" <- NULL
weatherData$"Fastest 2-minute wind speed" <- NULL
weatherData$"Fastest 5-second wind speed" <- NULL
weatherData$"Peak gust time" <- NULL
weatherData$LATITUDE <- NULL
weatherData$LONGITUDE <- NULL
weatherData$"AVG_TEMP(F)" <- NULL

#Create central tendency (median) column
weatherData$"MEDIAN_TEMPERATURE(F)" <- ((weatherData$MAX_TEMP - weatherData$MIN_TEMP) / 2) + weatherData$MIN_TEMP

#Combine fog columns
weatherData$FOG = ifelse(is.na(weatherData$"Heavy fog or heaving freezing fog (not always distinguished from fog)"), weatherData$"Fog, ice fog, or freezing fog (may include heavy fog)", weatherData$"Heavy fog or heaving freezing fog (not always distinguished from fog)")

#Remove original fog columns
weatherData$"Heavy fog or heaving freezing fog (not always distinguished from fog)" <- NULL
weatherData$"Fog, ice fog, or freezing fog (may include heavy fog)" <- NULL

#
#FIX TRAFFIC AND ROADSEGMENT DATASETS
#

#Convert speed information to integers
trafficData$SPEED <- as.integer(trafficData$SPEED) 

#Remove observations that have no speed values
trafficData <- na.omit(trafficData, cols=c("SPEED"))
trafficData <- trafficData[!(trafficData$SPEED=="-1"),]

#Remove superfluous traffic variables
trafficData$ID <- NULL

#Simplify roadSegmentData
roadSegmentData$STREET <- NULL
roadSegmentData$DIRECTION <- NULL
roadSegmentData$FROM_STREET <- NULL
roadSegmentData$TO_STREET <- NULL
roadSegmentData$LENGTH <- NULL
roadSegmentData$STREET_HEADING <- NULL
roadSegmentData$COMMENTS <- NULL
roadSegmentData$CURRENT_SPEED <- NULL
roadSegmentData$LAST_UPDATED <- NULL

#Use speeds to determine overall congestion level
#Data source indicates 0-9, 10-20, and 21+ display heavy, medium, and free flow conditions
trafficData$CONGESTION_LEVEL[trafficData$SPEED >= 0 & trafficData$SPEED < 10] <- "Heavy"
trafficData$CONGESTION_LEVEL[trafficData$SPEED >= 10 & trafficData$SPEED < 21] <- "Medium"
trafficData$CONGESTION_LEVEL[trafficData$SPEED >= 21] <- "FreeFlow"

#Remove the now extraneous Speed column
trafficData$SPEED <- NULL

#
#FIX CRIME DATASET
#

#Remove crime observations that have no applicable data (missing location, or type of crime)
crimeData <- na.omit(crimeData, cols=c("LATITUDE", "LONGITUDE", "CRIME_TYPE"))

#Remove superfluous crime variables
crimeData$ID <- NULL
crimeData$Block <- NULL
crimeData$Case.Number <- NULL
crimeData$IUCR <- NULL
crimeData$Arrest <- NULL
crimeData$Beat <- NULL
crimeData$District <- NULL
crimeData$Ward <- NULL
crimeData$Community.Area <- NULL
crimeData$FBI.Code <- NULL
crimeData$X.Coordinate <- NULL
crimeData$Y.Coordinate <- NULL
crimeData$Updated.On <- NULL
crimeData$Location <- NULL
#crimeData$Domestic <- NULL
crimeData$Location.Description <- NULL

#Rename columns as desired
colnames(crimeData)[colnames(crimeData)=="Domestic"] <- "DOMESTIC"
colnames(crimeData)[colnames(crimeData)=="Primary.Type"] <- "CRIME_TYPE"
colnames(crimeData)[colnames(crimeData)=="Description"] <- "CRIME_DESCRIPTION"
colnames(crimeData)[colnames(crimeData)=="Latitude"] <- "LATITUDE"
colnames(crimeData)[colnames(crimeData)=="Longitude"] <- "LONGITUDE"
colnames(crimeData)[colnames(crimeData)=="Time"] <- "TIME"
colnames(crimeData)[colnames(crimeData)=="Date"] <- "DATE"

#
# COMBINE DATASETS
# Note: Datasets should not share any variable names save that by which they are being combined


#For each crime copy over the daily weather data
combData <- merge(crimeData, weatherData, by="DATE") 

#Remove originating datasets from memory
crimeData <- NULL
weatherData <- NULL

#Match each crime location to the nearest traffic line
# Create spatial line lists for each traffic segment; id = segmentid
lineList <- vector("list", 1)  #Create a temporary "list" for holding lines
spatialLineList <- list() #Create an empty list of spatial lines


for (i in 1:nrow(roadSegmentData)) { 
  lineList <- vector("list", 1) 
  lineList[[1]] <- Line(matrix(c (roadSegmentData$START_LONGITUDE[i], roadSegmentData$END_LONGITUDE[i], roadSegmentData$START_LATITUDE[i], roadSegmentData$END_LATITUDE[i]), nrow = 2, ncol = 2))
  spatialLineList[[i]] <- SpatialLines(list(Lines(lineList, ID = roadSegmentData$SEGMENTID[i])), proj4string = CRS(as.character(NA)))
}

#Combine the spatial lines together in a single list
merged.lines <- do.call(rbind, spatialLineList) 

#For each crime location, identify the nearest line, saving the corresponding segment id to the combined dataset
for (i in 1:nrow(combData)) { 
  crimePoint <- SpatialPoints(data.frame(LONGITUDE=c(combData$LONGITUDE[i]), LATITUDE=c(combData$LATITUDE[i])))
  nearestSegement <- snapPointsToLines(crimePoint, merged.lines, maxDist = NA, withAttrs = FALSE, idField=NA)
  combData$SEGMENTID[i] <- as.numeric(as.character(nearestSegement$nearest_line_id))
  #message((i/nrow(combData)*100), "% Complete") #Print debug message
}

#Merge traffic data with combined dataset
finalData <- inner_join(combData, trafficData, by = c("DATE", "SEGMENTID")) 

#Clean final dataset
finalData$SEGMENTID <- NULL

#Clean workspace
combData <- NULL
crimePoint <- NULL
lineList <- NULL
spatialLineList <- NULL
nearestSegement <- NULL
roadSegmentData <- NULL
trafficData <- NULL

#Load dataset as previously calculated
#load("finalDataEnvironment.RData")




#
# Data Plotting
#

#Plot all crimes by type within Chicago
chicago <- get_map(location = 'chicago', zoom = 10)
ggmap(chicago) +
  geom_point(data = combData, mapping = aes(x = combData$LONGITUDE, y = combData$LATITUDE, color = combData$CRIME_TYPE))+
  labs(color = "Crime Type", x = "Longitude", y = "Latitude") +
  ggtitle("Locations of Crime within Chicago")

#
# NOTES AREA
#

#Create a map of illinois
# illinoisMap <- ggplot() +
#   borders(database = "state", region = "illinois", colour = "grey60", fill = "grey60") +
#   ggtitle("Illinois") +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   theme(panel.background = element_blank())
# 
# #Create a map of Chicago
# chicagoMap <- ggplot() +
#   borders(database = "world", colour = "grey60", fill = "grey60") +
#   ggtitle("Probably just Chicago?") +
#   coord_cartesian( 
#     xlim = c(-87.96, -87.5),
#     ylim = c(41.62, 42.05)) +
#   xlab("Longitude") +
#   ylab("Latitude") +
#   theme(panel.background = element_blank())
# 
# #Add information to the Chicago map
# chicagoMap + 
#   geom_point(data = dataset, mapping = aes(x = Longitude, y = Latitude), color = "red") +
#   ggtitle("Information")
# 
# 
# 
# 
# chicagoMap + 
#   geom_point(data = crimeData, mapping = aes(x = crimeData$LONGITUDE, y = crimeData$LATITUDE), color = "red") +
#   ggtitle("Information")
# 
# 