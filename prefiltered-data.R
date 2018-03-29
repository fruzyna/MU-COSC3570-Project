# Data analysis (visualizations) based on already cleaned data

# Necessary Libraries
library(tidyverse)
library(ggmap)

#
# Just Weather
#

months = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
weather <- separate(weatherData, col='DATE', into=c('year','month', 'day'), sep='-')

# Plot average high by month
meanTemp = mean(weather$`MAX_TEMP  (F)`)
meanTemp
weather$high = weather$`MAX_TEMP  (F)`
averageHigh = aggregate(high ~ month, weather, mean)
barplot(averageHigh$high, names.arg=months, main="Average Temperature by Month in Chicago", ylab="Degrees F", xlab="Month")

# Plot average precip by month
avgPrecip = aggregate(`PRECIPITATION(in)` ~ month, weather, mean)
avgPrecip
barplot(avgPrecip$`PRECIPITATION(in)`, names.arg=months, main="Average Precipitation by Month in Chicago", ylab="Inches", xlab="Month")

#
# Weather vs Crime
#

# most common by temperature
aggregate(finalData$CRIME_TYPE, list(finalData$`MAX_TEMP(F)`),
          function(x) { 
            ux <- unique(x) 
            ux[which.max(tabulate(match(x, ux)))]})

# mode
aggregate(`MAX_TEMP(F)` ~ CRIME_TYPE, finalData, function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]})

# delta from mean
deltaMean = aggregate(`MAX_TEMP(F)` ~ CRIME_TYPE, finalData, function(t) {
  delta <- mean(t) - meanTemp})

instances = list()
for(type in deltaMean$CRIME_TYPE) {
  instances[[type]] <- sum(finalData$CRIME_TYPE == type)
}
deltaMean$instances = instances

# plot delta mean temp by crime
deltaMean = deltaMean[deltaMean$instances > 500,]
deltaMean = deltaMean[deltaMean$CRIME_TYPE != 'INTERFERENCE WITH PUBLIC OFFICER',]
deltaMean

par(mar = c(15,4,4,2) + 0.1) # improve margins for long names
barplot(deltaMean$`MAX_TEMP(F)`, names.arg=deltaMean$CRIME_TYPE, las=2, main="Delta Temperature Between Annual Average and Average by Crime in Chicago", ylab="Degrees Above Annual Average (~59 F)")
title(xlab="Crime Type", line=13) # custom x label location
par(mar = c(5,4,4,2) + 0.1) # reset margins


# number of crimes by temperature
crimesPerTemp = table(finalData$`MAX_TEMP(F)`)
plot(crimesPerTemp, type='o', main='Number of Crimes by Temperature', ylab='Crimes', xlab='Temperature (F)')


# crimes by day
crimesPerDay = table(finalData$DATE)
plot(crimesPerDay, type='o', main='Number of Crimes by Day', ylab='Crimes', xlab='Date')

#
# Below data is using combData, because we screwed up
#

# # crimes by day from raw
# combData = read.csv('data/comb_data.csv')
# crimesPerDay2 = table(combData$DATE)
# plot(crimesPerDay2, type='o', main='Number of Crimes by Day', ylab='Crimes', xlab='Date')
# 
# 
# # delta from mean
# deltaMean2 = aggregate(`MAX_TEMP...F.` ~ CRIME_TYPE, combData, function(t) {
#   delta <- mean(t) - meanTemp})
# 
# instances = list()
# for(type in deltaMean2$CRIME_TYPE) {
#   instances[[type]] <- sum(combData$CRIME_TYPE == type)
# }
# deltaMean2$instances = instances
# 
# # plot delta mean temp by crime
# deltaMean2 = deltaMean2[deltaMean2$instances > 500,]
# deltaMean2 = deltaMean2[deltaMean2$CRIME_TYPE != 'INTERFERENCE WITH PUBLIC OFFICER',]
# deltaMean2
# 
# par(mar = c(15,4,4,2) + 0.1) # improve margins for long names
# barplot(deltaMean2$`MAX_TEMP...F.`, names.arg=deltaMean2$CRIME_TYPE, las=2, main="Delta Temperature Between Annual Average and Average by Crime in Chicago", ylab="Degrees Above Annual Average (~59 F)")
# title(xlab="Crime Type", line=13) # custom x label location
# par(mar = c(5,4,4,2) + 0.1) # reset margins

#
# Resume normal data
#

# totals
total_days = nrow(weatherData)
total_crimes = nrow(finalData)

# proportion of days of thunder
thunder_days = length(which(weatherData$THUNDER == 1))
thunder_prop = thunder_days / total_days
thunder_prop

# proportion crimes on days of thunder
thunder_crimes = length(which(finalData$THUNDER == 1))
thunder_crime_prop = thunder_crimes / total_crimes
thunder_crime_prop

# proportion of rainy days
is_rainy_day = weatherData$`PRECIPITATION(in)` > 0
rainy_days = length(which(is_rainy_day))
rainy_prop = rainy_days / total_days
rainy_prop

# proportion crime on rainy days
is_rainy_crime = finalData$`PRECIPITATION(in)` > 0
rainy_crimes = length(which(is_rainy_crime))
rainy_crime_prop = rainy_crimes / total_crimes
rainy_crime_prop

# proportion of heavy rain days
is_heavy_day = weatherData$`PRECIPITATION(in)` > 0.3
heavy_days = length(which(is_heavy_day))
heavy_prop = heavy_days / total_days
heavy_prop

# proportion crime on heavy rain days
is_heavy_crime = finalData$`PRECIPITATION(in)` > 0.3
heavy_crimes = length(which(is_heavy_crime))
heavy_crime_prop = heavy_crimes / total_crimes
heavy_crime_prop

# proportion of clear days
is_clear_day = weatherData$`PRECIPITATION(in)` == 0.0 & is.na(weatherData$THUNDER)
clear_days = length(which(is_clear_day))
clear_prop = clear_days / total_days
clear_prop

# proportion crime on clear days
is_clear_crime = finalData$`PRECIPITATION(in)` == 0.0 & is.na(finalData$THUNDER)
clear_crimes = length(which(is_clear_crime))
clear_crime_prop = clear_crimes / total_crimes
clear_crime_prop

# scaled versions
prop_crime_clear = clear_crime_prop / clear_prop
prop_crime_rainy = rainy_crime_prop / rainy_prop
prop_crime_heavy = heavy_crime_prop / heavy_prop
prop_crime_thunder = thunder_crime_prop / thunder_prop

# draw it
props = c(1, prop_crime_clear, prop_crime_rainy, prop_crime_heavy, prop_crime_thunder)
names = c('Average', 'Clear', 'Rainy', 'Heavy Rain', 'Thunder')
barplot(props, main='Proportion of Crimes by Weather', xlab='Day Type', ylab='Proportion', names.arg=names)

#
# Just Crime
#

# Plot all crimes by type within Chicago
chicago <- get_map(location = 'Chicago', zoom = 10)
ggmap(chicago) +
  geom_point(data = finalData, mapping = aes(x = finalData$LONGITUDE, y = finalData$LATITUDE, color = finalData$CRIME_TYPE))+
  labs(color = "Crime Type", x = "Longitude", y = "Latitude") +
  ggtitle("Locations of Crime within Chicago")

#
# Crime vs Traffic
#

# Finding what traffic crime happens in/around
# Crimes by each congestion level
crime_congestion_counts = table(finalData$CONGESTION_LEVEL)
crime_table = as.data.frame(crime_congestion_counts)
row.names(crime_table) = crime_table$Var1

counts = c(crime_table['FreeFlow', 'Freq'], crime_table['Medium', 'Freq'], crime_table['Heavy', 'Freq'])
names = c('FreeFlow', 'Medium', 'Heavy')
barplot(counts, main='Crimes per Congestion Level', xlab='Congestion Level', ylab='Crimes', names.arg=names)

# Total counts of frequency of each congestion level
total_congestion_counts = table(trafficData$CONGESTION_LEVEL)
total_table = as.data.frame(total_congestion_counts)
row.names(total_table) = total_table$Var1

counts = c(total_table['FreeFlow', 'Freq'], total_table['Medium', 'Freq'], total_table['Heavy', 'Freq'])
barplot(counts, main='Congestion Level Counts', xlab='Congestion Level', ylab='Counts', names.arg=names)

freeflow = total_table['FreeFlow','Freq']
heavy = total_table['Heavy','Freq']
medium = total_table['Medium','Freq']
sum = freeflow + heavy + medium

# Proportion to scale crimes by
fp = 1- freeflow / sum
hp = 1- heavy / sum
mp = 1- medium / sum

crime_table['FreeFlow', 'Freq'] = fp * crime_table['FreeFlow', 'Freq']
crime_table['Heavy', 'Freq'] = hp * crime_table['Heavy', 'Freq']
crime_table['Medium', 'Freq'] = mp * crime_table['Medium', 'Freq']

# Scale and display
counts = c(crime_table['FreeFlow', 'Freq'], crime_table['Medium', 'Freq'], crime_table['Heavy', 'Freq'])
names = c('FreeFlow', 'Medium', 'Heavy')
barplot(counts, main='Crimes per Congestion Level (Scaled)', xlab='Congestion Level', ylab='Crimes', names.arg=names)

#
# EVERYTHING!
#

qplot(`MAX_TEMP(F)`, `PRECIPITATION(in)`, data=finalData, facets=CONGESTION_LEVEL~CRIME_TYPE)