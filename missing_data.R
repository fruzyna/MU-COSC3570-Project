library(mice)
library(VIM)

traffic <- read.csv('data/traffic_historical.csv')
#summary(traffic)

# View which data is missing
md.pattern(traffic)
#traffic_missing <- aggr(traffic, numbers=TRUE, sortVars=TRUE, labels=names(traffic), cex_axis=1.5, gap=3)


crime <- read.csv('data/crimes.csv')
md.pattern(crime)