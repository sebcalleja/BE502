library(tidyverse)
library(tidyr)
library(lubridate)
library(plyr)

##------------------------------------------------------------------------------
#Pre-Processing of the Data

data=read.delim("/Users/sebastiancalleja/tucson_rain.txt", sep='\t')

data=data[tolower(as.character(data$quality))=="good",]

dates=apply(data,1, function(x) {strsplit(x["readingDate"],"-")})

data=cbind(data, t(as.data.frame(dates, row.names=c("year", "month", "day"))))

data_2018 <- data[tolower(as.character(data$year)) == "2018",]

data_2019 <- data[tolower(as.character(data$year)) == "2019",]

gauges=intersect(data_2018$gaugeId,data_2019$gaugeId)

data_2018=data_2018[is.element(data_2018$gaugeId,gauges),]

data_2019=data_2019[is.element(data_2019$gaugeId,gauges),]

month_2018 <- data_2018 %>% group_by(month)

month_2018_mean <- month_2018 %>% summarise(total = mean(rainAmount))

month_2019 <- data_2019 %>% group_by(month)

month_2019_mean <- month_2019 %>% summarise(total = mean(rainAmount))

Merged_month_mean <- merge(month_2018_mean, month_2019_mean, by = "month")

month_mean_table <- rename(Merged_month_mean, c("mean_2018" = "total.x", 
                                              "mean_2019" = "total.y"))



##-------------------------------------------------------------------------------
#Creating the function 


mean_diff <- function(x,y){


  if (x == "2018-01") 
    x == month_mean_table [1,2]
  if (x == "2018-02") 
    x = month_mean_table [2,2]
  if (x == "2018-03") 
    x = month_mean_table [3,2]
  if (x == "2018-04") 
    x = month_mean_table [4,2]
  if (x == "2018-05") 
    x = month_mean_table [5,2]
  if (x == "2018-06") 
    x = month_mean_table [6,2]
  if (x == "2018-07") 
    x = month_mean_table [7,2]
  if (x == "2018-08") 
    x = month_mean_table [8,2]
  if (x == "2018-09") 
    x = month_mean_table [9,2]
  if (x == "2018-10") 
    x = month_mean_table [10,2]
  if (x == "2018-11") 
    x = month_mean_table [11,2]
  if (x == "2018-12") 
    x = month_mean_table [12,2]
  
  if (y == "2019-01") 
    y == month_mean_table [1,3]
  if (y == "2019-02") 
    y = month_mean_table [2,3]
  if (y == "2019-03") 
    y = month_mean_table [3,3]
  if (y == "2019-04") 
    y = month_mean_table [4,3]
  if (y == "2019-05") 
    y = month_mean_table [5,3]
  if (y == "2019-06") 
    y = month_mean_table [6,3]
  if (y == "2019-07") 
    y = month_mean_table [7,3]
  if (y == "2019-08") 
    y = month_mean_table [8,3]
  if (y == "2019-09") 
    y = month_mean_table [9,3]
  if (y == "2019-10") 
    y = month_mean_table [10,3]
  if (y == "2019-11") 
    y = month_mean_table [11,3]
  if (y == "2019-12") 
    y = month_mean_table [12,3]
  
  abs(x-y)
}
##-----------------------------------------------------------------------------------
# Function User Input (example)
mean_diff("2018-01", "2019-01")
