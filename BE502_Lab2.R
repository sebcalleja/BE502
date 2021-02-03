library(tidyr)
library(devtools)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(plyr)
library(data.table)
##-------------------------------------------------------------------------------------
#Attempt of Lab2 functions
add_missing_data<-function(data,gauge_col="gaugeId",date_col=c("readingDate", "month"),rain_col="rainAmount",create_col="createdDate",info_cols=c("lng","lat", "gaugeType")){
  dates=sort(unique(as.character(data[[date_col]])))
  data_full=crossing(gaugeId=unique(as.character(data$gaugeId)),readingDate=dates)
  data_full$default=0
  data_full=merge(data_full, data[,c(gauge_col,date_col,rain_col)], by=c(gauge_col,date_col), all.x=T)
  data_full$rainAmount=apply(data_full[,c(rain_col, "default")], 1,function(x) sum(x, na.rm=T))
  gauge_info=subset(data[,c(gauge_col,info_cols,create_col)],!duplicated(data$gaugeId))
  data_full=merge(data_full, gauge_info, by=gauge_col)
  data_full=data_full[as.character(data_full$createdDate)<=as.character(data_full$readingDate),]
  return(data_full)
}

#mean_diff<-function(time1, time2, data=data,gauge_col="gaugeId",date_col="readingDate",rain_col="rainAmount"){
#  data1=data[grep(time1,as.character(data[[date_col]])),]
#  data2=data[grep(time2,as.character(data[[date_col]])),]
#  gauges=intersect(data1[[gauge_col]],data2[[gauge_col]])
#  data1=data1[is.element(data1[[gauge_col]],gauges),]
#  data2=data2[is.element(data2[[gauge_col]],gauges),]
#  rain1=mean(data1[[rain_col]])
#  rain2=mean(data2[[rain_col]])
#  ret=c(diff=rain1-rain2,rain1=rain1,rain2=rain2)
#  names(ret)=c("diff",paste(time1,"mean",sep=" "),paste(time2,"mean",sep=" "))
#  return(ret)
#}

boxplot<-function(time1, time2, data=data,gauge_col="gaugeId",date_col="readingDate",rain_col="rainAmount", month="month"){
  data1=data[grep(time1,as.character(data[[month]])),]
  data2=data[grep(time2,as.character(data[[month]])),]
  gauges=intersect(data1[[gauge_col]],data2[[gauge_col]])
  data1=data1[is.element(data1[[gauge_col]],gauges),]
  data2=data2[is.element(data2[[gauge_col]],gauges),]
  rain1=data1[[rain_col]]
  rain2=data2[[rain_col]]
  ret1=c(rain1=rain1, rain2=rain2)
  box_plot <- ggplot(data, aes(x = month, y = rain_col, fill = month)) + 
    geom_boxplot() + ggtitle ("Monthly Rain Amount") + 
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "none")
  return(box_plot)
}

data=read.delim("/Users/sebastiancalleja/tucson_rain.txt", header=T, sep="\t", stringsAsFactors=F)
data=data[as.character(data$quality)=="Good",]
data=data[as.character(data$createdDate)<"2018",]
dates=apply(data,1, function(x) {strsplit(x["readingDate"],"-")})
data=cbind(data, t(as.data.frame(dates, row.names=c("year", "month", "day"))))
data_full=add_missing_data(data)

#example call
boxplot("2018-01", "2019-01", data)
##-----------------------------------------------------------------------------------
# Boxplot of Monthly Data
data=read.delim("/Users/sebastiancalleja/tucson_rain.txt", header=T, sep="\t", stringsAsFactors=F)
data=data[as.character(data$quality)=="Good",]
data=data[as.character(data$createdDate)<"2018",]
dates=apply(data,1, function(x) {strsplit(x["readingDate"],"-")})
data=cbind(data, t(as.data.frame(dates, row.names=c("year", "month", "day"))))
data_2018=data[data$year==2018,]
data_2019=data[data$year==2019,]
gauges=intersect(data_2018$gaugeId,data_2019$gaugeId)
data_2018=data_2018[is.element(data_2018$gaugeId,gauges),]
data_2019=data_2019[is.element(data_2019$gaugeId,gauges),]
data_2018=data.frame(data_2018)
data_2019=data.table(data_2019)

boxplot_2018 <- ggplot(data_2018, aes(x = month, y = rainAmount, fill = month)) + 
  geom_boxplot() + ggtitle ("Monthly Rain Amount 2018") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

boxplot_2019 <- ggplot(data_2019, aes(x = month, y = rainAmount, fill = month)) + 
  geom_boxplot() + ggtitle ("Monthly Rain Amount 2019") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")

boxplot_2018
boxplot_2019
##----------------------------------------------------------------------------------


