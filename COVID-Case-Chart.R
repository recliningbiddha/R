# Written by CHRISTOPHER STONELL, May 2020
# SCRIPT GENERATES CONTROL CHART FOR QLD COVID-19 CASE DATA

#clear workspace
rm(list=ls())

# load libraries
library(qicharts2)
library(jsonlite)
library(ggplot2)

# load data from json feed url - readJSON doesnt work directly, so download.file use as intermediate step
url <- 'https://services1.arcgis.com/vHnIGBHHqDR6y0CR/arcgis/rest/services/COVID19_Time_Series/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json'
file <- 'JSONdownload.json'

download.file(url, file)
jsonrawdata <- fromJSON(file)

# Other Data Sources that I've used:
git_data_za <- read.csv(url('https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv'), sep=",",header=TRUE) # ZA data on a GIT repository from U Pretoria
git_data <- read.csv(url('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/05-13-2020.csv'), sep=",",header=TRUE) # US data on a GIT repository
owid <- read.csv(url('https://covid.ourworldindata.org/data/owid-covid-data.csv'), sep=",", header = TRUE) # World data from Our World in Data

# extract relevant data from JSON data
ausdata<-jsonrawdata$features$attributes

#start calculating variables
Date = as.POSIXlt.POSIXct(unlist(ausdata['Date']/1000))
Total <- ausdata$QLD
Total[is.na(Total)][1] <- 0 # replacing first element as zero if NA
Total <- zoo::na.locf(Total) # replace all other NA with previous non-NA value
New <- c(0,diff(Total)) # Pad 1st row of 'New' column with 0 so same length as other vars

# make data.frame
df <- data.frame(Date, Total, New)

# plot chart using last 30 days
print(qic(New, x = as.Date(Date, tryFormats = c("%d/%m/%Y")), data = df[(nrow(df)-30):nrow(df),], chart = 'c', part = 83, title = 'Qld Daily Confirmed Cases', ylab = 'Count', xlab = 'Week'))

# Compile total case charts for Aus and ZA
aus <- ggplot(ausdata, aes(x=as.Date(as.POSIXlt.POSIXct(unlist(ausdata['Date']/1000))))) + geom_line(aes(y=Total_Cases), size=1.2) + geom_line(aes(y=QLD), color = "maroon4", size=1.2) + geom_line(aes(y=NSW), color="skyblue1", size=1.2) + geom_line(aes(y=VIC), color = "blueviolet",size=1.2)
za <- ggplot(git_data_za, aes(x=as.Date(date, format="%d-%m-%Y"))) + geom_line(aes(y=total), size=1.2) + geom_line(aes(y=KZN), color="green4", size=1.2) + geom_line(aes(y=GP), color="blue4", size=1.2) + geom_line(aes(y=EC), color="darkorchid4", size=1.2) + theme(legend.position="right")

# These charts can be displayed by entering 'aus' or 'za'
