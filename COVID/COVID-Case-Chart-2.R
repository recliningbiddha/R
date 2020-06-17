# Written by CHRISTOPHER STONELL, May 2020, Updated June 2020
# SCRIPT GENERATES CHARTS FOR COVID-19 CASE DATA

#clear workspace
rm(list=ls())

# Define Functions
# ################
# load_packages function: install and load multiple R packages.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
load_packages <- function(pkg){
new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
}
# End Define Functions

# load libraries
#packages <- c("ggplot2", "qicharts2", "jsonlite", "tidyr", "ggpubr")
packages <- c("ggplot2", "ggpubr", "tidyr")
load_packages(packages)

# load data from json feed url - readJSON doesnt work directly, so download.file use as intermediate step
print("Downloading Australia data")
url <- 'https://services1.arcgis.com/vHnIGBHHqDR6y0CR/arcgis/rest/services/COVID19_Time_Series/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json'
# download.file(url, file, verbose=FALSE) # now redundant line used to download url to file then in next line re-read file to var, now read direct from url
jsonrawdata <- jsonlite::fromJSON(url(url))
# need data source for transmission - ? scrape webpage https://www.qld.gov.au/health/conditions/health-alerts/coronavirus-covid-19/current-status/statistics#caseoverview

# Other Data Sources
print("Downloading South Africa Data")
git_data_za <- read.csv(url('https://raw.githubusercontent.com/dsfsi/covid19za/master/data/covid19za_provincial_cumulative_timeline_confirmed.csv'), sep=",",header=TRUE) # ZA data on a GIT repository from U Pretoria
print("Downloading World Data")
git_data <- read.csv(url('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/05-13-2020.csv'), sep=",",header=TRUE) # US data on a GIT repository
owid <- read.csv(url('https://covid.ourworldindata.org/data/owid-covid-data.csv'), sep=",", header = TRUE) # World data from Our World in Data

#OWID data json dowload - duplicates data in owid .csv - not used in calculations - included for interest and reference.
url <- 'https://covid.ourworldindata.org/data/owid-covid-data.json'
owid_json_rawdata <- jsonlite::fromJSON(url(url))
# end of duplicate OWID data download

# Population data defining constants
population <- vector(mode="list", length=20)
names(population) <- c("NSW","VIC","QLD","SA","WA","TAS","NT","ACT","Total_Aus","EC","FS","GP","KZN","LP","MP","NC","NW","WC","total_ZA")
population$NSW <- 7317500; population$VIC <- 5640900; population$QLD <- 4599400; population$SA<- 1659800; population$WA <- 2366900; population$TAS <- 511000; population$NT <- 231200; population$ACT <- 366900
population$EC <- 6712276; population$FS <- 2887465; population$GP <- 15176116; population$KZN <- 11289086; population$LP <- 5982584; population$MP <- 4592187; population$NC <- 1263875; population$NW <- 4027160; population$WC <- 6844272

# extract relevant data from JSON data
ausdata<-jsonrawdata$features$attributes

#start calculating variables
ausdata$Date <- as.POSIXlt.POSIXct(unlist(ausdata['Date']/1000))
ausdata[1,is.na(ausdata[1,])] <- 0 # replacing first element as zero if NA
ausdata <- zoo::na.locf(ausdata) # replace NA's with previous number in column

# make data.frame for QLD data
df <- data.frame(ausdata$Date, ausdata$QLD, c(0,diff(ausdata$QLD))) # df(Date, Total cases, New cases each day) New cases padded with 0 in first row to make column same length for plotting
colnames(df) <- c("Date", "QLD", "New")

# plot QI chart using last 30 days
qi <- qicharts2::qic(New, x = as.Date(Date, tryFormats = c("%d/%m/%Y")), data = df[(nrow(df)-30):nrow(df),], chart = 'c', part = 83, title = 'Qld Daily Confirmed Cases', ylab = 'Count', xlab = 'Week')

# Compute data for Aus charts
#Calculate the new cases for each State
ausdata$NSWnew <- c(0,diff(ausdata$NSW))
ausdata$VICnew <- c(0,diff(ausdata$VIC))
ausdata$QLDnew <- c(0,diff(ausdata$QLD))
ausdata$SAnew <- c(0,diff(ausdata$SA))
ausdata$WAnew <- c(0,diff(ausdata$WA))
ausdata$TASnew <- c(0,diff(ausdata$TAS))
ausdata$NTnew <- c(0,diff(ausdata$NT))
ausdata$ACTnew <- c(0,diff(ausdata$ACT))
ausdata$Totalnew <- ausdata$NSWnew + ausdata$VICnew + ausdata$QLDnew + ausdata$SAnew + ausdata$WAnew + ausdata$TASnew + ausdata$NTnew + ausdata$ACTnew

#Pivot data for ggplot
ausdata_long <- pivot_longer(ausdata, c("NSW","VIC","QLD","SA","WA","TAS","NT","ACT","Total_Cases"), names_to = "State", values_to = "Cases")
ausdata_long_new <- pivot_longer(ausdata, c("NSWnew", "VICnew", "QLDnew","SAnew","WAnew","TASnew","NTnew","ACTnew", "Totalnew"), names_to = "State", values_to = "Cases")
ausdata_long_new_30d <- pivot_longer(tail(ausdata, n=30), c("NSWnew", "VICnew", "QLDnew","SAnew","WAnew","TASnew","NTnew","ACTnew"), names_to = "State", values_to = "Cases")
ausdata_test <- pivot_longer(ausdata, c("Total_Tests"), names_to = "Total", values_to = "Tests")

# Plot individual Charts
#Total cases by state
aus_total <- ggplot(ausdata_long, aes(x = as.Date(Date), y = Cases, group = State, color = State)) + geom_line(size=1.2) + labs(x = "Date", y = "Total Cases", title = "Total cases by State")
# Total new cases by State
aus_new <- ggplot(ausdata_long_new_30d, aes(x=as.Date(Date))) + geom_bar(aes(y= Cases, group = State, color = State, fill = State), data = subset(ausdata_long_new_30d, State != "Totalnew"), position="stack", stat="identity") + geom_line(aes(y=Cases), data = subset(ausdata_long_new_30d, State == "Totalnew", size = 1.5)) + labs(x = "Date", y = "New Cases / day", title = "New cases / day by State for last 30d")
# National Total Tests vs +ve cases
aus_tests <- ggplot(tail(ausdata_test, n=30), aes(x=as.Date(Date))) + geom_bar(aes(y=Tests/100), stat="identity") + geom_line(aes(y=(Total_Cases))) + labs(x = "Date", y = "National Tests x 10000", title = "National Total Tests vs Total Positive Cases")
# Qld total tests vs +ve cases
aus_tests_qld <- ggplot(tail(ausdata_test, n=30), aes(x=as.Date(Date))) + geom_bar(aes(y=QLD_Tests/100), stat="identity") + geom_line(aes(y=(QLD))) + labs(x = "Date", y = "Qld Total Tests x 1000", title = "Qld Total Tests vs Total Positive Cases")
# Total cases per 100,000 population by State
aus_states <- ggplot(ausdata_long, aes(x = as.Date(Date))) +
		geom_line(aes(y = Cases/population$ACT*100000,  group = State, color = State), data=subset(ausdata_long, State == "ACT"), size=1.2) +
		geom_line(aes(y = Cases/population$NSW*100000, group = State, color = State), data=subset(ausdata_long, State == "NSW"), size=1.2) +
		geom_line(aes(y = Cases/population$NT *100000,  group = State, color = State), data=subset(ausdata_long, State == "NT"), size=1.2) +
		geom_line(aes(y = Cases/population$QLD*100000, group = State, color = State), data=subset(ausdata_long, State == "QLD"), size=1.2) +
		geom_line(aes(y = Cases/population$SA *100000, group = State, color = State), data=subset(ausdata_long, State == "SA"), size=1.2) +
		geom_line(aes(y = Cases/population$TAS*100000,  group = State, color = State), data=subset(ausdata_long, State == "TAS"), size=1.2) +
		geom_line(aes(y = Cases/population$VIC*100000, group = State, color = State), data=subset(ausdata_long, State == "VIC"), size=1.2) +
		geom_line(aes(y = Cases/population$WA *100000, group = State, color = State), data=subset(ausdata_long, State == "WA"), size=1.2) +
		geom_line(aes(y = Cases/22693690*100000,group = State, color = State), data=subset(ausdata_long, State == "Total_Cases"), size=1.2) +
		labs(x = "Date", y = "Cases per 100 000 popln", title = "Cases per 100 000 population by state")
# Transmission Sources

# Combine charts into one display
aus <- ggpubr::ggarrange(aus_total, aus_states, aus_new, aus_tests, aus_tests_qld)

# Compute data for ZA charts
# EC = Eastern Cape, FS = Free State, GP = Gauteng, KZN = Kwa-Zulu Natal, LP = Limpopo, MP = Mpumalanga, NC = Northern Cape, NW = North West Province, WC = Western Cape,
git_data_za$ECnew <- c(0,diff(git_data_za$EC))
git_data_za$FSnew <- c(0,diff(git_data_za$FS))
git_data_za$GPnew <- c(0,diff(git_data_za$GP))
git_data_za$KZNnew <- c(0,diff(git_data_za$KZN))
git_data_za$LPnew <- c(0,diff(git_data_za$LP))
git_data_za$MPnew <- c(0,diff(git_data_za$MP))
git_data_za$NCnew <- c(0,diff(git_data_za$NC))
git_data_za$NWnew <- c(0,diff(git_data_za$NW))
git_data_za$WCnew <- c(0,diff(git_data_za$WC))
git_data_za$Totalnew <- git_data_za$ECnew + git_data_za$FSnew + git_data_za$GPnew + git_data_za$KZNnew + git_data_za$LPnew + git_data_za$MPnew + git_data_za$NCnew + git_data_za$NWnew + git_data_za$WCnew

# Pivot data for ggplot
zadata_long <- pivot_longer(git_data_za, c("EC","FS","GP","KZN","LP","MP","NC","NW","WC","total"), names_to = "Province", values_to = "Cases")
zadata_long$date <- as.Date(zadata_long$date, format = "%d-%m-%Y")
zadata_long_new30d <- pivot_longer(tail(git_data_za, n=30), c("ECnew","FSnew","GPnew","KZNnew","LPnew","MPnew","NCnew","NWnew","WCnew","Totalnew"), names_to = "Province", values_to = "Cases")
zadata_long_new30d$date <- as.Date(zadata_long_new30d$date, format = "%d-%m-%Y")
# Unable to do ZA testing charts as don't have testing data
# zadata_test <- pivot_longer(zadata, c("Total_Tests"), names_to = "Total", values_to = "Tests")

# Plot individual charts
za_total <- ggplot(zadata_long, aes(x= date, y = Cases, group = Province, color = Province)) + geom_line(size=1.2) + labs(x = "Date", y = "Total Cases", title = "Total cases by Province")
za_new <- ggplot(zadata_long_new30d, aes(x=date)) + geom_bar(aes(y= Cases, group = Province, color = Province, fill = Province), data = subset(zadata_long_new30d, Province != "Totalnew"), position="stack", stat="identity") + geom_line(aes(y=Cases), data = subset(zadata_long_new30d, Province == "Totalnew", size = 1.5)) + labs(x = "Date", y = "New Cases / day", title = "New cases / day by Province for last 30d")
# Unable to do ZA testing charts as don't have testing data
#za_tests <- ggplot(tail(ausdata_test, n=30), aes(x=as.Date(Date))) + geom_bar(aes(y=Tests/1000), stat="identity") + geom_line(aes(y=(Totalnew))) + labs(x = "Date", y = "National Tests x 1000", title = "National Total Tests vs Total Positive Cases")
za_provinces <- ggplot(zadata_long, aes(x= date)) +
		geom_line(aes(y = Cases/population$EC *100000, group = Province, color = Province), data = subset(zadata_long, Province == "EC"), size=1.2) +
		geom_line(aes(y = Cases/population$FS *100000, group = Province, color = Province), data = subset(zadata_long, Province == "FS"), size=1.2) +
		geom_line(aes(y = Cases/population$GP *100000, group = Province, color = Province), data = subset(zadata_long, Province == "GP"), size=1.2) +
		geom_line(aes(y = Cases/population$KZN*100000, group = Province, color = Province), data = subset(zadata_long, Province == "KZN"), size=1.2) +
		geom_line(aes(y = Cases/population$LP *100000, group = Province, color = Province), data = subset(zadata_long, Province == "LP"), size=1.2) +
		geom_line(aes(y = Cases/population$MP *100000, group = Province, color = Province), data = subset(zadata_long, Province == "MP"), size=1.2) +
		geom_line(aes(y = Cases/population$NC *100000, group = Province, color = Province), data = subset(zadata_long, Province == "NC"), size=1.2) +
		geom_line(aes(y = Cases/population$NW *100000, group = Province, color = Province), data = subset(zadata_long, Province == "NW"), size=1.2) +
		geom_line(aes(y = Cases/population$WC *100000, group = Province, color = Province), data = subset(zadata_long, Province == "WC"), size=1.2) +
		geom_line(aes(y = Cases/58775022*100000,group = Province, color = Province), data = subset(zadata_long, Province == "total"), size=1.2) +
		labs (x="Date", y="Cases per 100 000 popln", title = "Total case per 100 000 population by Province")

# Combine charts into one display
za <- ggpubr::ggarrange(za_total, za_new, za_provinces)

# Calculate the per population Charts
popln_data <- na.omit(owid[owid$location==c("Australia","United States","United Kingdom","South Africa","Italy","Brazil","Cuba","Colombia"),c("date","location","total_cases_per_million","total_deaths_per_million")])
popln_data$date <- as.Date(popln_data$date)
popln_cases_plot <- ggplot(popln_data, aes(x=date)) +
  geom_line(aes(y = total_cases_per_million, group = location, color = location), data = subset(popln_data, location == "Australia"), size =1.2) +
  geom_line(aes(y = total_cases_per_million, group = location, color = location), data = subset(popln_data, location == "United Kingdom"), size = 1.2) +
  geom_line(aes(y = total_cases_per_million, group = location, color = location), data = subset(popln_data, location == "United States"), size = 1.2) +
  geom_line(aes(y = total_cases_per_million, group = location, color = location), data = subset(popln_data, location == "South Africa"), size = 1.2) +
  geom_line(aes(y = total_cases_per_million, group = location, color = location), data = subset(popln_data, location == "Italy"), size = 1.2) +
  geom_line(aes(y = total_cases_per_million, group = location, color = location), data = subset(popln_data, location == "Spain"), size = 1.2) +
  geom_line(aes(y = total_cases_per_million, group = location, color = location), data = subset(popln_data, location == "Brazil"), size = 1.2) +
  geom_line(aes(y = total_cases_per_million, group = location, color = location), data = subset(popln_data, location == "Cuba"), size = 1.2) +
  geom_line(aes(y = total_cases_per_million, group = location, color = location), data = subset(popln_data, location == "Colombia"), size = 1.2) +
  labs(x="Date", y="Cases per million population", title="Total cases per million population by country")
popln_deaths_plot <- ggplot(popln_data, aes(x=date)) +
  geom_line(aes(y = total_deaths_per_million, group = location, color = location), data = subset(popln_data, location == "Australia"), size =1.2) +
  geom_line(aes(y = total_deaths_per_million, group = location, color = location), data = subset(popln_data, location == "United Kingdom"), size = 1.2) +
  geom_line(aes(y = total_deaths_per_million, group = location, color = location), data = subset(popln_data, location == "United States"), size = 1.2) +
  geom_line(aes(y = total_deaths_per_million, group = location, color = location), data = subset(popln_data, location == "South Africa"), size = 1.2) +
  geom_line(aes(y = total_deaths_per_million, group = location, color = location), data = subset(popln_data, location == "Italy"), size = 1.2) +
  geom_line(aes(y = total_deaths_per_million, group = location, color = location), data = subset(popln_data, location == "Spain"), size = 1.2) +
  geom_line(aes(y = total_deaths_per_million, group = location, color = location), data = subset(popln_data, location == "Brazil"), size = 1.2) +
  geom_line(aes(y = total_deaths_per_million, group = location, color = location), data = subset(popln_data, location == "Cuba"), size = 1.2) +
  geom_line(aes(y = total_deaths_per_million, group = location, color = location), data = subset(popln_data, location == "Colombia"), size = 1.2) +
  labs(x="Date", y="Deaths per million population", title = "Total deaths per million population by country")
country_plots <- ggpubr::ggarrange(popln_cases_plot, popln_deaths_plot)

# These charts can be displayed by entering 'aus' or 'za'
print("Display charts by entering 'aus', 'za', 'qi', 'country_plots'")

# display Aus chart and exit
print(aus)
