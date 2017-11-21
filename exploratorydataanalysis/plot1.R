# clear workspace
rm(list=ls())
# Assume working directory set to the file containing data zip file before calling script
# Detect filenames ending with .zip and collect name of 2nd (corosponds with assignment file on my system)
data_zipfile <- dir(pattern=".zip")[2]
# List and record the 1st filename in the zip file
filename <- unzip(data_zipfile, list=TRUE)[1]
# read the data file the file direct from zipfile
household_data <- read.table(unz(data_zipfile, filename),sep=";",na.strings="?", header=TRUE)
# Clean up dates & times
household_data$Date <- as.Date(household_data$Date, format= "%d/%m/%Y")
# extract date range to work with
household_data <- subset(household_data, subset=(Date >="2007-02-01" & Date <= "2007-02-02"))
household_data$datetime <- paste(as.Date(household_data$Date), household_data$Time)
household_data$datetime <- as.POSIXct(household_data$datetime)

# Convert factors to numerics
household_data$Global_active_power <- as.numeric(household_data$Global_active_power)
household_data$Global_reactive_power <- as.numeric(household_data$Global_reactive_power)
household_data$Voltage <- as.numeric(household_data$Voltage)
household_data$Global_intensity <- as.numeric(household_data$Global_intensity)
household_data$Sub_metering_1 <- as.numeric(household_data$Sub_metering_1)
household_data$Sub_metering_2 <- as.numeric(household_data$Sub_metering_2)
household_data$Sub_metering_3 <- as.numeric(household_data$Sub_metering_3)

#plot 1
hist(household_data$Global_active_power, 
     col="red",
     xlim=c(0,6), 
     ylim=c(0,1200), 
     main = "Global Active Power", 
     xlab = "Global Active Power (kilowatts)")

# write plot to file
dev.copy(png, file="plot1.png", height=480, width=480)
dev.off()
