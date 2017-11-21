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


#plot 3
with(household_data, {
  plot(Sub_metering_1 ~ datetime,
       type = "l",
       ylab = "Global Active Power (kilowatts)",
       xlab = "")
  lines(Sub_metering_2 ~ datetime, col="red")
  lines(Sub_metering_3 ~ datetime, col = "blue")
})
legend("topright", 
       col=c("black", "red", "blue"),
       lty = 1,
       lwd= 2,
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

# write plot to file
dev.copy(png, file="plot3.png", height=480, width=480)
dev.off()

