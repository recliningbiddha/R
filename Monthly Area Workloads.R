# clear workspace
rm(list=ls())
# Load required libraries
library("gridExtra")
# library("dplyr")
library("qcc")
library("formattable")

# Set directories
anaes.data.directory <- "~/MEGAsync/QA\ Data/Data/Anaesthetic\ Data"
functions.directory  <- "~/MEGAsync/QA\ Data/R Functions"
pacu.data.directory  <- "~/MEGAsync/QA\ Data/Data/PACU\ Data"
output.directory     <- "~/MEGAsync/QA\ Data/Data/Output"

# Load required functions and routines
setwd(functions.directory)
source("Load\ &\ Combine.R")

# Retrieve MOT Data
setwd(anaes.data.directory)
# **consider amend function so pass path and fxn reads files then returns to default path**
# ** also consider using zip files to store the data - perhaps one for mot and one for pac for each time period - then wouldn't need folder per data set but zip file per data set
mot.data <- CombineAll()

##CLEAN DATA
# Modify dates from excel to work in R
# **could this be written as a fxn to be called with the format to convert and format to return**
mot.data$date = as.character(mot.data$Operation.Date)
mot.data$date = as.Date(mot.data$Operation.Date,format = "%d-%b-%Y")
mot.data$Month_Yr <- format(as.Date(mot.data$date), "%Y-%m")
# Correct .PACU.ICU.WARD variable (appears as pacu.icu.ward & icu.pacu.ward - combined into pacu.icu.ward)
mot.data$PACU.ICU.WARD <- replace(mot.data$PACU.ICU.WARD, (mot.data$PACU.ICU.WARD == "HOME" | mot.data$PACU.ICU.WARD == "WARD"), "PACU")
monthlycases <- table(mot.data$Month_Yr, mot.data$PACU.ICU.WARD)[,c("ICU","PACU")]

## Plot and save stacked barplot of MOT monthly activity seperated into ICU and PACU 
# **consider using a function called with filename properties to open the file and close after plot**
## setwd(output.directory)
## filename.prefix <- paste(min(mot.data$Month_Yr), "-", max(mot.data$Month_Yr)," ")
## filename <- paste(filename.prefix, "MOT-throughput-barplot.pdf")
## pdf(filename, height = 7, width =12 )
barplot(t(monthlycases), 
        ylab="# cases/ month", 
        las=2, 
        main=paste("Cases through MOT by month\nSeperated by discharge to ICU or PACU\nFor the period ",min(mot.data$Month_Yr)," to ",max(mot.data$Month_Yr)), 
        legend.text=c("ICU","PACU"),
        args.legend = list(
          x="topright",
          inset=c(-0.04,0),
          bty = "n"
        )
        )
## dev.off()

# Write table of Activity by Specialty
# **These tables need formatting
## setwd(output.directory)
## filename <-paste(filename.prefix, "Activity-by-Specialty.pdf")
## pdf(filename, height=13, width=25)
## grid.table(table(mot.data$Month_Yr, mot.data$Specialty.Desc))
## dev.off()

# Write table of Individual activity
# **This table needs formatting
## setwd(output.directory)
## filename <- paste(filename.prefix, "Activity-by-Anaesthetist.pdf")
## pdf(filename, height=13, width=50)
## grid.table(table(mot.data$Month_Yr, mot.data$Anaes.1.Name))
## dev.off()

# Start analysis of PACU data
setwd(pacu.data.directory)
pacu.data <- CombineAll()
# Normalise dates & accomodate the timestamp field
pacu.data$date <- as.character(pacu.data$In.Recovery.At)
pacu.data$date <- as.Date(pacu.data$In.Recovery.At,format = "%d/%m/%Y  %I:%M:%S %p")
pacu.data$Month_Yr <- format(as.Date(pacu.data$date), "%Y-%m")
pacu.data$Day_Month_Yr <- format(as.Date(pacu.data$date), "%Y-%m-%d")

# Adverse events in ORMIS are:
events.descriptors <- c("ANAES RESP INTERVENT", "BLOOD FLUID LOSS", "CARDIO / RESP ARREST", "HAEMODYNAMIC COMP", "HYPOTHERMIA <36 DEG", "OTHER", "PAIN R/V ANAES CONS", "PERSISTENT PONV", "PROLONGED STAY >2 HR", "PROLONGED UNCONSC", "REACTION", "REINTUB/ VENTILATION", "RESP COMPLICTION", "RETURN TO OR", "UNPLANNED ADMISS ICU", "ANTIEMETICS")

# Use PACU cases from MOT data for totals.
events.adverse <- data.frame(matrix(NA, nrow=length(unique(mot.data$Month_Yr)), ncol=length(events.descriptors)+2))
# Data frame had date as 1st col, then total PACU cases that month as 1 column and event totals as other columns.
names(events.adverse) <- c("date","total.pacu.cases",events.descriptors)
# add dates from mot.data to dates column. 
events.adverse$date <- sort(unique(pacu.data$Month_Yr), decreasing=FALSE)
# add total cases per month through PACU to column
events.adverse$total.pacu.cases <- monthlycases[,"PACU"]

# now to enter event counts to each column - ensure dates match up
# adverse event counts need to be matched by date
for (i in events.descriptors){
  t <- as.data.frame(table(pacu.data$Month_Yr, pacu.data$Answer == i))
  t <- subset(t, t["Var2"]==TRUE)
  events.adverse[,i] <- t["Freq"]
} # End for i loop
# Consider improving by also searching for common strings entered in place of standard phrases

# If no adverse events in a category the column will remain filled with NA - convert these to zero for calculations
events.adverse[is.na(events.adverse)] <- 0

# **Read QCC package documentation. some useful info re p plots and g plots. p plot requires sample size field. 
# Respiratory event = Anaes intervent + Cardio/ Resp Arrest + Reintub + Resp Complication
# Serious Resp event = Anaes intervent + Cario Resp Arrest + Reintub
# V Serous Resp Event = Arrest + Reintub

# Do plots for each event descriptor & write to pdf
## improve by adding warning limits and colour change if exceed bounds
for (i in events.descriptors){
  ## setwd(output.directory)
  ## filename.prefix <- paste(min(mot.data$Month_Yr), "-", max(mot.data$Month_Yr)," ")
  ## filename <- paste(filename.prefix, "MOT-throughput-barplot.pdf")
  ## pdf(filename, height = 7, width =12 )

    qcc(events.adverse[i],
      type="p",
      sizes=events.adverse$total.pacu.cases,
      labels=events.adverse$date,
      axes.las=2,
      data.name = i,
      add.stats=FALSE)
  
  ## dev.off()
  
  } # end for i

# Use qcc g plot for plot of days between events
# code for calculating days between infrequent events
# use for return to or, cardiac arrest, reintubation, unplanned admiss icu - at this stage might need to comsider large data set so longer time span analysed
# pacu.data[which(pacu.data$Answer==events.descriptors[2]),"Day_Month_Yr"]

# Reset working directory to functions.directory so history etc saved there on exit.
setwd(functions.directory)
