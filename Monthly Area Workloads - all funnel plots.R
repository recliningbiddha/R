# WRITTEN BY CHRISTOPHER STONELL 2017
# GENERATES ACHS INDICATORS AND CONTROL CHARTS FOR PACU INDICATORS
# PLEASE ACKNOWEDGE THE SOURCE IF YOU CHOOSE TO USE THIS CODE.

# clear workspace
rm(list=ls())

# Load required libraries
library("gridExtra")
library("devtools")
library("qcc")
library("formattable")
library("htmltools")
library("webshot")
library("plyr")
library("knitr")
library(ggplot2)
library(digest) # for anonymise.R function to hash MRN

# Set directories
anaes.data.directory <- "~/MEGAsync/QA\ Data/Data/Anaesthetic\ Data"
functions.directory  <- "~/MEGAsync/QA\ Data/R Functions"
pacu.data.directory  <- "~/MEGAsync/QA\ Data/Data/PACU\ Data"
output.directory     <- "~/MEGAsync/QA\ Data/Data/Output"

# Load required functions and routines
source_url("https://raw.githubusercontent.com/vxoli/R/master/Load%20%26%20Combine.R")
source_url("https://raw.githubusercontent.com/vxoli/R/master/export_formattable.R")
source_url("https://raw.githubusercontent.com/vxoli/R/master/anonymise.R")
# source("~/MEGAsync/QA Data/R Functions/anonymise.R")
#setwd(functions.directory)
#source("Load\ &\ Combine.R")

# Retrieve MOT Data
setwd(anaes.data.directory)
# **consider amend function so pass path and fxn reads files then returns to default path**
# ** also consider using zip files to store the data - perhaps one for mot and one for pac for each time period - then wouldn't need folder per data set but zip file per data set
mot.data <- data.frame(CombineAll())

# Drop Patient Name fields from data.frame
mot.data <- mot.data[, -which(names(mot.data) %in% c("Patient.First.Name", "Patient.Last.Name"))]
# Anonymise MRN with hash function
cols_to_anon <- c("MRN")
mot.data[,cols_to_anon] <- anonymise(mot.data,cols_to_anon)

# Modify dates from excel to work in R
# **could this be written as a fxn to be called with the format to convert and format to return**
mot.data$date = as.character(mot.data$Operation.Date)
mot.data$date = as.Date(mot.data$Operation.Date,format = "%d-%b-%Y")
mot.data$Month_Yr <- format(as.Date(mot.data$date), "%Y-%m")

# Correct $PACU.ICU.WARD variable (appears as pacu.icu.ward, icu.pacu.ward & pacu.ward.icu) - combined into pacu.icu.ward
mot.data$PACU.ICU.WARD[is.na(mot.data$PACU.ICU.WARD)] <- mot.data$ICU.PACU.WARD[is.na(mot.data$PACU.ICU.WARD)]
mot.data$PACU.ICU.WARD[is.na(mot.data$PACU.ICU.WARD)] <- as.character(mot.data$PACU.WARD.ICU[is.na(mot.data$PACU.ICU.WARD)])
mot.data <- mot.data[, -which(names(mot.data) %in% c("ICU.PACU.WARD", "PACU.WARD.ICU"))]


# Correct .PACU.ICU.WARD variable (appears as pacu.icu.ward & icu.pacu.ward - combined into pacu.icu.ward)
mot.data$PACU.ICU.WARD <- replace(mot.data$PACU.ICU.WARD, (mot.data$PACU.ICU.WARD == "HOME" | mot.data$PACU.ICU.WARD == "WARD"), "PACU")
monthlycases <- table(mot.data$Month_Yr, mot.data$PACU.ICU.WARD)[,c("ICU","PACU")]

# Clean up names in data
# Correct some duplicate names and remove suffex - Anaes Consultant from some
levels(mot.data$Anaes.1.Name) <- c(levels(mot.data$Anaes.1.Name), "SLYKERMAN,Julia", "HUANG,Dennis", "SINGH,Raman")
mot.data$Anaes.1.Name[mot.data$Anaes.1.Name == "COLLARD,Caroline - Anaes Cons"] <- "COLLARD,Caroline"
mot.data$Anaes.1.Name[mot.data$Anaes.1.Name == "SLYKERMAN,Julia - Anaes Consultant"] <- "SLYKERMAN,Julia"
mot.data$Anaes.1.Name[mot.data$Anaes.1.Name == "HUANG,Dennis - Anaes Consultant"] <- "HUANG,Dennis"
mot.data$Anaes.1.Name[mot.data$Anaes.1.Name == "SINGH,Raman - Anaes Consultant"] <- "SINGH,Raman"


## Plot and save stacked barplot of MOT monthly activity seperated into ICU and PACU 
# **consider using a function called with filename properties to open the file and close after plot**
setwd(output.directory)
filename.prefix <- paste(min(mot.data$Month_Yr), "-", max(mot.data$Month_Yr)," ")
filename <- paste(filename.prefix, "MOT-throughput-barplot.pdf")
pdf(filename, height = 7, width =12 )
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
dev.off()

# Write table of Activity by Specialty sorted by specialty
# **These tables need formatting
## setwd(output.directory)
## filename <-paste(filename.prefix, "Activity-by-Specialty.pdf")
## pdf(filename, height=13, width=25)
## table(mot.data$Month_Yr, mot.data$Specialty.Desc)[,order(colnames(table(mot.data$Month_Yr, mot.data$Specialty.Desc)))][,1:5]
## dev.off()

# Write table of Individual activity sorted by surname
# **This table needs formatting
## setwd(output.directory)
## filename <- paste(filename.prefix, "Activity-by-Anaesthetist.pdf")
## pdf(filename, height=13, width=50)
## table(mot.data$Month_Yr, droplevels(mot.data$Anaes.1.Name, registrars))[,order(colnames(table(mot.data$Month_Yr, droplevels(mot.data$Anaes.1.Name, registrars))))][,32:35]
## dev.off()

# Start analysis of PACU data
setwd(pacu.data.directory)
pacu.data <- data.frame(CombineAll())
# Drop patient names from pacu.data
pacu.data <- pacu.data[, -which(names(pacu.data) %in% c("Patient.First.Name", "Patient.Last.Name"))]
# Anonymise MRN with hash function
cols_to_anon <- c("MRN")
pacu.data[,cols_to_anon] <- anonymise(pacu.data,cols_to_anon)

# Normalise dates & accomodate the timestamp field
# pacu.data$date <- as.character(pacu.data$In.Recovery.At) - do i need this line???
pacu.data$date <- as.Date(pacu.data$In.Recovery.At,format = "%d/%m/%Y  %I:%M:%S %p")
pacu.data$Month_Yr <- format(as.Date(pacu.data$date), "%Y-%m")
pacu.data$Day_Month_Yr <- format(as.Date(pacu.data$date), "%Y-%m-%d")
pacu.data$Answer <- as.character(pacu.data$Answer)

# Adverse events in ORMIS are:
events.descriptors <- c("ANAES RESP INTERVENT", "ANAES RESP INTERVENTION", "BLOOD FLUID LOSS", "CARDIO / RESP ARREST", "HAEMODYNAMIC COMP", "HYPOTHERMIA <36 DEG", "OTHER", "PERSISTENT PONV", "PROLONGED STAY >2 HR", "PROLONGED UNCONSC", "REACTION", "REINTUB/ VENTILATION", "RESP COMPLICATION", "RETURN TO OR", "UNPLANNED ADMISSION ICU", "ANTIEMETICS", "ANAESTH R/V- PAIN", "PAIN R/V ANAES CONS")

# Use PACU cases from MOT data for pacu total cases per month
events.adverse <- data.frame(matrix(NA, nrow=length(unique(mot.data$Month_Yr)), ncol=length(events.descriptors)+2))
# Data frame had date as 1st col, then total PACU cases per month as 1 column and event totals as other columns.
names(events.adverse) <- c("date","total.pacu.cases",events.descriptors)
# add dates from mot.data to dates column. 
events.adverse$date <- sort(unique(pacu.data$Month_Yr), decreasing=FALSE)
# add total cases per month through PACU to column
events.adverse$total.pacu.cases <- monthlycases[,"PACU"]


## !!! I might be able to tidy this section up with the aggregate function or tapply rather than
## generating tables and selecting columns!!!
## e.g. aggregate(mot.data$MRN,by=list(mot.data$Month_Yr,mot.data$Anaes.1.Name), FUN=length)


#First do frequency table by month
# enter event counts to each column - ensure dates match up
for (i in events.descriptors){
  t <- as.data.frame(table(pacu.data$Month_Yr, pacu.data$Answer == i))
  t <- subset(t, t["Var2"]==TRUE)
  events.adverse[,i] <- t["Freq"]
} # End for i loop
# Alternate method to count by date and event - may be quicker/ better - seems to not work correctly ??syntax
# events.adverse[,i] <- with(pacu.data, tapply(pacu.data$Answer==i, pacu.data$Month_Yr, FUN=function(x) length(unique(x))))
# If no adverse events in a category the column will remain filled with NA - convert these to zero for calculations
events.adverse[is.na(events.adverse)] <- 0

## this cleaning of the data needs to occur immediately after read from CSV file with CombineAll()
# combine duplicate columns and drop excess
# Respiatory Intervention
events.adverse$"ANAES RESP INTERVENTION" <- events.adverse$"ANAES RESP INTERVENT" + events.adverse$"ANAES RESP INTERVENTION"
events.adverse$"ANAES RESP INTERVENT" <- NULL
events.descriptors <- names(events.adverse[2:length(events.adverse)])

# Pain reviews
events.adverse$"ANAESTH R/V- PAIN" <- events.adverse$"PAIN R/V ANAES CONS" + events.adverse$"ANAESTH R/V- PAIN"
events.adverse$"PAIN R/V ANAES CONS" <- NULL
events.descriptors <- names(events.adverse[2:length(events.adverse)])
names(events.adverse)[names(events.adverse)=="ANAESTH R/V- PAIN"] <- "Pain Revew" # Change name to Pain Review

# PONV
events.adverse$ANTIEMETICS <- events.adverse$ANTIEMETICS + events.adverse$`PERSISTENT PONV`
events.adverse$`PERSISTENT PONV` <- NULL
names(events.adverse)[names(events.adverse)=="ANTIEMETICS"] <- "PONV" # Change name to PONV
events.descriptors <- names(events.adverse[2:length(events.adverse)])

# Consider also searching for common strings entered in place of standard phrases

# grep useful for searching e.g. grep(string,df$col,ignore.case=TRUE) returns vectors of rows with matches grep(string,df$col,ignore.case=TRUE, value=FALSE/TRUE)

# Tried Using grep to search & calculate the PONV rate in code below
# Turned out the results was very similar to searching for "ANTIEMETICS"
# so Decided to add events.adverse$ANTIEMETICS and events.adverse$Persistant N&V into one and cahnge name to PONV (above)
# nv.row <- c(grep("nausea", pacu.data$Answer, ignore.case=TRUE),
#              grep("antiemetic", pacu.data$Answer, ignore.case=TRUE),
#              grep("vomiting", pacu.data$Answer, ignore.case=TRUE),
#             grep("PONV", pacu.data$Answer, ignore.case = TRUE)
# ) # end c(...)
# Extract the data from unique row numbers (search word might be entered twivce in a row)
# nv.data <- pacu.data[unique(nv.row),]
# Extract the unique patients among the dataset
# nv.row <- row(nv.data[unique(nv.data$MRN),])
# nv.data <- nv.data[nv.row[,1],]
# nv <- table(nv.data$Month_Yr, nv.data$Month_Yr)
# nv <- rowSums(nv)

# Respiratory event = Anaes intervent + Cardio/ Resp Arrest + Reintub + Resp Complication
events.adverse$"Resp V Serious" <- events.adverse$"CARDIO / RESP ARREST" + events.adverse$"REINTUB/ VENTILATION"
events.descriptors <- c(events.descriptors, "Resp Event", "Resp Serious", "Resp V Serious")
events.adverse$"Resp Event" <- events.adverse$"ANAES RESP INTERVENT" + events.adverse$"CARDIO / RESP ARREST" + events.adverse$"REINTUB/ VENTILATION" + events.adverse$"RESP COMPLICATION"
# Serious Resp event = Anaes intervent + Cario Resp Arrest + Reintub
events.adverse$"Resp Serious" <- events.adverse$"ANAES RESP INTERVENT" + events.adverse$"CARDIO / RESP ARREST" + events.adverse$"REINTUB/ VENTILATION"
# V Serous Resp Event = Arrest + Reintub

# Change CARDIO / RESP ARREST as this label causes an error later when used for file nameing
names(events.adverse)[names(events.adverse)=="CARDIO / RESP ARREST"] <- "CARDIO-RESP ARREST"
events.descriptors <- names(events.adverse[2:length(events.adverse)])
# Update pacu.data$Answer with new event name
pacu.data$Answer[grep("CARDIO / RESP ARREST", pacu.data$Answer, ignore.case=TRUE)] <- as.character("CARDIO-RESP ARREST")
# Change REINTUB/ VENTILATION as this label causes an error later when used for file nameing
names(events.adverse)[names(events.adverse)=="REINTUB/ VENTILATION"] <- "REINTUBATION"
events.descriptors <- names(events.adverse[2:length(events.adverse)])
# Update pacu.data$Answer with new event name
pacu.data$Answer[grep("REINTUB/ VENTILATION", pacu.data$Answer, ignore.case=TRUE)] <- as.character("REINTUBATION")

# Unplanned ICU admission should be calculated from the mot data detecting dispatity between
# planned discharge ward and actual discharge ward

# Remove unnecessary columns for charting etc or use new vector with only columns needed for charting
# Update the event descriptors in the vector
events.infrequent.descriptors <- c(events.descriptors[4], events.descriptors[11], events.descriptors[13],events.descriptors[14]) #Cardiac Arrest, Reintubation, Return to OR, Unplanned ICU
events.to.skip <- c(events.descriptors[7], events.descriptors[9], events.descriptors[10], events.descriptors[19], events.infrequent.descriptors) # Other, Prolonged Unconc, Reaction, Resp V Serious

# Do plots for each event descriptor & write to pdf
events.to.chart <- events.descriptors[! events.descriptors %in% events.to.skip]
events.to.chart <- events.to.chart[2:length(events.to.chart)]
for (i in events.to.chart){
  setwd(output.directory)
  filename.prefix <- paste(min(mot.data$Month_Yr), "-", max(mot.data$Month_Yr)," ")
  filename <- paste(filename.prefix, i," p-chart.pdf")
  pdf(filename, height = 7, width =12 )

    qcc(events.adverse[i],
      type="p",
      sizes=events.adverse$total.pacu.cases,
      nsigmas=3,
      labels=events.adverse$date,
      axes.las=2,
      data.name = i, #c(i, min(mot.data$Month_Yr), "-", max(mot.data$Month_Yr)),
      add.stats=FALSE,
      xlab= "Date",
      ylab = "Proportion",
      title = paste(i, "\n", min(mot.data$Month_Yr), " to ", max(mot.data$Month_Yr))
      ) # Close qcc
    dev.off()
  } # end for i

# Use qcc g-chart for plot of days between events
# use for return to or, cardiac arrest, reintubation, unplanned admiss icu - might need to consider larger data set so longer time span analysed
# Plot g-charts
for (i in events.infrequent.descriptors) {
  if (i %in% (events.infrequent.descriptors[2])) next
 t <- as.data.frame(table(pacu.data$Day_Month_Yr, pacu.data$Answer == i))
 t <- subset(t, t["Var2"]==TRUE)
 events.infrequent <- t["Freq"]
 noevents <- diff(which(c(1,events.infrequent[,1])>=1))-1

setwd(output.directory)
filename.prefix <- paste(min(mot.data$Month_Yr), "-", max(mot.data$Month_Yr)," ")
filename <- paste(filename.prefix, i," g-chart.pdf")
pdf(filename, height = 7, width =12 )
t <- subset(t, t$Freq>=1)
par(mfrow=c(1,1))
qcc(noevents,
    type="g",
    #nsigmas=3,
    conf=0.95,
    labels=t[,"Var1"],
    axes.las=2,
    add.stats=FALSE,
    xlab= "Date",
    ylab = "Days between",
    title = paste(i, "\n", min(mot.data$Month_Yr), " to ", max(mot.data$Month_Yr)),
    restore.par = FALSE #to allow adding lines etc to chart with par command
)# Close qcc
dev.off()
} # Close For i

# code for calculating days between infrequent events
# pacu.data[which(pacu.data$Answer==events.descriptors[2]),"Day_Month_Yr"]

# CALCULATE ACHS INDICATORS and TABLE:
# ACHS indicators are:
# 1.1 Pre-anesthetic consultation by anaesthetist
# 1.2 Smoking cessation advice in pre-anaesthetic consultation
# 2.1 Presence of trained assistant
# 2.2 Documentation complies with ANZCA PS6
# 2.3 Stop-before you block procedure
# 2.4 Prophlactic antiemetics administered to patients wit a history
# 3.1 Relief of respiratory distress in recovery (re-intub/ LMA/ ventilation)
# 3.2 PONV treatment in PACU
# 3.3 Temp < 36C
# 3.4 Pain not responding to protocol
# 3.5 Unplanned stay > 2 hrs
# 4.1 Unplanned admission to ICU
# 4.2 Documented handover MOT-PACU
# 4.3 Documented handover PACU-Ward
# 5.1 Pain scores recorded for surgical patients
# 5.2 Post-op epidurals reviewed by anaesthetist daily
#

ACHS.2.1 <- percent(length(mot.data[,"Tech.1.Name"])/length(mot.data[,"Anaes.1.Name"]))
ACHS.3.1 <- percent(sum(events.adverse$`Resp V Serious`) / sum(monthlycases[,"PACU"]),format="d")
ACHS.3.2 <- percent(sum(events.adverse$PONV) / sum(monthlycases[,"PACU"]),format="d")
ACHS.3.3 <- percent(sum(events.adverse$`HYPOTHERMIA <36 DEG`) / sum(monthlycases[,"PACU"]),format="d")
ACHS.3.4 <- percent(sum(events.adverse$`Pain Revew`) / sum(monthlycases[,"PACU"]),format="d")
ACHS.3.5 <- percent(sum(events.adverse$`PROLONGED STAY >2 HR`) / sum(monthlycases[,"PACU"]),format="d")

ACHS.table <- data.frame(
  Indicator = c("2.1 Presence of trained assistant",
                "3.1 Relief of respiratory distress in recovery",
                "3.2 PONV treatment in PACU",
                "3.3 Temp < 36C",
                "3.4 Pain not responding to protocol",
                "3.5 Unplanned stay > 2 hrs"
    ),
  Numerator = c(length(mot.data[,"Tech.1.Name"]), 
                sum(events.adverse$`Resp V Serious`),
                sum(events.adverse$PONV),
                sum(events.adverse$`HYPOTHERMIA <36 DEG`),
                sum(events.adverse$`Pain Revew`),
                sum(events.adverse$`PROLONGED STAY >2 HR`)),
  Denominator = c(length(mot.data[,"Anaes.1.Name"]),
                  sum(monthlycases[,"PACU"]),
                  sum(monthlycases[,"PACU"]),
                  sum(monthlycases[,"PACU"]),
                  sum(monthlycases[,"PACU"]),
                  sum(monthlycases[,"PACU"])),
  Value = c(ACHS.2.1, ACHS.3.1, ACHS.3.2, ACHS.3.3, ACHS.3.4, ACHS.3.5)
) # Close ACHS.table <- df
# Write table to pdf
setwd(output.directory)
filename <- paste(filename.prefix, " ACHS Indicators.pdf")
#pdf(filename, height = 7, width =12 )
export_formattable(formattable(ACHS.table,align=c("l","r","r","r")), filename)
# dev.off()

# FUNNEL PLOTS
# Combine mot.data & pacu.data into one data set linked by MRN & date
combined.data <- join(mot.data, pacu.data, by=c("MRN","date"), type='right', match='all')
# Clean up combined.data - remove cases with no anaesthetist, registrars and other extraneous names
registrars <- c("", "ALVAREZ,Juan Sebastian Lopera","BACAJEWSK,Rafal","HUNG,David -  Registrar Anaesthetist","JONES,Alison","POLLARD,Amy","SMITH,Robert","COLLARD,Cameron - Anaes Regs", "LOPERA ALVAREZ,Juan Sebastian", "MEHMOOD,Junaid", "MCDERMOTT,Laura", "JONES,Tyson", "BELL,Cameron - Anaes Regs", "BURNELL,Sheena", "SHAW,Rebecca", "BEUTH,Jodie", "BURGESS,Tegan", "GUY,Louis", "LIM,Kian - Anaes Registrar", "PEARSON,Yana", "RANCE,Timothy","SOUNESS,Andrew", "WILLIAMS,Charles", "DAWAR,Ahmad", "CHAWLA,Gunjan", "HAENKE,Daniel","HUANG,Jason - Gastroenterologist", "RICKARDS,Leah", "TOGNOLINI,Angela", "WILLIAMS,Courtney", "EDWARDS,Lucas", "FERNANDEZ,Nevin", "HOLLAND,Tom", "KIPPIN,Louisa", "TURNER,Maryann", "JAMSHIDI,Behruz", "HUANG,Jason", "DASILVA,Dianna", "FARZADI,Maryam Zarhra", "FLINT,Nathan", "HERDY,Charles", "HOLGATE,Andrew")
combined.data <- combined.data[!combined.data$Anaes.1.Name %in% c(registrars, "MURRAY,John", "POSTLE,David", "BRUNELLO,Kathryn"),]

# remove cases destined to ICU - analyse PACU only
combined.data <- combined.data[combined.data$PACU.ICU.WARD == "PACU",]

events.to.chart <- c(names(events.adverse)[7],names(events.adverse)[16], names(events.adverse)[17])

# manually select adverse events from the list below for now - change to loop once established.
for (event in events.to.chart) {
# determine the anaesthetists names with patients experiencing events
if (event == names(events.adverse)[7]) {cases <- na.omit(combined.data[combined.data$Answer %in% c("ANTIEMETICS","PONV"),]["Anaes.1.Name"])}
if (event == names(events.adverse)[16]) {cases <- na.omit(combined.data[combined.data$Answer %in% c("PAIN R/V ANAES CONS", "ANAESTH R/V- PAIN"),]["Anaes.1.Name"])}
if (event == names(events.adverse)[17]) {cases <- na.omit(combined.data[combined.data$Answer == "HYPOTHERMIA <36 DEG",]["Anaes.1.Name"])}

# Calculate the case load per anaesthetist - taken from MOT data because row number increased by join to form combined.data
anaes.cases <- NA
p <- NA 
anaes.names <- colnames(table(mot.data$Month_Yr, droplevels(mot.data$Anaes.1.Name, c(registrars, "MURRAY,John", "POSTLE,David", "BRUNELLO,Kathryn"))))
for (i in 1 : length(anaes.names)){
  anaes.cases[i] <- sum(table(mot.data$Month_Yr, droplevels(mot.data$Anaes.1.Name, c(registrars, "MURRAY,John", "POSTLE,David", "BRUNELLO,Kathryn")))[,i])
  p[i] <- sum(cases$Anaes.1.Name == anaes.names[i]) / anaes.cases[i]
  }

## code to plot funnel plot from https://stats.stackexchange.com/questions/5195/how-to-draw-funnel-plot-using-ggplot2-in-r/5210#5210
library(ggplot2)

number <- anaes.cases
p.se <- sqrt((p*(1-p)) / (number))
df <- data.frame(p, number, p.se)

## common effect (fixed effect model)
## p.fem <- weighted.mean(p, 1/p.se^2)
## arithmetic mean - I don't understand the use if FEM and weighted mean - perhaps I should look this up? I have repaced with arithmetic mean
## p.fem <- sum(p*number)/sum(number)
p.fem <- mean(p)

## lower and upper limits for 95% and 99.9% CI, based on FEM estimator
number.seq <- seq(0.001, max(number), 0.1)
number.ll95 <- p.fem - 1.96 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
number.ul95 <- p.fem + 1.96 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
number.ll999 <- p.fem - 3.29 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
number.ul999 <- p.fem + 3.29 * sqrt((p.fem*(1-p.fem)) / (number.seq)) 
dfCI <- data.frame(number.ll95, number.ul95, number.ll999, number.ul999, number.seq, p.fem)


for (i in 1:length(anaes.names)){
## draw plot
fp <- ggplot(aes(x = number, y = p), data = df) +
  geom_point(shape = 1) +
  geom_point(aes(x = number[i], y = p[i]), shape = 16) + 
  geom_line(aes(x = number.seq, y = number.ll95), data = dfCI) +
  geom_line(aes(x = number.seq, y = number.ul95), data = dfCI) +
  geom_line(aes(x = number.seq, y = number.ll999), linetype = "dashed", data = dfCI) +
  geom_line(aes(x = number.seq, y = number.ul999), linetype = "dashed", data = dfCI) +
  geom_hline(aes(yintercept = p.fem), data = dfCI) +
  scale_y_continuous(limits = c(0,0.15)) +
  ggtitle(paste("Funnel Plot of: ", event, "for", anaes.names[i]," ",min(mot.data$Month_Yr)," - ", max(mot.data$Month_Yr))) +
  xlab("Number of cases") + ylab(paste("Proportion")) + theme_bw() 

setwd(output.directory)
filename <- paste(filename.prefix, anaes.names[i], event, "Funnel Plot.pdf")
ggsave(filename, plot = fp, device="pdf", path= output.directory, width = 12, height = 7)
} # For i loop
} # For events loop
# CLEAN UP before ending.
# Reset working directory to functions.directory so history etc saved there on exit.
setwd(functions.directory)
# Clear Workspace
# rm(list=ls())