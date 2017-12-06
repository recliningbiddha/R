## Read datafile with readRDS function as given in assignment instructions
if (!exists("NEI")) {NEI <- readRDS("summarySCC_PM25.rds")}
if (!exists("SCC")) {SCC <- readRDS("Source_Classification_Code.rds")}

Baltimore <- NEI[NEI$fips=="24510",]
aggregate.by.year <- aggregate(Emissions ~ year, Baltimore, sum)

# png("plot2.png")
barplot(height = aggregate.by.year$Emissions, names.arg = aggregate.by.year$year, xlab="Year", ylab= "Total pm 2.5 emissions", main = "Plot 1")
dev.off()