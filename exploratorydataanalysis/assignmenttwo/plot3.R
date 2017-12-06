## Read datafile with readRDS function as given in assignment instructions
if (!exists("NEI")) {NEI <- readRDS("summarySCC_PM25.rds")}
if (!exists("SCC")) {SCC <- readRDS("Source_Classification_Code.rds")}

library(ggplot2)

Baltimore <- NEI[NEI$fips=="24510",]
aggregate.by.year <- aggregate(Emissions ~ year + type, Baltimore, sum)

png("plot3.png", width=640, height=480)
g <- ggplot(aggregate.by.year, aes(year, Emissions, color=type))
g <- g + geom_line()
g <- g + ylab("Total emissions per year") + xlab("Year") + ggtitle('Total Emissions in Baltimore City (fips == "24510") by type from 1999 to 2008')
print(g)
dev.off()