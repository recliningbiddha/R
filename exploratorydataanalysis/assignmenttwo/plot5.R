## Read datafile with readRDS function as given in assignment instructions
if (!exists("NEI")) {NEI <- readRDS("summarySCC_PM25.rds")}
if (!exists("SCC")) {SCC <- readRDS("Source_Classification_Code.rds")}

library(ggplot2)

NEI_vehicle <- NEI[NEI$fips == "24510" & NEI$type=="ON-ROAD",]

aggregate.by.year <- aggregate(Emissions ~ year, NEI_vehicle, sum)

png("plot5.png", width=640, height=480)
g <- ggplot(aggregate.by.year, aes(factor(year), Emissions))
g <- g + geom_bar(stat="identity")
g <- g + ylab("Total emissions per year") + xlab("Year") + ggtitle('Total Emissions from road vehicles in Baltimore City from 1999 to 2008')
print(g)
dev.off()