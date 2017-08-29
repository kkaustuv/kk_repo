NEI<- readRDS("summarySCC_PM25.rds")

#Select all data for Baltimore City
Baltimore<-subset(NEI,NEI$fips=="24510")

#Aggregate of Emission by Year for Baltimore City
Baltimore_y_w_emiss <- tapply(Baltimore$Emissions,Baltimore$year,sum)

#Plot Graph
png("Plot2.png", width=480, height=480)
barplot(Baltimore_y_w_emiss,xlab="Year",ylab = "PM2.5 Emission  ( In tons)",main= "Baltimore City Total Emission")
dev.off()