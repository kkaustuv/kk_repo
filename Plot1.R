NEI<- readRDS("summarySCC_PM25.rds")

#Aggregate of Emission by Year 
year_w_emission<-tapply(NEI$Emissions/10^6,NEI$year,sum)

#Plot Graph
png("plot1.png", width=480, height=480)
barplot(year_w_emission,xlab="Year",ylab = "PM2.5 Emission  ( In 10^6 tons)",main= "USA Total Emission")
dev.off()