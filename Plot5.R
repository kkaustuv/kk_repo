NEI<- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Find Vehicle source IDs from SCC data Frame
vehicle<-grepl("vehicle",SCC$SCC.Level.Two,ignore.case = TRUE)
vehicle_scc<-SCC[vehicle,]$SCC

#Get all NEI data for Vehicle sources
vehicle_nei<-NEI[NEI$SCC %in% vehicle_scc,]

#Get Baltimore Vehicle Data
Baltimore_vehicle_nei<-subset(vehicle_nei,vehicle_nei$fips=="24510")

#Plot Graph
library("ggplot2")

png("Plot5.png", width=480, height=480)

gp<-ggplot(Baltimore_vehicle_nei,aes(factor(year),Emissions))+geom_bar(stat="identity")+labs(x="Year",y="Emission",title="Baltimore Vehicle Emission ")
print(gp)
dev.off()