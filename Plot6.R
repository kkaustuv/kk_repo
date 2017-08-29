NEI<- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Find Vehicle source IDs from SCC data Frame
vehicle<-grepl("vehicle",SCC$SCC.Level.Two,ignore.case = TRUE)
vehicle_scc<-SCC[vehicle,]$SCC

#Get all NEI data for Vehicle sources
vehicle_nei<-NEI[NEI$SCC %in% vehicle_scc,]

#Get Baltimore City Vehicle Data
Baltimore_vehicle_nei<-subset(vehicle_nei,vehicle_nei$fips=="24510")
Baltimore_vehicle_nei$city<- "Baltimore"
#Get  Los Angeles County Vehicle Data
Los_angeles_vehicle_nei<-subset(vehicle_nei,vehicle_nei$fips=="06037")
Los_angeles_vehicle_nei$city<-"Los Angeles"

#Combine Baltimore and Los Angeles Data
bal_los_nei <- rbind(Baltimore_vehicle_nei,Los_angeles_vehicle_nei)

#Plot Graph
library("ggplot2")

png("Plot6.png", width=480, height=480)

gp<-ggplot(bal_los_nei,aes(factor(year),Emissions/10^3,fill=city))+geom_bar(stat="identity")+facet_wrap(~city)+labs(x="Year",y="Emission (In 10^3 Tons)",title="Baltimore and Los Angeles Vehicle Emission")
print(gp)
dev.off()