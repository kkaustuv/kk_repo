NEI<- readRDS("summarySCC_PM25.rds")

#Select all data for Baltimore City
Baltimore<-subset(NEI,NEI$fips=="24510")


#Plot Graph
library("ggplot2")

png("Plot3.png", width=480, height=480)
gp<-ggplot(Baltimore,aes(factor(year),Emissions,fill=type))+geom_bar(stat="identity")+facet_wrap(~type)+labs(x="Year",y="Emission (In Tons)",title="Baltimore City Emission across all types")

print(gp)
dev.off()
