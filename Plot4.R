NEI<- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Find Coal Combusion source IDs from SCC data Frame
comb<-grepl("comb",SCC$SCC.Level.One,ignore.case = TRUE)
coal<-grepl("coal",SCC$SCC.Level.Four,ignore.case = TRUE)
coal_comb<-comb&coal
coal_comb_scc<-SCC[coal_comb,]$SCC
#Get all NEI data for Coal Combusion sources
coal_comb_nei<-NEI[NEI$SCC %in% coal_comb_scc,]

#Aggregate of Emission by Year for Baltimore City
Baltimore_y_w_emiss <- tapply(Baltimore$Emissions,Baltimore$year,sum)

#Plot Graph
png("Plot4.png", width=480, height=480)
barplot(Baltimore_y_w_emiss,xlab="Year",ylab = "PM2.5 Emission  ( In tons)",main= "Baltimore City Total Emission")
dev.off()