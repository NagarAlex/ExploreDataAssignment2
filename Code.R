#Reading datsets
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
#Question 1
yeartotal <- with(NEI, tapply(Emissions, year, sum))
plot(yeartotal, xaxt = "n", xlab = "Year", ylab = "PM2.5Emissions (Tons)", pch = 20, main = "Plot 1", type = "l")
axis(1, at =1:4,  labels = names(yeartotal) )

#Question 2
#Baltmore only
Balt <- subset(NEI, NEI$fips == "24510")
#Sum Emissions by Year (yt = year total)
Baltyt <-  with(Balt, tapply(Emissions, year, sum))
#plot
plot(Baltyt, xaxt = "n", ylab = "PM2.5Emissions at Baltimore (Tons)", xlab = "Year", pch = 20, main = "Plot 2")
axis(1, at =1:4 , labels = names(Baltyt) )

#Question 3
#ggplot
library(ggplot2)
#Split by type (BS = Baltimore Split)
BS <- split(Balt, Balt$type)
#Write function in order to use lapply (tyearem = total yearly emissions)
tyearem <- function(x){tapply(x$Emissions, x$year, sum)}
BStyear <- lapply(BS, tyearem)

#convert to data frame 
y <- lapply(BStyear, as.data.frame)
#merge individual type data frames
y12 <- merge(y[1], y[2], by = "row.names")
y34 <- merge(y[3], y[4], by = "row.names")
#eliminate surplus year column
y1234 <- merge(y12, y34[,-1], by = "row.names")
#eliminate surplus (list number) column
y1234 <- y1234[,-1]
#Fix names
colnames(y1234) <- c("Year", names(BStyear))
colnames(y1234) <- gsub("-", ".", colnames(y1234))
y1234$Year <- as.numeric(y1234$Year)
#plot using ggplot2
plotbase <- ggplot(data = y1234, aes(x = Year))
NRplot <- plotbase + geom_line(aes(y = NON.ROAD))
NPplot <- plotbase + geom_line(aes(y = NONPOINT))
ORplot <- plotbase + geom_line(aes(y = ON.ROAD))
Pplot <- plotbase + geom_line(aes(y= POINT))
#Place on 2x2 grid

library("gridExtra")
grid.arrange(NRplot, NPplot, ORplot, Pplot, nrow = 2, ncol = 2)

#Question 4
#Identify Coal 
CoalSCC <- SCC[grep("Coal", SCC$EI.Sector),]
#Coal Subset
Coal <- subset(NEI, NEI$SCC %in% unique(CoalSCC$SCC))
#Sum Emissions by year
Cemyear <- tapply(Coal$Emissions, Coal$year, sum)
#Plot
par(mfrow = c(1,1))
Coalplot <- plot(Cemyear, xlab = "Year", ylab = "Coal Emissions (Tons)", xaxt = "n", pch = 20, main = "Plot 4")
axis(1, at = 1:4, labels = names(Cemyear))

#Question 5
#Identify Vehicle Sources
VehicleSCC <- SCC[grep("Vehicle", SCC$EI.Sector),]
#Vehicle Subset
Vehicle <-  subset(NEI, NEI$SCC %in% unique(VehicleSCC$SCC))
#Baltimore Subset
BaltVehicle <- subset(Vehicle, Vehicle$fips == "24510")
#Sum Emissions by Year
BVemyear <- tapply(BaltVehicle$Emissions, BaltVehicle$year, sum)
#Plot
BVplot <- plot(BVemyear, xlab = "Year", ylab = "Vehicle Emissions (Tons)", xaxt = "n", pch = 20, main = "Plot 5")
axis(1, at = 1:4, labels = names(BVemyear))


#Question 6
#Vehicle Subset
VehicleSCC <- SCC[grep("Vehicle", SCC$EI.Sector),]
Vehicle <-  subset(NEI, NEI$SCC %in% unique(VehicleSCC$SCC))
#Baltimore
BaltVehicle <- subset(Vehicle, Vehicle$fips == "24510")
BVemyear <- tapply(BaltVehicle$Emissions, BaltVehicle$year, sum)
#LA
LAVehicle <- subset(Vehicle, Vehicle$fips == "06037")
LAVemyear <- tapply(LAVehicle$Emissions, LAVehicle$year, sum)
#Plot
par(mfrow= c(1,2))
BVplot <- plot(BVemyear, xlab = "Year", ylab = "Vehicle Emissions (Tons)", xaxt = "n", pch = 20, main = "Baltimore")
axis(1, at = 1:4, labels = names(BVemyear))
LAVplot <- plot(LAVemyear, xlab = "Year", ylab = "Vehicle Emissions (Tons)", xaxt = "n", pch = 20, main ="Los Angeles" )
axis(1, at = 1:4, labels = names(LAVemyear))
