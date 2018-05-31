setwd("~/Desktop/Analysis/TemperatureAnalysis")

library(ncdf4)
library(raster) #manipulation
library(rgdal) #geospatial analysis
library(ncdf4.helpers)
library(abind)
library(ggplot2)
library(dplyr)
library(gridExtra)
#This is the data from the daily time-series (NOAA 1/4° x 1/4° resolution)
NJNCtemps1 <- nc_open("1989_2001.nc")
NJNCtemps2 <- nc_open("2002_2012.nc")

sst.array1 <- ncvar_get(NJNCtemps1,"sst")
sst.array2 <- ncvar_get(NJNCtemps2,"sst")
sst.arrays <- abind(sst.array1,sst.array2)

#use this command to swap rows and columns 
sst.arrays <- aperm(sst.arrays, c(2,1,3))
lat <- ncvar_get(NJNCtemps1,"lat")

lon <- ncvar_get(NJNCtemps1,"lon")
lon <- lon-360

#Name rows and columns of 
colnames(sst.arrays) <- c(lon)
rownames(sst.arrays) <- c(lat)




#load in shelf data as a raster
shelf <- raster("shelfData.nc", crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

#stack entire temperature array as a raster brick
temperatureBrick <- brick(sst.arrays, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
                          crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

#flip the brick to make highest latitude at the top
temperatureBrick <- flip(temperatureBrick,direction = 'y')


#make an extent (same as temp data) and crop shelf data to it
temp.object <- extent(-81.625,-71.875,30.875,40.625)
shelf <- crop(shelf,temp.object)

#resample shelf data to temperature data (so they have matching resolutions)
interpShelf<- resample(shelf,temperatureBrick,method = 'bilinear')

#convert all shelf data that is greater than 0m or less than 200m to NA
interpShelf[interpShelf < (-200)]<-NA
interpShelf[interpShelf >= 0]<-NA

#mask the temperature data by these bounds on the shelf data (removes all temperatures not on shelf)
temperatureBrick <- mask(temperatureBrick,interpShelf)


#Subsetting the data
temperature.df <- as.data.frame(temperatureBrick,xy=TRUE)
njTemp.df <- temperature.df[1:841,]
ncTemp.df <- temperature.df[841:1600,]


#create an array of dates from '89 to 2012
dates <- seq(as.Date("1989-01-01"), as.Date("2012-12-31"), by=1)
colnames(njtempObject) <- c("x","y",as.character(dates))

dates.df <- as.data.frame(do.call(rbind,strsplit(as.character(dates),'-',fixed = TRUE))) #splits up dates by '-' markings
njTemp.df <- matrix(colMeans(njTemp.df[3:8768],na.rm = TRUE),nrow=1,ncol=8766) #creates a row array of average daily temps for north location
njTemp.df <- t(njTemp.df) #transpose into column array
njTemp.df<- cbind(dates.df,njTemp.df) #add the dates as 3 more columns (year, month, day)
colnames(njTemp.df)<-c("year","month","day","north")#rename the columns 

#complete the above operations on the south temps as well
ncTemp.df <- matrix(colMeans(ncTemp.df[3:8768],na.rm = TRUE),nrow=1,ncol=8766)
ncTemp.df <- t(ncTemp.df)
#combine north df with south df
allTemps.df <- cbind(njTemp.df,ncTemp.df)
colnames(allTemps.df)<-c("year","month","day","north","south")#just so southern temps say "south"

#save all daily temps
dailyTemps <- allTemps.df[,4:5]
dailyTemps <- transform(dailyTemps,Date = dates)
save(dailyTemps,file = "~/Desktop/Analysis/with 50 extras/dailyTemps.RData")
dailyTemps <- transform(dailyTemps,
                        year = dates.df[,1],
                        month = dates.df[,2],
                        day = dates.df[,3])

#create data frames of monthly and annual means for north and south
northYears.df <- dailyTemps %>%
             group_by(year) %>%
             summarise(north.SST = mean(north))
northMonths.df <- dailyTemps %>%
                  group_by(year,month) %>%
                  summarise(north.SST = mean(north))

southYears.df <- dailyTemps %>%
                  group_by(year) %>%
                  summarise(south.SST = mean(south))
southMonths.df <- dailyTemps %>%
                  group_by(year,month) %>%
                  summarise(south.SST = mean(south))

#combine the north and south temperatures into a single data frame (only did annual here)
nsYears.df <- cbind(as.data.frame(northYears.df),as.data.frame(southYears.df[,2]))
save(nsYears.df,file=("~/Desktop/Analysis/with 50 extras/nsYearsdf.RData"))







#START HERE
###########
load("~/Desktop/Analysis/with 50 extras/dailyTemps.RData")
load("~/Desktop/Analysis/with 50 extras/nsYearsdf.RData")

#plot north and south shelf temperatures 
years = c(1989:2012) #so x-scale works in ggplot
annualPlot <- ggplot(nsYears.df)+geom_line(aes(years,north.SST,colour = "NORTH"))+geom_line(aes(years,south.SST,colour = "SOUTH"))+
  geom_point(aes(years,south.SST,colour = "SOUTH"))+
  geom_point(aes(years,north.SST,colour = "NORTH"))+
  labs(title="Annual Shelf Temperatures",x="Years",y="Temperature (°C)")+
  scale_colour_manual("Location",values = c("steelblue3","tomato"))+
  scale_x_continuous(breaks = seq(min(years), max(years), by = 2))+
  scale_y_continuous(limits = c(4,26),breaks = seq(min(4),max(26),by=2))+
  theme_bw() +
  theme(axis.text.x = element_text(angle=45),
        axis.text.y = element_text(angle=45))

#jpeg(file="~/Desktop/Analysis/TemperatureAnalysis/Plots/AnnualShelfTemperatures.jpeg", width = 10, height = 7.5, res = 300, units = "in")
#annualPlot
#dev.off()


#averaging the data by month (use aggregate to sort my month and year)

monthlyAverages <- as.data.frame(aggregate(dailyTemps[,1:2], list(dailyTemps$month,dailyTemps$year),mean,na.rm=TRUE))
colnames(monthlyAverages)<- c("Month","Year","North","South")
#monthCount <- c(1:288)
monthlyAverages <- transform(monthlyAverages,monthCount = c(1:288))
monthlyAverages <- transform(monthlyAverages,Season = )

monthlyTempPlot <- ggplot(monthlyAverages, aes(monthCount,North)) + geom_line() + geom_line(aes(color = Year))
+ ggtitle("Monthly Northern Temperatures") + xlab("Month Count") + ylab("Temperature (°C)")

#jpeg(file="~/Desktop/Analysis/TemperatureAnalysis/Plots/MonthlyTempPlot.jpeg", width = 10, height = 7.5, res = 300, units = "in")
#monthlyTempPlot
#dev.off()


#subset seasonally
winter <- monthlyAverages[which(monthlyAverages$Month=='01' | monthlyAverages$Month=='02' | monthlyAverages$Month=='03' | monthlyAverages$Month=='04' ),]
fall <- monthlyAverages[which(monthlyAverages$Month=='09' | monthlyAverages$Month=='10' | monthlyAverages$Month=='11' | monthlyAverages$Month=='12' ),]


#split seasonal into north and south and average over the subsetted periods
#also add in a location and season column for each data frame
annualWinterN <- as.data.frame(
                winter %>%
                group_by(Year) %>%
                summarise(SST = mean(North))
                )
annualWinterN <- transform(annualWinterN,Location = "North",Season ="W" )

annualFallN <- as.data.frame(
              fall %>%
              group_by(Year) %>%
              summarise(SST = mean(North))
              )
annualFallN <- transform(annualFallN,Location = "North",Season = "F")

annualWinterS <- as.data.frame(
                 winter %>%
                 group_by(Year) %>%
                 summarise(SST = mean(South))
                 )
annualWinterS <- transform(annualWinterS,Location = "South",Season="W")

annualFallS <- as.data.frame(
               fall %>%
               group_by(Year) %>%
               summarise(SST = mean(South))
               )
annualFallS <- transform(annualFallS,Location="South",Season="F")


#use rbind to create a data frame for each season
#order data frames by ascending year
fallTemps<-rbind(annualFallN,annualFallS)
fallTemps<-fallTemps[order(fallTemps$Year),]
winterTemps<-rbind(annualWinterN,annualWinterS)
winterTemps<-winterTemps[order(winterTemps$Year),]

#merge the two seasonal data frames into one master data frame
#order by year
seasonalAverages <- rbind(annualWinterN,annualFallN,annualWinterS,annualFallS)
seasonalAverages<- seasonalAverages[order(seasonalAverages$Year),]

#Create a faceted plot to show seasonal differences between north and south
seasonalPlot <- ggplot(seasonalAverages)+geom_line(data = seasonalAverages[seasonalAverages$Season=='F',], aes(Year,SST,colour='Fall'),group=1)+
  geom_line(data = seasonalAverages[seasonalAverages$Season=='W',],aes(Year,SST,colour='Winter'),group=1)+
  labs(title="Seasonal Shelf SST by Location",x="Years",y="Temperature (°C)")+
  scale_colour_manual("Season",values = c("tomato","steelblue3"))+
  scale_x_discrete(breaks = seq(min(years), max(years), by = 2))+
  scale_y_continuous(limits = c(4,26),breaks = seq(min(4),max(26),by=2))+
  facet_grid(Location ~ .)+
  theme_bw() +
  theme(axis.text.x = element_text(angle=45),
        axis.text.y = element_text(angle=45))+
  theme(strip.background =element_rect(fill="gray80"))


#save file as jpeg
#jpeg(file="~/Desktop/Analysis/TemperatureAnalysis/Plots/SeasonalShelfTemperatures.jpeg", width = 10, height = 7.5, res = 300, units = "in")
#seasonalPlot
#dev.off()

  
  




