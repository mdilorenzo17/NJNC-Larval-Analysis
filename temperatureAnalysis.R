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

annualPlot <- ggplot(nsYears.df)+
  #geom_line(aes(years,north.SST,colour = "NORTH"))+
  #geom_line(aes(years,south.SST,colour = "SOUTH"))+
  geom_point(aes(years,south.SST,colour = "SOUTH"))+
  geom_point(aes(years,north.SST,colour = "NORTH"))+
  geom_smooth(aes(x = years,y = north.SST,method = 'auto'))+
  geom_smooth(aes(x = years,y = south.SST,colour = "SOUTH",method = 'auto'))+
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
annualWinterN <- transform(annualWinterN,Location = "North",Season ="Winter" )

annualFallN <- as.data.frame(
              fall %>%
              group_by(Year) %>%
              summarise(SST = mean(North))
              )
annualFallN <- transform(annualFallN,Location = "North",Season = "Fall")

annualWinterS <- as.data.frame(
                 winter %>%
                 group_by(Year) %>%
                 summarise(SST = mean(South))
                 )
annualWinterS <- transform(annualWinterS,Location = "South",Season="Winter")

annualFallS <- as.data.frame(
               fall %>%
               group_by(Year) %>%
               summarise(SST = mean(South))
               )
annualFallS <- transform(annualFallS,Location="South",Season="Fall")


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

#Plot all data (smooth) -> lm for only winter south temps(jan-april) and all north temps
jpeg(file="~/Desktop/Analysis/AFS/lmTemperatureTrends.jpeg", width = 10, height = 7.5, res = 300, units = "in")
ggplot()+
#  geom_smooth(data = dailyTemps[as.numeric(dailyTemps$month)==01 | as.numeric(dailyTemps$month)==02 | as.numeric(dailyTemps$month)==03 | as.numeric(dailyTemps$month)==04,],aes(x=Date,y=north,color="North",linetype = "Winter"),method = 'lm')+
#  geom_smooth(data = dailyTemps[as.numeric(dailyTemps$month)==09 | as.numeric(dailyTemps$month)==10 | as.numeric(dailyTemps$month)==11 | as.numeric(dailyTemps$month)==12,],aes(x=Date,y=north,color="North",linetype = "Fall"),method = 'lm')+
#  geom_smooth(data = dailyTemps[as.numeric(dailyTemps$month)==01 | as.numeric(dailyTemps$month)==02 | as.numeric(dailyTemps$month)==03 | as.numeric(dailyTemps$month)==04,],aes(x=Date,y=south,color="South",linetype = "Winter"),method = 'lm')+
  geom_smooth(data = dailyTemps[which(dailyTemps$Season == "Winter"),],aes(x=Date,y=south,color="South",linetype="Winter"),method = 'lm')+
  geom_smooth(data = dailyTemps[which(dailyTemps$Season == "Winter"),],aes(x=Date,y=north,color="North",linetype="Winter"),method = 'lm')+
  geom_smooth(data = dailyTemps[which(dailyTemps$Season == "Fall"),],aes(x=Date,y=north,color="North",linetype="Fall"),method = 'lm') +
  scale_linetype_manual("Season", values = c("Winter" = 1, "Fall" = 6))+
  scale_y_continuous(limits = c(0,31))+
  #scale_linetype_discrete("Season", labels = c("winter","fall"))+
  scale_color_manual("Location", values = c("blue","red"))+
  ylab("Temperature (°C)")+
  theme_bw()+
  guides(lty = guide_legend(override.aes = list(col = 'black')))+
  theme(axis.title.x = element_text(size = 15),
        axis.text.x = element_text(size = 13),
        axis.title.y = element_text(size = 15),
        axis.text.y = element_text(size=13),
        legend.text = element_text(size = 13),
        legend.title = element_text(size=15))
dev.off()


  
#===========================================================
#Determine Hot vs. Cold Years
#===========================================================

#Calculate means
nFallMean <- mean(seasonalAverages[which(seasonalAverages$Season=='Fall' & seasonalAverages$Location=='North'),]$SST)
nWinterMean <- mean(seasonalAverages[which(seasonalAverages$Season=='Winter' & seasonalAverages$Location=='North'),]$SST)
sFallMean <- mean(seasonalAverages[which(seasonalAverages$Season=='Fall' & seasonalAverages$Location=='South'),]$SST)
sWinterMean <- mean(seasonalAverages[which(seasonalAverages$Season=='Winter' & seasonalAverages$Location=='South'),]$SST)

nMean <- mean(seasonalAverages[which(seasonalAverages$Location=='North'),]$SST)
sMean <- mean(seasonalAverages[which(seasonalAverages$Location=='South'),]$SST)


hLineMeans <- data.frame(Location = c("North","South"),
                         FallMeans = c(nFallMean,sFallMean),
                         WinterMeans = c(nWinterMean,sWinterMean))

hLineAnnual <- data.frame(Location = c("North","South"),
                          Mean = c(nMean,sMean))



#++++++++++++++++++++
#FUNCTIONS
#++++++++++++++++++++


#This function categorizes hot or cold and calculates anomaly
for(i in 1:nrow(seasonalAverages)){
  if (seasonalAverages$Season[i] == 'Fall' & 
      seasonalAverages$Location[i] == 'North' & 
      seasonalAverages$SST[i] < mean(seasonalAverages[which(seasonalAverages$Season=='Fall' & seasonalAverages$Location=='North'),]$SST))
  {seasonalAverages$HorC[i] = "Cold"
   seasonalAverages$anomaly[i] = seasonalAverages$SST[i] - mean(seasonalAverages[which(seasonalAverages$Season=='Fall' & seasonalAverages$Location=='North'),]$SST) } 
  
  else if (seasonalAverages$Season[i] == 'Fall' & 
           seasonalAverages$Location[i] == 'North' & 
           seasonalAverages$SST[i] > mean(seasonalAverages[which(seasonalAverages$Season=='Fall' & seasonalAverages$Location=='North'),]$SST)) 
  {seasonalAverages$HorC[i] = "Hot"
   seasonalAverages$anomaly[i] = seasonalAverages$SST[i] - mean(seasonalAverages[which(seasonalAverages$Season=='Fall' & seasonalAverages$Location=='North'),]$SST)}
  
  else if (seasonalAverages$Season[i] == 'Winter' & 
           seasonalAverages$Location[i] == 'North' & 
           seasonalAverages$SST[i] < mean(seasonalAverages[which(seasonalAverages$Season=='Winter' & seasonalAverages$Location=='North'),]$SST)) 
  {seasonalAverages$HorC[i] = "Cold"
   seasonalAverages$anomaly[i] = seasonalAverages$SST[i] - mean(seasonalAverages[which(seasonalAverages$Season=='Winter' & seasonalAverages$Location=='North'),]$SST)}
  
  else if (seasonalAverages$Season[i] == 'Winter' & 
           seasonalAverages$Location[i] == 'North' & 
           seasonalAverages$SST[i] > mean(seasonalAverages[which(seasonalAverages$Season=='Winter' & seasonalAverages$Location=='North'),]$SST)) 
  {seasonalAverages$HorC[i] = "Hot"
   seasonalAverages$anomaly[i] = seasonalAverages$SST[i] - mean(seasonalAverages[which(seasonalAverages$Season=='Winter' & seasonalAverages$Location=='North'),]$SST)}
  
  else if (seasonalAverages$Season[i] == 'Fall' & 
           seasonalAverages$Location[i] == 'South' & 
           seasonalAverages$SST[i] < mean(seasonalAverages[which(seasonalAverages$Season=='Fall' & seasonalAverages$Location=='South'),]$SST)) 
  {seasonalAverages$HorC[i] = "Cold"
  seasonalAverages$anomaly[i] = seasonalAverages$SST[i] - mean(seasonalAverages[which(seasonalAverages$Season=='Fall' & seasonalAverages$Location=='South'),]$SST)}
  
  else if (seasonalAverages$Season[i] == 'Fall' & 
           seasonalAverages$Location[i] == 'South' & 
           seasonalAverages$SST[i] > mean(seasonalAverages[which(seasonalAverages$Season=='Fall' & seasonalAverages$Location=='South'),]$SST)) 
  {seasonalAverages$HorC[i] = "Hot"
  seasonalAverages$anomaly[i] = seasonalAverages$SST[i] - mean(seasonalAverages[which(seasonalAverages$Season=='Fall' & seasonalAverages$Location=='South'),]$SST)}
  
  else if (seasonalAverages$Season[i] == 'Winter' & 
           seasonalAverages$Location[i] == 'South' & 
           seasonalAverages$SST[i] < mean(seasonalAverages[which(seasonalAverages$Season=='Winter' & seasonalAverages$Location=='South'),]$SST)) 
  {seasonalAverages$HorC[i] = "Cold"
  seasonalAverages$anomaly[i] = seasonalAverages$SST[i] - mean(seasonalAverages[which(seasonalAverages$Season=='Winter' & seasonalAverages$Location=='South'),]$SST)}
  
  else if (seasonalAverages$Season[i] == 'Winter' & 
           seasonalAverages$Location[i] == 'South' & 
           seasonalAverages$SST[i] > mean(seasonalAverages[which(seasonalAverages$Season=='Winter' & seasonalAverages$Location=='South'),]$SST)) 
  {seasonalAverages$HorC[i] = "Hot"
  seasonalAverages$anomaly[i] = seasonalAverages$SST[i] - mean(seasonalAverages[which(seasonalAverages$Season=='Winter' & seasonalAverages$Location=='South'),]$SST)}
  
}


#This function just groups the categorical anomalies for plotting
for(i in 1:nrow(seasonalAverages)){
  if(seasonalAverages$HorC[i] == 'Cold' & seasonalAverages$Season[i] == 'Fall')
  {seasonalAverages$group[i] = "Cold; Fall"}
  
  else if(seasonalAverages$HorC[i] == 'Cold' & seasonalAverages$Season[i] == 'Winter')
  {seasonalAverages$group[i] = "Cold; Winter"} 
  
  else if(seasonalAverages$HorC[i] == 'Hot' & seasonalAverages$Season[i] == 'Fall')
  {seasonalAverages$group[i] = "Hot; Fall"}
  
  else if(seasonalAverages$HorC[i] == 'Hot' & seasonalAverages$Season[i] == 'Winter')
  {seasonalAverages$group[i] = "Hot; Winter"}
}


#This function calculates anamoly of each individual (based on 30dayAVG vs Regional mean across all years)
for(i in 1:nrow(updatedallSL)){
  
  if(updatedallSL$Location[i]=='North')
  {updatedallSL$anomaly[i] = updatedallSL$northAVG[i] - mean(seasonalAverages[which(seasonalAverages$Location=='North'),]$SST)}
  
  else if(updatedallSL$Location[i]=='South')
  {updatedallSL$anomaly[i] = updatedallSL$southAVG[i] - mean(seasonalAverages[which(seasonalAverages$Location=='South'),]$SST)}
}

#This function just uses anomaly to determine if a fish is hot or cold (haven't added a buffer region yet)
for(i in 1:nrow(updatedallSL)){
  
  if(updatedallSL$anomaly[i] > 0)
  {updatedallSL$HorC[i] = "Hot"}
  
  else if(updatedallSL$anomaly[i] < 0)
  {updatedallSL$HorC[i] = "Cold"}
}

#This function calculates seasonal anomaly of each individual
for(i in 1:nrow(updatedallSL)){
  
  if(updatedallSL$Location[i]=='North' & updatedallSL$Season[i]=='Fall')
  {updatedallSL$seasonalAnomaly[i] = updatedallSL$northAVG[i] - nFallMean
   updatedallSL$group[i] = "North-Fall"}
  else if(updatedallSL$Location[i]=='North' & updatedallSL$Season[i]=='Winter')
  {updatedallSL$seasonalAnomaly[i] = updatedallSL$northAVG[i] - nWinterMean
   updatedallSL$group[i] = "North-Winter"}
  
  else if(updatedallSL$Location[i]=='South' & updatedallSL$Season[i]=='Fall')
  {updatedallSL$seasonalAnomaly[i] = updatedallSL$southAVG[i] - sFallMean
   updatedallSL$group[i] = "South-Fall"}
  else if(updatedallSL$Location[i]=='South' & updatedallSL$Season[i]=='Winter')
  {updatedallSL$seasonalAnomaly[i] = updatedallSL$southAVG[i] - sWinterMean
   updatedallSL$group[i] = "South-Winter"}
}

for(i in 1:nrow(updatedallSL)){
  if(updatedallSL$seasonalAnomaly[i] < 0)
  {updatedallSL$HorC_S[i] = "Cold"}
  
  else if(updatedallSL$seasonalAnomaly[i] > 0)
  {updatedallSL$HorC_S[i] = "Hot"}
}



#++++++++++++++++++++++
#Plotting anomaly stuff
#++++++++++++++++++++++

#Create a faceted plot to show seasonal differences between north and south
seasonalPlot <- ggplot(seasonalAverages,aes(as.numeric(levels(Year))[Year],SST,color=as.factor(group),shape=as.factor(group)))+
  geom_point()+
  geom_hline(data = hLineMeans, aes(yintercept = WinterMeans),alpha=0.4,linetype="longdash")+
  geom_hline(data = hLineMeans, aes(yintercept = FallMeans),alpha=0.4,linetype="longdash")+
  labs(title="Annual Shelf SST by Season & Location",x="Years",y="Temperature (°C)")+
  scale_colour_manual(name = "Hot or Cold Year by Season",
                      values = c("blue","blue","red","red"),
                      labels = c("Cold, Fall","Cold, Winter","Hot, Fall","Hot, Winter"))+
  scale_shape_manual(name = "Hot or Cold Year by Season",
                     values = c(19,17,19,17),
                     labels = c("Cold, Fall","Cold, Winter","Hot, Fall","Hot, Winter"))+
  #scale_fill_manual("Seasonal Values",values = c("blue","red"))+
  scale_y_continuous(limits = c(4,26),breaks = seq(min(4),max(26),by=2))+
  scale_x_continuous(breaks = c(seq(1989,2012,by = 2)))+
  facet_grid(Location ~ .)+
  theme_bw() +
  theme(axis.text.x = element_text(angle=45),
        axis.text.y = element_text(angle=45),
        strip.background =element_rect(fill="gray80"),
        legend.box.background = element_rect(colour = "black"))

#save file as jpeg
# jpeg(file="~/Desktop/Analysis/TemperatureAnalysis/Plots/individualSeasonalAnomalies2.jpeg", width = 10, height = 7.5, res = 300, units = "in")
# grid.draw(new_plot)
# dev.off()



#I think the issue here is that when I use annual averages, all of the fall individuals are going to be "Hot" 
# and all of the winter individuals will be "Cold"

indivAnomaly <- ggplot(updatedallSL,aes(x=Date,y=anomaly,color=as.factor(HorC),alpha=anomaly))+
  geom_point(aes(alpha = abs(anomaly)))+ #trying to show strength of anomaly with opacity
  geom_hline(aes(yintercept = 0),linetype="longdash")+
  scale_color_manual(name = "Hot or Cold Individuals",
                     values = c("blue","red"))+
  scale_x_date(date_breaks = ("2 years"),labels = date_format("%Y"),limits = as.Date(c('1989-01-01','2012-12-31')))+
  scale_alpha_continuous(limits = c(0,10),range = c(0.4,1),guide = 'none')+
  scale_y_continuous(limits = c(-10,10))+
  facet_grid(Location ~ .)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45),
        axis.text.y = element_text(angle=45),
        strip.background =element_rect(fill="gray80"),
        legend.box.background = element_rect(colour = "black"))


#This plot is a fix from the last one --> it takes season into account when calculating anomaly
#Want to get blank panel in there for "South - Fall"
#Want to have the strips of the north panel one color and the strips of the south panels another color
indvSeasonAnom <- ggplot(updatedallSL, aes(x=Date,y=seasonalAnomaly,fill=as.factor(HorC_S)))+
  geom_hline(aes(yintercept = 0),linetype="longdash",alpha=0.4)+
  geom_point(pch = 21,color="white")+
  geom_count(show.legend = T,pch=21,color="black")+
  scale_fill_manual(name = "Hot or Cold Individuals",
                     values = c("blue","red"))+
  guides(fill = guide_legend(override.aes = list(pch = 21,color="black",size=2)))+
  scale_x_date(date_breaks = ("2 years"),labels = date_format("%Y"),limits = as.Date(c('1989-01-01','2012-12-31')))+
  labs(title = "Individual Ingress Temperature Anomalies",y="Seasonal Anomaly")+
  scale_y_continuous(limits = c(-8,8))+
  facet_grid(Location+Season ~ .)+
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,vjust = 0.7),
        legend.box.background = element_rect(colour = "black"))

# #NOT USING RIGHT NOW BUT ADDS DUMMY ROW TO DF FOR EMPTY SOUTH-FALL PANEL
# #####
# dummyRow <- updatedallSL[1,]
# dummyRow[1,] <- NA
# dummyRow[1,c(2,5)] <- c("South","Fall")
# updatedallSLdummy <- rbind(updatedallSL,dummyRow)


#Hacking the facet panels
anomalyGrob <- ggplotGrob(indvSeasonAnom)
print(anomalyGrob)
anomalyGrob$grobs[[13]]$grobs[[1]]$children[[1]]$gp$fill <- "tomato"
anomalyGrob$grobs[[14]]$grobs[[1]]$children[[1]]$gp$fill <- "steelblue3"
anomalyGrob$grobs[[15]]$grobs[[1]]$children[[1]]$gp$fill <- "steelblue3"

anomalyGrob$grobs[[13]]$grobs[[2]]$children[[1]]$gp$fill <- "grey85"
anomalyGrob$grobs[[14]]$grobs[[2]]$children[[1]]$gp$fill <- "grey85"

anomalyGrob$grobs[[13]]$grobs[[2]]$children[[1]]$gp$col <- "steelblue3"
anomalyGrob$grobs[[14]]$grobs[[2]]$children[[1]]$gp$col <- "steelblue3"
anomalyGrob$grobs[[15]]$grobs[[2]]$children[[1]]$gp$col <- "tomato"

anomalyGrob$grobs[[13]]$grobs[[2]]$children[[1]]$gp$lwd <- 5
anomalyGrob$grobs[[14]]$grobs[[2]]$children[[1]]$gp$lwd <- 5
anomalyGrob$grobs[[15]]$grobs[[2]]$children[[1]]$gp$lwd <- 5

grid.draw(anomalyGrob)


#WORK IN PROGRESS
# from: https://stackoverflow.com/questions/24169675/multiple-colors-in-a-facet-strip-background
#Too hacky, worked it out above
updatedallSL$facet_fill_color <- c("a","b")[updatedallSL$Location]
## Create main plot
dummy <- indvSeasonAnom
dummy$layers <- NULL
dummy <- dummy + geom_rect(data=updatedallSL, xmin=-Inf, ymin=-Inf, xmax=Inf, ymax=Inf,
                           aes(fill = facet_fill_color))+scale_fill_manual(values = c("steelblue3","tomato"))

library(gtable)

g1 <- ggplotGrob(indvSeasonAnom)
g2 <- ggplotGrob(dummy)

gtable_select <- function (x, ...) 
{
  matches <- c(...)
  x$layout <- x$layout[matches, , drop = FALSE]
  x$grobs <- x$grobs[matches]
  x
}

panels <- grepl(pattern="panel", g2$layout$name)
strips <- grepl(pattern="strip-right", g2$layout$name)
g2$grobs[strips] <- replicate(sum(strips), nullGrob(), simplify = FALSE)
g2$layout$l[panels] <- g2$layout$l[panels] + 1
g2$layout$r[panels] <- g2$layout$r[panels] + 2

new_strips <- gtable_select(g2, panels | strips)
grid.newpage()
grid.draw(new_strips)

gtable_stack <- function(g1, g2){
  g1$grobs <- c(g1$grobs, g2$grobs)
  g1$layout <- rbind(g1$layout, g2$layout)
  g1
}
## ideally you'd remove the old strips, for now they're just covered
new_plot <- gtable_stack(g1, new_strips)
grid.newpage()
grid.draw(new_plot)


                            

















