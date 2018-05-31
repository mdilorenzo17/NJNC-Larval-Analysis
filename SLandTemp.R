setwd("~/Desktop/Analysis/with 50 extras")
larvs <- read.csv('Larval PADE datasheet - Staging Data.csv')
load(file = "~/Desktop/Analysis/with 50 extras/nsYearsdf.RData") #to get annual temp data in here
load(file = "~/Desktop/Analysis/with 50 extras/dailyTemps.RData")

library(ggplot2)
library(scales)
library(gridExtra)
library(dplyr)
library(mgcv)

#create n/s data frames for individuals of each year
n1989 <- as.data.frame(larvs[which(larvs$Year=='1989' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s1989 <- as.data.frame(larvs[which(larvs$Year=='1989' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n1990 <- as.data.frame(larvs[which(larvs$Year=='1990' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s1990 <- as.data.frame(larvs[which(larvs$Year=='1990' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n1991 <- as.data.frame(larvs[which(larvs$Year=='1991' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s1991 <- as.data.frame(larvs[which(larvs$Year=='1991' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n1992 <- as.data.frame(larvs[which(larvs$Year=='1992' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s1992 <- as.data.frame(larvs[which(larvs$Year=='1992' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n1993 <- as.data.frame(larvs[which(larvs$Year=='1993' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s1993 <- as.data.frame(larvs[which(larvs$Year=='1993' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n1998 <- as.data.frame(larvs[which(larvs$Year=='1998' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s1998 <- as.data.frame(larvs[which(larvs$Year=='1998' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n1999 <- as.data.frame(larvs[which(larvs$Year=='1999' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s1999 <- as.data.frame(larvs[which(larvs$Year=='1999' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n2000 <- as.data.frame(larvs[which(larvs$Year=='2000' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s2000 <- as.data.frame(larvs[which(larvs$Year=='2000' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n2001 <- as.data.frame(larvs[which(larvs$Year=='2001' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s2001 <- as.data.frame(larvs[which(larvs$Year=='2001' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n2002 <- as.data.frame(larvs[which(larvs$Year=='2002' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s2002 <- as.data.frame(larvs[which(larvs$Year=='2002' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n2003 <- as.data.frame(larvs[which(larvs$Year=='2003' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s2003 <- as.data.frame(larvs[which(larvs$Year=='2003' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n2008 <- as.data.frame(larvs[which(larvs$Year=='2008' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s2008 <- as.data.frame(larvs[which(larvs$Year=='2008' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n2009 <- as.data.frame(larvs[which(larvs$Year=='2009' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s2009 <- as.data.frame(larvs[which(larvs$Year=='2009' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n2010 <- as.data.frame(larvs[which(larvs$Year=='2010' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s2010 <- as.data.frame(larvs[which(larvs$Year=='2010' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n2011 <- as.data.frame(larvs[which(larvs$Year=='2011' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s2011 <- as.data.frame(larvs[which(larvs$Year=='2011' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

n2012 <- as.data.frame(larvs[which(larvs$Year=='2012' & larvs$North.or.South=='n'),c(1,3,5,6,8,13,14)])
s2012 <- as.data.frame(larvs[which(larvs$Year=='2012' & larvs$North.or.South=='s'),c(1,3,5,6,8,13,14)])

northernSL <- rbind(n1989,n1990,n1991,n1992,n1993,n1998,n1999,n2000,n2001,n2002,n2003,n2008,n2009,n2010,n2011,n2012)
southernSL <- rbind(s1989,s1990,s1991,s1992,s1993,s1998,s1999,s2000,s2001,s2002,s2003,s2008,s2009,s2010,s2011,s2012)
allSL <- rbind(northernSL,southernSL)
allSL <- allSL[order(allSL$Year),]
allSL <- transform(allSL,counter = c(1:nrow(allSL)))
colnames(allSL) <- c("Year","Location","ID","Date","Season","AverageSL","stage","counter")
allSL <- na.omit(allSL)
allSL$counter <- as.numeric(allSL$counter)
#Reformatting the date column here
allSL$Date <- format(as.Date(allSL$Date, format = "%m/%d/%Y"), "%Y-%m-%d")
allSL$Date <- as.Date(allSL$Date)
allSL <- transform(allSL, jd = as.Date(allSL$Date, format = "%d%b%y"))
allSL$jd <- format(allSL$jd,"%j")
allSL$jd <- as.numeric(allSL$jd)
#to get premetamorph and F out of facet
updatedallSL <- allSL[which(allSL$stage != "F" & allSL$stage != "PREMETAMORPH"),]
#change location names for aesthetics
levels(updatedallSL$Location)[levels(updatedallSL$Location) %in%  "n"] <- "North"
levels(updatedallSL$Location)[levels(updatedallSL$Location) %in%  "s"] <- "South"

years <- c(1989:2012)
SLTempPlot <- 
  ggplot(nsYears.df)+
  geom_line(aes(years,north.SST,colour = "NORTH"))+
  geom_line(aes(years,south.SST,colour = "SOUTH"))+
  geom_point(data = allSL[allSL$Location=='n',],aes(Year,AverageSL,colour="NORTH"),size=1)+
  geom_point(data = allSL[allSL$Location=='s',],aes(Year,AverageSL,colour="SOUTH"),size=1)+
  #scale_shape_manual("Standard Length Location",values=c(19, 18))+
  labs(title="Standard Length of Individuals vs. Annual Temperature Averages",x="Years",y="Temperature (°C)")+
  scale_colour_manual("Temperature Location",values = c("steelblue3","tomato"))+
  scale_x_continuous(breaks = seq(min(years), max(years), by = 2))+
  scale_y_continuous(limits = c(4,26),breaks = seq(min(4),max(26),by=2),sec.axis = sec_axis(~ . *1,name  = "Average Standard Length (mm)"))
  theme_bw() +
  theme(axis.text.x = element_text(angle=45),
        axis.text.y = element_text(angle=45))

#jpeg(file="~/Desktop/Analysis/TemperatureAnalysis/Plots/StandardLengthvsTemp.jpeg", width = 10, height = 7.5, res = 300, units = "in")
#SLTempPlot
#dev.off()  

 

test1<-ggplot(allSL[allSL$Location=='n' & allSL$Year==1990,], aes(x = Date, y = AverageSL)) + 
  geom_point(aes(color="north")) +
  geom_point(data = allSL[allSL$Location=='s' & allSL$Year==1990,],aes(x = Date, y = AverageSL,color="south"))+
  geom_line(data = dailyTemps,aes(x = Date, y = north, color = "north"))+
  geom_line(data = dailyTemps,aes(x = Date, y = south, color = "south"))+
  theme_bw() +
  scale_color_manual("Location",values = c("blue","red"))+
  scale_y_continuous(limits = c(4,30),breaks = seq(min(4),max(30),by=2),sec.axis = sec_axis(~ . *1,name  = "Daily Temp"))+
  labs(title = "1990", x = "", y = "Average SL") +
  scale_x_date(date_breaks = ("months"), labels = date_format("%b"),limits = as.Date(c('1990-01-01','1990-12-31')))

test2<-ggplot(allSL[allSL$Location=='n' & allSL$Year==1991,], aes(x = Date, y = AverageSL)) + 
  geom_point(aes(color="north")) +
  geom_point(data = allSL[allSL$Location=='s' & allSL$Year==1991,],aes(x = Date, y = AverageSL,color="south"))+
  geom_line(data = dailyTemps,aes(x = Date, y = north, color = "north"))+
  geom_line(data = dailyTemps,aes(x = Date, y = south, color = "south"))+
  theme_bw() +
  scale_color_manual("Location",values = c("blue","red"))+
  scale_y_continuous(limits = c(4,30),breaks = seq(min(4),max(30),by=2),sec.axis = sec_axis(~ . *1,name  = "Daily Temp"))+
  labs(title = "1991", x = "", y = "Average SL") +
  scale_x_date(date_breaks = ("months"),labels = date_format("%b"),limits = as.Date(c('1991-01-01','1991-12-31')))






#jpeg(file="~/Desktop/Analysis/TemperatureAnalysis/Plots/TestPlot.jpeg", width = 10, height = 7.5, res = 300, units = "in")
#grid.arrange(test1,test2)
#dev.off() 


#Honestly, this is just for visualization
#all this data can be seen on the "main" plot, it just helps to zoom in and look at each year 
  plot(AverageSL~Date,data=allSL[allSL$Location=='n' & allSL$Year==1990,],xlim=c(as.Date("1990-1-1"),as.Date("1990-12-31")),pch=16,col='blue')
  #Here, the mean temperature in april,1990 = 10.82995
  #   During this time period, the Average SL =
  
  #The mean temp in December,1990 = 11.5505
  #   During this time period, the Average SL = 
  points(AverageSL~Date,data=allSL[allSL$Location=='s' & allSL$Year==1990,],pch=16,col='red')
  
 
  
  
#adding a counter into the data frame to count all of the days
dailyTemps <- transform(dailyTemps,counter = c(1:nrow(dailyTemps)))
  
  
  #This is a work in progress
 
  ggregression <- ggplot(updatedallSL, aes(x = Date, y = AverageSL)) + 
    geom_point(aes(fill = factor(Location)),pch = 21,color = "black")+
    #geom_point(data = updatedallSL[updatedallSL$Location=='n',], aes(color="north")) +
    #geom_point(data = updatedallSL[updatedallSL$Location=='s',],aes(color="south"))+
    #geom_line(data = dailyTemps,aes(x = Date, y = north, color = "north"))+
    #geom_line(data = dailyTemps,aes(x = Date, y = south, color = "south"))+
    #facet_grid(Year ~ .)+
    geom_smooth(data = updatedallSL[which(updatedallSL$Location=='North'),],method = 'lm',aes(color = "North SL Fit"),size = .5)+
    geom_smooth(data = updatedallSL[which(updatedallSL$Location=='South'),],method = 'lm',aes(color = "South SL Fit"), size = .5)+
    geom_smooth(data = dailyTemps,aes(x= Date,y= north,color = "Northern Temp Fit"), method = 'gam',formula = y~s(x),size = 1.3)+
    geom_smooth(data = dailyTemps,aes(x= Date,y= south,color = "Southern Temp Fit"), method = 'gam',formula = y~s(x),size = 1.3)+
    #geom_line(data = dailyTemps,aes(x= Date,y= south,color = "Southern Temp Fit")) +
    geom_line(data = dailyTemps,aes(x= Date,y= north,color = "Northern Temp Fit")) +
    #facet_grid(. ~ stage)+
    theme_bw() +
    scale_fill_manual("Location",values = c("blue","red"))+
    scale_color_manual("Fitted Curve",values = c("blue","lightskyblue","red","lightpink"))+
    #scale_y_continuous(limits = c(4,30),breaks = seq(min(4),max(30),by=2),sec.axis = sec_axis(~ . *1,name  = "Daily Temp"))+
    labs(title = "GAM Temperature Regression", x = "Year", y = "Average Standard Length (mm)")+
    #scale_x_continuous(limits = c(0,365),breaks = c(0,100,200,300))
    scale_y_continuous(sec.axis = sec_axis(~ . *1,name  = "SST (°C)"))
    scale_x_date(date_breaks = ("2 years"),labels = date_format("%Y"),limits = as.Date(c('1989-01-01','2012-12-31')))
  
  jpeg(file="~/Desktop/Analysis/TemperatureAnalysis/Plots/ggRegressionWithDailyTemps.jpeg", width = 10, height = 7.5, res = 300, units = "in")
  ggregression
  dev.off()
    
#This is a dplyr function used to join data with similar dates 
updatedallSL <- left_join(updatedallSL, dailyTemps, by="Date")
updatedallSL <- updatedallSL[c("Year","Location","ID","Date","Season","AverageSL","stage","jd","north","south")]
colnames(updatedallSL)[9] <- "NorthTemps"
colnames(updatedallSL)[10] <- "SouthTemps" 

northTemps <- ggplot(updatedallSL[updatedallSL$Location=='North',], aes(x = NorthTemps, y = AverageSL))+
  geom_point()+
  geom_smooth(method='lm')+
  scale_y_continuous(limits = c(4,30),breaks = seq(min(4),max(30),by=2))+
  scale_x_continuous(limits = c(4,22),breaks = seq(min(4),max(30),by=2))

southTemps <- ggplot(updatedallSL[updatedallSL$Location=='South',], aes(x = SouthTemps, y = AverageSL))+
  geom_point()+
  geom_smooth(method='lm',aes(color='red'))+
  scale_y_continuous(limits = c(4,30),breaks = seq(min(4),max(30),by=2))+
  scale_x_continuous(limits = c(4,22),breaks = seq(min(4),max(30),by=2))+
  guides(color=FALSE)
grid.arrange(northTemps,southTemps)


#this is all just messing around with GAMS to see what the model is doing
#not quite sure what it all means but I'm concerned about how well this type of model is fitting my daily temperature data
#from https://www.fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
  gamTest <- gamm(north~s(counter,bs="cc"),data=dailyTemps)
  summary(gamTest$gam)
  layout(matrix(1:1, ncol = 1))
  plot(gamTest$gam, scale = 0)
  layout(1)
  
  layout(matrix(1:2, ncol = 2))
  acf(resid(gamTest$lme), lag.max = 36, main = "ACF")
  pacf(resid(gamTest$lme), lag.max = 36, main = "pACF")
  layout(1)
####  
  
#Just checking out northern individuals  
ggplot(updatedallSL[updatedallSL$Location=='North',], aes(x = Date, y = AverageSL)) + 
  geom_point(aes(fill = factor(Season)),pch = 21,color = "black")+
  scale_fill_manual("Location",values = c("red","blue"))
  
#seeing how the trends look when seasons are split up
#still seems that the largest individuals are found at the lowest temperature
x <- ggplot(updatedallSL[updatedallSL$Location=='North' & updatedallSL$Season=='Fall',],aes(x = NorthTemps, y = AverageSL))+
  geom_smooth(method = 'lm')
y <- ggplot(updatedallSL[updatedallSL$Location=='North' & updatedallSL$Season=='Winter',],aes(x = NorthTemps, y = AverageSL))+
  geom_smooth(method = 'lm',color='red')
grid.arrange(x,y)

  