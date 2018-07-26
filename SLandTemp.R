setwd("~/Desktop/Analysis/with 50 extras")
larvs <- read.csv('Larval PADE datasheet - Staging Data.csv')
load(file = "~/Desktop/Analysis/with 50 extras/nsYearsdf.RData") #to get annual temp data in here
load(file = "~/Desktop/Analysis/with 50 extras/dailyTemps.RData")

library(grid)
library(ggplot2)
library(ggpubr)
library(gtable)
library(scales)
library(gridExtra)
library(dplyr)
library(mgcv)
library(multiplex)
library(reshape2)
library(dplyr)
library(nnet)
library(MASS)

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
  scale_y_continuous(limits = c(4,26),breaks = seq(min(4),max(26),by=2),sec.axis = sec_axis(~ . *1,name  = "Average Standard Length (mm)"))+
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

#This is a dplyr function used to join data with similar dates 


updatedallSL <- updatedallSL[order(updatedallSL$Date),]

#Average the previous 30 days temperature for each individual 
#(make their "unique" temperature be the average of the previous 30 days prior to being collected)
#North
for(i in 1:8766) {
  if (i > 30){
    dailyTemps$northAVG[i] <- mean(dailyTemps$north[(i-30):i])  
  }
  else {
    dailyTemps$northAVG[i] <- NA
  }
}

#South
for(i in 1:8766) {
  if (i > 30){
    dailyTemps$southAVG[i] <- mean(dailyTemps$south[(i-30):i])  
  }
  else {
    dailyTemps$southAVG[i] <- NA
  }
}

  
#ADD THESE COLUMNS TO UPDATEDALLSL DF
#northAVG = 30-day temp average on that collection date in the northern region
#southAVG = " in the southern region
updatedallSL <- left_join(updatedallSL, dailyTemps, by="Date")
updatedallSL <- subset(updatedallSL, select = -c(8,12,13,14,15))

#Average 60 days now
for(i in 1:8766) {
  if (i > 60){
    dailyTemps$northAVG60[i] <- mean(dailyTemps$north[(i-60):i])  
  }
  else {
    dailyTemps$northAVG60[i] <- NA
  }
}

#South
for(i in 1:8766) {
  if (i > 60){
    dailyTemps$southAVG60[i] <- mean(dailyTemps$south[(i-60):i])  
  }
  else {
    dailyTemps$southAVG60[i] <- NA
  }
}

# don't do this a second time
# updatedallSL <- left_join(updatedallSL, dailyTemps, by="Date")
# updatedallSL <- subset(updatedallSL, select = -c(11,12,13,14,15,16,17,18))
# colnames(updatedallSL) <- c("Year","Location","ID","Date","Season","AverageSL","stage","jd","north","south","northAVG","southAVG","northAVG60","southAVG60")

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
    geom_line(data = dailyTemps,aes(x= Date,y= north,color = "Northern Daily Temps")) +
    #facet_grid(Season ~ .)+
    theme_bw() +
    scale_fill_manual("Location",values = c("blue","red"))+
    scale_color_manual("Fitted Curve",values = c("blue","gray","lightskyblue","red","lightpink"))+
    #scale_y_continuous(limits = c(4,30),breaks = seq(min(4),max(30),by=2),sec.axis = sec_axis(~ . *1,name  = "Daily Temp"))+
    labs(title = "GAM Temperature Regression", x = "Year", y = "Average Standard Length (mm)")+
    #scale_x_continuous(limits = c(0,365),breaks = c(0,100,200,300))
    scale_y_continuous(sec.axis = sec_axis(~ . *1,name  = "SST (°C)"))+
    scale_x_date(date_breaks = ("2 years"),labels = date_format("%Y"),limits = as.Date(c('1989-01-01','2012-12-31')))
  
  #jpeg(file="~/Desktop/Analysis/TemperatureAnalysis/Plots/ggRegressionWithDailyTemps.jpeg", width = 10, height = 7.5, res = 300, units = "in")
  #ggregression
  #dev.off()
    
updatedallSL$stage_f <- factor(updatedallSL$stage, levels = c("G","H-","H","H+","I"))

northTemps <- ggplot(updatedallSL[updatedallSL$Location=='North',], aes(x = northAVG, y = AverageSL))+
  geom_point()+
  geom_smooth(method='lm',se=F ,aes(color='North'))+
  scale_y_continuous(limits = c(4,30),breaks = seq(min(4),max(30),by=4))+
  scale_x_continuous(limits = c(4,22),breaks = seq(min(4),max(30),by=2))+
  ggtitle("North")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        plot.title = element_text(margin = margin(t=0,b=-10),hjust = 0.5,size = 10),
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size=13))+
  scale_color_manual("Location",values = 'blue')+
  facet_grid(stage_f ~ .)+
  guides(color=FALSE)

 

southTemps <- ggplot(updatedallSL[updatedallSL$Location=='South',], aes(x = southAVG, y = AverageSL))+
  geom_point()+
  geom_smooth(method='lm',se=F,aes(color='South'))+
  scale_y_continuous(limits = c(4,30),breaks = seq(min(4),max(30),by=2))+
  scale_x_continuous(limits = c(4,22),breaks = seq(min(4),max(30),by=2))+
  ggtitle("South")+
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 13),
        plot.title = element_text(margin = margin(t=0,b=-10),hjust = 0.5,size = 10))+
  facet_grid(stage_f ~ .)+
  scale_color_manual("Location",values = 'tomato')+
  guides(color=FALSE)

grid.arrange(northTemps,
             southTemps,
             #top = "Average SL (mm) vs. 30-Day Temperature Averages",
             left = "Average Standard Length (mm)",
             bottom = "Temperature (°C)",
             nrow = 1,
             ncol = 2)



 
TempASL <- ggplot(updatedallSL, aes(x=specificTemp,y=AverageSL,color=Location))+
  geom_point(aes(fill = "black"),pch = 21,colour = "black",show.legend = F)+
  geom_smooth(method = 'lm', se = F,show.legend = F)+
  facet_grid(stage_f ~ Location)+
  scale_color_manual("Location", values = c("dodgerblue1","firebrick2"))+
  scale_fill_manual(values = "black")+
  xlab("Temperature (°C)")+
  ylab("Average Standard Length (mm)")+
  theme_bw()+
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size=13))



TempASL.grob <- ggplotGrob(TempASL)
#print(TempASL.grob)
TempASL.grob$grobs[[26]]$grobs[[1]]$children[[1]]$gp$fill <- "dodgerblue1"
TempASL.grob$grobs[[27]]$grobs[[1]]$children[[1]]$gp$fill <- "firebrick2"
TempASL.grob$grobs[[26]]$grobs[[1]]$children[[2]]$children[[1]]$label <- "Little Egg Inlet, NJ"
TempASL.grob$grobs[[27]]$grobs[[1]]$children[[2]]$children[[1]]$label <- "Beaufort, NC"

jpeg(file="~/Desktop/Analysis/AFS/TempASL30dayAVG.jpeg", width = 10, height = 7.5, res = 300, units = "in")
grid.draw(TempASL.grob)
dev.off()

#getting p-values
Location <- c(rep("North",5),rep("South",5))
Stage <- rep(c("G","H-","H","H+","I"),2)
p.values <- cbind(Location,Stage)
p.values <- as.data.frame(p.values)

for(i in 1:nrow(p.values)){
  p.values$pval[i] = summary(lm(AverageSL ~ specificTemp, 
                        data = updatedallSL[which(updatedallSL$Location==p.values$Location[i] & updatedallSL$stage_f==p.values$Stage[i]),]))$coeff[2,4]
}




#this is all just messing around with GAMS to see what the model is doing
#not quite sure what it all means but I'm concerned about how well this type of model is fitting my daily temperature data
#from https://www.fromthebottomoftheheap.net/2014/05/09/modelling-seasonal-data-with-gam/
#   gamTest <- gamm(north~s(counter,bs="cc"),data=dailyTemps)
#   summary(gamTest$gam)
#   layout(matrix(1:1, ncol = 1))
#   plot(gamTest$gam, scale = 0)
#   layout(1)
#   
#   layout(matrix(1:2, ncol = 2))
#   acf(resid(gamTest$lme), lag.max = 36, main = "ACF")
#   pacf(resid(gamTest$lme), lag.max = 36, main = "pACF")
#   layout(1)
# ####  
#   
# #Just checking out northern individuals  
# ggplot(updatedallSL[updatedallSL$Location=='North',], aes(x = Date, y = AverageSL)) + 
#   geom_point(aes(fill = factor(Season)),pch = 21,color = "black")+
#   scale_fill_manual("Location",values = c("red","blue"))
  
#seeing how the trends look when seasons are split up
#still seems that the largest individuals are found at the lowest temperature




#adding stage "rank" to dataframe
for(i in 1:nrow(updatedallSL)) {
  if (updatedallSL$stage[i] == 'G'){updatedallSL$LikertRank[i] <- 1}
  else if (updatedallSL$stage[i] == 'H-'){updatedallSL$LikertRank[i] <- 2}
  else if (updatedallSL$stage[i] == 'H'){updatedallSL$LikertRank[i] <- 3}
  else if (updatedallSL$stage[i] == 'H+'){updatedallSL$LikertRank[i] <- 4} 
  else if (updatedallSL$stage[i] == 'I'){updatedallSL$LikertRank[i] <- 5}
  else {updatedallSL$LikertRank[i] <- NA}
}

#Plotting likert rank vs temp
northStage <- ggplot(updatedallSL[updatedallSL$Location=='North',], aes(x = northAVG, y = LikertRank))+
  geom_point(aes(color = northAVG))+
  #geom_smooth(method='lm',aes(color='North'))+
  scale_y_continuous(limits = c(1,5),breaks = seq(min(1),max(5),by=1))+
  scale_x_continuous(limits = c(4,22),breaks = seq(min(4),max(30),by=2))+
  ggtitle("North")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        plot.title = element_text(margin = margin(t=0,b=-10),size = 10))+
  #scale_color_manual("Location",values = 'blue')+
  scale_color_gradient(low = "blue", high = "red",limits = c(4,22))+
  guides(color=FALSE)


southStage <- ggplot(updatedallSL[updatedallSL$Location=='South',], aes(x = southAVG, y = LikertRank))+
  geom_point(aes(color = southAVG))+
  #geom_smooth(method='lm',aes(color='South'))+
  scale_y_continuous(limits = c(1,5),breaks = seq(min(1),max(5),by=1))+
  scale_x_continuous(limits = c(4,22),breaks = seq(min(4),max(30),by=2))+
  ggtitle("South")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(margin = margin(t=0,b=-10),size = 10))+
  scale_color_gradient(low = "blue", high = "red",limits = c(4,22))+
  #scale_color_manual("Location",values = 'red')+
  guides(color=FALSE)

#jpeg(file="~/Desktop/Analysis/TemperatureAnalysis/Plots/TempStage30dayAVG.jpeg", width = 10, height = 7.5, res = 300, units = "in")
#grid.arrange(northStage,
#             southStage,
#            top = "Likert Rank (stage) vs. 30-day Temperature Averages",
#             left = "Likert Rank",
#             bottom = "Temperature (°C)")
#dev.off()


#julian day plot
#uses Jan. 1 of each year as the "origin", counts days in year until the next Jan. 1
#using this to see how size distribution looks in a single year and how it changes over consecutive years

#jpeg(file="~/Desktop/Analysis/TemperatureAnalysis/Plots/JDvsASL.jpeg", width = 10, height = 7.5, res = 300, units = "in")
grid.arrange(
ggplot(updatedallSL[updatedallSL$Location == 'North',],aes(x = jd, y = AverageSL))+
  geom_point(aes(color=factor(Season)))+
  scale_x_continuous(limits = c(1,365), breaks = seq(min(0),max(365),by = 150))+
  scale_y_continuous(limits = c(8,23), breaks = seq(min(8),max(23),by = 3))+
  scale_color_manual("Season",values = c("red","blue"))+
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank())+
  facet_grid(. ~ Year),

ggplot(updatedallSL[updatedallSL$Location == 'South',],aes(x = jd, y = AverageSL))+
  geom_point(aes(color=factor(Season)))+
  scale_x_continuous(limits = c(1,365), breaks = seq(min(0),max(365),by = 150))+
  scale_y_continuous(limits = c(8,23), breaks = seq(min(8),max(23),by = 3))+
  scale_color_manual("Season",values = c("blue"))+
  xlab("Julian Day")+
  theme( axis.title.y = element_blank())+
  facet_grid(. ~ Year),

left = "Average Standard Length (mm)"
)
#dev.off()


breaks <- c(5,7,9,11,13,15,17,19,21,23)
labels <- c("< 7","[7,9)","[9,11)","[11,13)","[13,15)","[15,17)","[17,19)","[19,21)","[21,23)")
labels2 <-c("(5,7]","(7,9]","(9,11]","(11,13]","(13,15]","(15,17]","(17,19]","(19,21]","(21,23]")
northBins <- cut(updatedallSL[updatedallSL$Location == 'North',]$northAVG, breaks, include.lowest = T,right = FALSE,labels = labels)
southBins <- cut(updatedallSL[updatedallSL$Location == 'South',]$southAVG, breaks,labels = labels)

NbinCol <- cbind(updatedallSL[updatedallSL$Location=='North',],northBins)
SbinCol <- cbind(updatedallSL[updatedallSL$Location=='South',],southBins)

NbinCol$stage_f <- factor(NbinCol$stage, levels = c("G","H-","H","H+","I"))
SbinCol$stage_f <- factor(SbinCol$stage, levels = c("G","H-","H","H+","I"))
test <- NbinCol$northBins

#Plots for counts in each temp bin (divided into stages)
#This was the original idea
#For some reason when I facet by stage, the counts get messed up
##**FIX** -> dont use '$' in aes, just use column name 

#jpeg(file="~/Desktop/Analysis/TemperatureAnalysis/Plots/TempBinsFacet.jpeg", width = 10, height = 7.5, res = 300, units = "in")
grid.arrange(
  ggplot(data = NbinCol, aes(x=northBins, fill = ..count..))+
    geom_bar(color='black', alpha= 0.9) + 
    stat_count(geom="text", aes(label=..count..), vjust = -.5) +
    theme_bw() + 
    labs(title = "North")+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5,size = 11))+
    ylim(0,50) +
    facet_grid(stage_f ~ .)+
    scale_x_discrete(drop=FALSE)+
    guides(fill=FALSE),
  
ggplot(data = SbinCol, aes(x=southBins, fill = ..count..))+
  geom_bar(color='black', alpha= 0.9) + 
  stat_count(geom="text", aes(label=..count..), vjust = -0.5) +
  theme_bw() +
  labs(title = "South")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 11))+
  ylim(0,50) +
  facet_grid(stage_f ~ .)+
  scale_x_discrete(drop=FALSE)+
  guides(fill=FALSE),

nrow = 1,
ncol = 2,
bottom = "Temperature Range (°C)",
left = "Number of Individuals",
top = "Individual Counts by Temperature Bin"
)
#dev.off()



#===================================================
# New Julian Day analysis (adjusting for season)
#===================================================

# require(nnet)
# test <- multinom(LikertRank ~ jd + northAVG, data=updatedallSL[which(updatedallSL$Location=='North'),])

for(i in 1:nrow(updatedallSL)){
if (updatedallSL$Season[i] == 'Fall')
  {updatedallSL$jd_adj[i] = updatedallSL$jd[i]  - 273} #makes day 1 for fall October first (may be a day off for leap years)
else 
  {updatedallSL$jd_adj[i] = updatedallSL$jd[i]}
}

jd_breaks <- c(0,16,32,48,64,80,96) 
jdFallLabels <- c("[0,16)","[16,32)","[32,48)","[48,64)","[64,80)","[80,96)")
jdFallBins <- cut(updatedallSL[updatedallSL$Season == 'Fall',]$jd_adj, jd_breaks, include.lowest = T, right = FALSE,labels = jdFallLabels)    

#Have to run "HorC" functions in other script before running this!!
jdFallCol <- cbind(updatedallSL[updatedallSL$Season=='Fall',],jdFallBins)

# CHECK THIS
colnames(jdFallCol)[23] <- "Bin"
#jdFallCol <- jdFallCol[,c(7,17,23)]

jdWinterBins <- cut(updatedallSL[updatedallSL$Season == 'Winter' & updatedallSL$jd < 96,]$jd_adj, jd_breaks, include.lowest = T, right = FALSE,labels = jdFallLabels)
jdWinterCol <- cbind(updatedallSL[updatedallSL$Season=='Winter' & updatedallSL$jd < 96,],jdWinterBins)
colnames(jdWinterCol)[23] <- "Bin"
#jdWinterCol <- jdWinterCol[,c(7,17,23)]

jd_adjBins <- cut(updatedallSL[updatedallSL$jd_adj < 96,]$jd_adj, jd_breaks, include.lowest = T, right = FALSE,labels = jdFallLabels)
jd_adjCol <- cbind(updatedallSL[updatedallSL$jd_adj < 96,],jd_adjBins)
jd_adjCol <- jd_adjCol[,c(2,5,7,11,12,16,17,18)]
  



#Want to change this to geom_points with a regression line
ggplot(data = jdFallCol, aes(x=jdFallBins,fill = ..count..))+
  geom_bar(color='black', ,alpha= 0.9) + 
  stat_count(geom="text", aes(label=..count..), vjust = -.5) +
  theme_bw() + 
  #labs(title = "North")+
  #theme(axis.title.x = element_blank(),
   #     axis.title.y = element_blank(),
    #    plot.title = element_text(hjust = 0.5,size = 11))+
  ylim(0,30) +
  facet_grid(stage ~ .)+
  scale_x_discrete(drop=FALSE)
  #guides(fill=FALSE)  
  

#trying to use density plot to show distribution kernel
#jpeg(file="~/Desktop/Analysis/TemperatureAnalysis/Plots/adjJDdensity.jpeg", width = 10, height = 7.5, res = 300, units = "in")
grid.arrange(
ggplot(jdFallCol[jdFallCol$stage!="I",],aes(Bin,color=stage,group=stage,fill=stage))+
  geom_density(alpha=0.1)+
  labs(title="Fall")+
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(margin = margin(t=0,b=-10),hjust = 0.5,size = 11)),
ggplot(subset(jdWinterCol[jdWinterCol$stage!="I",],!is.na(jdWinterBins)),aes(Bin,color=stage,group=stage,fill=stage))+
  geom_density(alpha=0.1)+
  labs(title="Winter")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(margin = margin(t=0,b=-10),hjust = 0.5,size = 11))+
  ylim(0,0.6),
bottom = "Julian Day Bin",
left = "density"
)
#dev.off()

#combined fall and spring of all individuals (North and South)
#this is good for now, still not sure if I should be using geom_density for a discrete scale
grid.arrange(
ggplot(subset(jd_adjCol[jd_adjCol$stage!='I',],Location == 'North'),aes(jd_adjBins,color=stage,group=stage,fill=stage))+
  geom_density(alpha=0.1),

ggplot(subset(jd_adjCol[jd_adjCol$stage!='I',],Location == 'South'),aes(jd_adjBins,color=stage,group=stage,fill=stage))+
  geom_density(alpha=0.1)+
  scale_x_discrete(limits = c("[0,16)","[16,32)","[32,48)","[48,64)","[64,80)","[80,96)"))
)


#Want to compare hot and cold years
#calculating proportions based off (a) stage, (b) bin, and (c) hot/cold
#combining jdfallcol and jdwinter col to have all data in single data frame
proportion.df <- rbind(jdFallCol,jdWinterCol)
#proportion.df <- proportion.df[,c(1:18,20:24)]
stages <- c("G","G","H-","H-","H","H","H+","H+","I","I")
new.stages <- c(rep("G",4),rep("H-",4),rep("H",4),rep("H+",4),rep("I",4))

PropCalc.df<- data.frame("Bin" = c(rep(jdFallLabels[[1]],10),rep(jdFallLabels[[2]],10),rep(jdFallLabels[[3]],10),rep(jdFallLabels[[4]],10),
                         rep(jdFallLabels[[5]],10),rep(jdFallLabels[[6]],10)),
                         "Stage" = (rep(stages,6)),
                         "HorC" = rep(c("Hot","Cold"),6),
                         "Prop" = 0)
PropCalc.df$Stage <- as.character(PropCalc.df$Stage)
PropCalc.df$HorC <- as.character(PropCalc.df$HorC)


#Adding Season in
PropCalc.df2<- data.frame("Bin" = c(rep(jdFallLabels[[1]],20),rep(jdFallLabels[[2]],20),rep(jdFallLabels[[3]],20),rep(jdFallLabels[[4]],20),
                                   rep(jdFallLabels[[5]],20),rep(jdFallLabels[[6]],20)),
                         "Stage" = rep(new.stages,6),
                         "HorC" = rep(c("Hot","Hot","Cold","Cold"),5),
                         "Location" = rep(c("North","South"),5),
                         "Prop" = 0)
PropCalc.df2$Location <- as.character(PropCalc.df2$Location)
PropCalc.df2$Stage <- as.character(PropCalc.df2$Stage)
PropCalc.df2$HorC <- as.character(PropCalc.df2$HorC)

#calculations

#Bin Counts
bin1 <- summary(proportion.df$Bin)[[1]]
bin2 <- summary(proportion.df$Bin)[[2]]
bin3 <- summary(proportion.df$Bin)[[3]]
bin4 <- summary(proportion.df$Bin)[[4]]
bin5 <- summary(proportion.df$Bin)[[5]]
bin6 <- summary(proportion.df$Bin)[[6]]

#Calculating all of the proportions
for(i in 1:nrow(PropCalc.df)){

  if(PropCalc.df$Stage[i]=="G" & PropCalc.df$HorC[i]=="Hot")
  {PropCalc.df$Prop[i] = nrow(proportion.df[which(proportion.df$stage=="G" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df$Bin[i])),])/
      nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df$Bin[i]),])}
  else if(PropCalc.df$Stage[i]=="G" & PropCalc.df$HorC[i]=="Cold")
  {PropCalc.df$Prop[i] = nrow(proportion.df[which(proportion.df$stage=="G" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df$Bin[i]),])}
  
  else if(PropCalc.df$Stage[i]=="H-" & PropCalc.df$HorC[i]=="Hot")
  {PropCalc.df$Prop[i] = nrow(proportion.df[which(proportion.df$stage=="H-" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df$Bin[i]),])}
  else if(PropCalc.df$Stage[i]=="H-" & PropCalc.df$HorC[i]=="Cold")
  {PropCalc.df$Prop[i] = nrow(proportion.df[which(proportion.df$stage=="H-" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df$Bin[i]),])}
  
  else if(PropCalc.df$Stage[i]=="H" & PropCalc.df$HorC[i]=="Hot")
  {PropCalc.df$Prop[i] = nrow(proportion.df[which(proportion.df$stage=="H" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df$Bin[i]),])}
  else if(PropCalc.df$Stage[i]=="H" & PropCalc.df$HorC[i]=="Cold")
  {PropCalc.df$Prop[i] = nrow(proportion.df[which(proportion.df$stage=="H" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df$Bin[i]),])}
  
  else if(PropCalc.df$Stage[i]=="H+" & PropCalc.df$HorC[i]=="Hot")
  {PropCalc.df$Prop[i] = nrow(proportion.df[which(proportion.df$stage=="H+" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df$Bin[i]),])}
  else if(PropCalc.df$Stage[i]=="H+" & PropCalc.df$HorC[i]=="Cold")
  {PropCalc.df$Prop[i] = nrow(proportion.df[which(proportion.df$stage=="H+" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df$Bin[i]),])}
  
  else if(PropCalc.df$Stage[i]=="I" & PropCalc.df$HorC[i]=="Hot")
  {PropCalc.df$Prop[i] = nrow(proportion.df[which(proportion.df$stage=="I" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df$Bin[i]),])}
  else if(PropCalc.df$Stage[i]=="I" & PropCalc.df$HorC[i]=="Cold")
  {PropCalc.df$Prop[i] = nrow(proportion.df[which(proportion.df$stage=="I" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df$Bin[i]),])}
}

#having trouble with this function (Location doesn't seem to be working)
#Working Now, just had to change from factors to characters in "propcalc.df2
for(i in 1:nrow(PropCalc.df2)){
  
  if(PropCalc.df2$Stage[i]=="G" & PropCalc.df2$HorC[i]=="Hot" & PropCalc.df2$Location[i]=="North")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="North" & proportion.df$stage=="G" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="G" & PropCalc.df2$HorC[i]=="Hot" & PropCalc.df2$Location[i]=="South")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="South" & proportion.df$stage=="G" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="G" & PropCalc.df2$HorC[i]=="Cold" & PropCalc.df2$Location[i]=="North")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="North" & proportion.df$stage=="G" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="G" & PropCalc.df2$HorC[i]=="Cold" & PropCalc.df2$Location[i]=="South")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="South" & proportion.df$stage=="G" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  
  
  else if(PropCalc.df2$Stage[i]=="H-" & PropCalc.df2$HorC[i]=="Hot" & PropCalc.df2$Location[i]=="North")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="North" & proportion.df$stage=="H-" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="H-" & PropCalc.df2$HorC[i]=="Hot" & PropCalc.df2$Location[i]=="South")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="South" & proportion.df$stage=="H-" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="H-" & PropCalc.df2$HorC[i]=="Cold" & PropCalc.df2$Location[i]=="North")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="North" & proportion.df$stage=="H-" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="H-" & PropCalc.df2$HorC[i]=="Cold" & PropCalc.df2$Location[i]=="South")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="South" & proportion.df$stage=="H-" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  
  else if(PropCalc.df2$Stage[i]=="H" & PropCalc.df2$HorC[i]=="Hot" & PropCalc.df2$Location[i]=="North")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="North" & proportion.df$stage=="H" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="H" & PropCalc.df2$HorC[i]=="Hot" & PropCalc.df2$Location[i]=="South")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="South" & proportion.df$stage=="H" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="H" & PropCalc.df2$HorC[i]=="Cold" & PropCalc.df2$Location[i]=="North")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="North" & proportion.df$stage=="H" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="H" & PropCalc.df2$HorC[i]=="Cold" & PropCalc.df2$Location[i]=="South")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="South" & proportion.df$stage=="H" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  
  else if(PropCalc.df2$Stage[i]=="H+" & PropCalc.df2$HorC[i]=="Hot" & PropCalc.df2$Location[i]=="North")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="North" & proportion.df$stage=="H+" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="H+" & PropCalc.df2$HorC[i]=="Hot" & PropCalc.df2$Location[i]=="South")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="South" & proportion.df$stage=="H+" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="H+" & PropCalc.df2$HorC[i]=="Cold" & PropCalc.df2$Location[i]=="North")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="North" & proportion.df$stage=="H+" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="H+" & PropCalc.df2$HorC[i]=="Cold" & PropCalc.df2$Location[i]=="South")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="South" & proportion.df$stage=="H+" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  
  else if(PropCalc.df2$Stage[i]=="I" & PropCalc.df2$HorC[i]=="Hot" & PropCalc.df2$Location[i]=="North")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="North" & proportion.df$stage=="I" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="I" & PropCalc.df2$HorC[i]=="Hot" & PropCalc.df2$Location[i]=="South")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="South" & proportion.df$stage=="I" & proportion.df$HorC_S=="Hot" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="I" & PropCalc.df2$HorC[i]=="Cold" & PropCalc.df2$Location[i]=="North")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="North" & proportion.df$stage=="I" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
  else if(PropCalc.df2$Stage[i]=="I" & PropCalc.df2$HorC[i]=="Cold" & PropCalc.df2$Location[i]=="South")
  {PropCalc.df2$Prop[i] = nrow(proportion.df[which(proportion.df$Location=="South" & proportion.df$stage=="I" & proportion.df$HorC_S=="Cold" & as.character(proportion.df$Bin)==as.character(PropCalc.df2$Bin[i])),])/
    nrow(proportion.df[which(as.character(proportion.df$Bin)==PropCalc.df2$Bin[i]),])}
}
  
  

#WORK IN PROGRESS

#++++++++++++++++++++++ Not Separated by Season +++++++++++++++++++++++++
jdHorCPlot <- ggplot(PropCalc.df,aes(x=Bin,y=Prop,color=factor(Stage,levels = c("G","H-","H","H+","I")),group=1))+
  geom_line()+
  geom_point()+
  facet_grid(HorC ~ factor(Stage,levels = c("G","H-","H","H+","I")))+
  labs(color="Stage")+
  ylab("Proportion")+
  xlab("Julian Day Bin")+
  scale_x_discrete(breaks = c("[0,16)","[32,48)","[64,80)"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.7,size = 13),
        axis.text.y = element_text(size=13),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=13))
  
#$lwd <- 1.422638
grob <- ggplotGrob(jdHorCPlot)
print(grob)
grob$grobs[[31]]$grobs[[1]]$children[[1]]$gp$fill <- "steelblue3"
grob$grobs[[32]]$grobs[[1]]$children[[1]]$gp$fill <- "tomato"
grob$grobs[[32]]$grobs[[1]]$children[[2]]$children[[1]]$gp$font

#grob$grobs[[2]]$children[[4]]$gp$col[4] <- "#F8766DFF"
jpeg(file="~/Desktop/Analysis/AFS/jdHorCPlot.jpeg", width = 10, height = 7.5, res = 300, units = "in")
grid.draw(grob)
dev.off()
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#++++++++++++++++++++++++++ Separated by Season +++++++++++++++++++++++++
jdHorCPlot2 <- ggplot(PropCalc.df2,aes(x=Bin,y=Prop,color=factor(Stage,levels = c("G","H-","H","H+","I")),group=1))+
  geom_line()+
  geom_point()+
  facet_grid(Location+HorC ~ factor(Stage,levels = c("G","H-","H","H+","I")))+
  labs(color="Stage",title="Julian Day Hot vs. Cold Ingress")+
  ylab("Proportion")+
  xlab("Julian Day Bin")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.7,size = 7))

jdHorCPlot3 <- ggplot(PropCalc.df2,aes(x=factor(Stage,levels = c("G","H-","H","H+","I")),y=Prop,color=Bin,group=1))+
  geom_line()+
  geom_point()+
  facet_grid(Location+HorC ~ Bin)+
  labs(color="Bin",title="Julian Day Hot vs. Cold Ingress")+
  ylab("Proportion")+
  xlab("Julian Day Bin")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.7,size = 7))


grob2 <- ggplotGrob(jdHorCPlot2)
print(grob2)
grob2$grobs[[45]]

# Find the location of the strips in the main plot
locations <- grep("strip-r", grob2$layout$name)
# Filter out the strips (trim = FALSE is important here for positions relative to the main plot)
strip <- gtable_filter(grob2, "strip-r", trim = FALSE)
stript <- gtable_filter(grob2, "strip-t", trim = FALSE)
# Gathering our positions for the main plot
top <- strip$layout$t[c(1,3)]
bot <- strip$layout$b[c(2,4)]
r   <- strip$layout$r[1]

mat   <- matrix(vector("list", length = 6), nrow = 2)
mat[] <- list(zeroGrob())
# The separator for the facets has zero width
res <- gtable_matrix("rightcol", mat, unit(c(1, 0, 1), "null"), unit(c(1, 1), "null"))

# Adding the first layer
zz <- res %>%
  gtable_add_grob(grob2$grobs[[locations[1]]]$grobs[[1]], 1, 1, 1, 3) %>%
  gtable_add_grob(grob2, ., t = top[1],  l = r,  b = bot[1],  r = r, name = c("add-strip"))
# Adding the second layer (note the indices)
pp <- gtable_add_grob(res, grob2$grobs[[locations[3]]]$grobs[[1]], 1, 1, 1, 3) %>%
  gtable_add_grob(zz, ., t = top[2],  l = r,  b = bot[2],  r = r, name = c("add-strip"))
# Plotting
grid.newpage()
print(grid.draw(pp))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





# jpeg(file="~/Desktop/Analysis/TemperatureAnalysis/Plots/jdHorCPlot.jpeg", width = 10, height = 7.5, res = 300, units = "in")
# grid.draw(grob)
# dev.off()


#===================================================
# Averaging temperatures by JD Bins
#===================================================

for(i in 1:nrow(dailyTemps)){
  if (as.numeric(dailyTemps$month[i]) >= 1 && as.numeric(dailyTemps$month[i]) <= 6)
  {dailyTemps$Season[i] <- 'Winter'} #makes day 1 for fall October first (may be a day off for leap years)
  else if(as.numeric(dailyTemps$month[i]) >= 10)
  {dailyTemps$Season[i] <- 'Fall'}
  else
  {dailyTemps$Season[i] <- NA}
}

dailyTemps <- transform(dailyTemps, jd = as.Date(dailyTemps$Date, format = "%d%b%y"))
dailyTemps$jd <- format(dailyTemps$jd,"%j")
dailyTemps$jd <- as.numeric(dailyTemps$jd)

for(i in 1:nrow(dailyTemps)){
  if (!is.na(dailyTemps$Season[i]) && dailyTemps$Season[i] == 'Fall')
  {dailyTemps$jd_adj[i] <- dailyTemps$jd[i]  - 273} #makes day 1 for fall October first (may be a day off for leap years)
  else if (!is.na(dailyTemps$Season[i]) && dailyTemps$Season[i] == 'Winter') 
  {dailyTemps$jd_adj[i] <- dailyTemps$jd[i]}
  else 
  {dailyTemps$jd_adj[i] <- NA}}

dailyTempBins <- cut(dailyTemps[dailyTemps$jd_adj < 96,]$jd_adj, jd_breaks, include.lowest = T, right = FALSE,labels = jdFallLabels)
dailyTempCol <- cbind(dailyTemps[dailyTemps$jd_adj < 96,],dailyTempBins)
#dailyTempCol <- jd_adjCol[,c(2,5,7,11,12,16,17,18)]


meltTest <- melt(dailyTempCol,id='year')

grid.arrange(
ggplot()+
  stat_summary(data = dailyTempCol[dailyTempCol$Season=="Winter",],aes(x=dailyTempBins,y=north,color=year,group=year),fun.y = "mean",geom = "line",linetype = "longdash")+
  stat_summary(data = dailyTempCol[dailyTempCol$Season=="Fall",],aes(x=dailyTempBins,y=north,color=year,group = year),fun.y = "mean",geom = "line")+
  scale_x_discrete(limits = c("[0,16)","[16,32)","[32,48)","[48,64)","[64,80)","[80,96)"))+
  scale_y_continuous(limits = c(0,30)),

ggplot()+
  stat_summary(data = dailyTempCol[dailyTempCol$Season=="Winter",],aes(x=dailyTempBins,y=south,color=year,group=year),fun.y = "mean",geom = "line",linetype = "longdash")+
  stat_summary(data = dailyTempCol[dailyTempCol$Season=="Fall",],aes(x=dailyTempBins,y=south,color=year,group = year),fun.y = "mean",geom = "line")+
  scale_x_discrete(limits = c("[0,16)","[16,32)","[32,48)","[48,64)","[64,80)","[80,96)"))+
  scale_y_continuous(limits = c(0,30))
)
  
 
 

#------------------------REGRESSION-----------------------------------------------------------------------
#size of a given stage ~ temperature + julian + site + temperature*site + julian*site + year

#making column for site specific temp
for(i in 1:nrow(updatedallSL)){
  if(updatedallSL$Location[i] == "South")
  {updatedallSL$specificTemp[i] = updatedallSL$southAVG[i]}
else if(updatedallSL$Location[i] == "North")
  {updatedallSL$specificTemp[i] = updatedallSL$northAVG[i]}
}

#have to re-adjust jd to october - april (In dailyTemps dataframe)
k <- 92
for(i in 1:nrow(dailyTemps)){
  
 if(as.numeric(dailyTemps$month[i]) == 10 & as.numeric(dailyTemps$day[i]) == 1)
 {dailyTemps$jd_cont[i] = 1
  k <- 2}
  
  else 
  {dailyTemps$jd_cont[i] = k
   k <- k+1}
}

#now change updatedallsl data frame
updatedallSL <- subset(updatedallSL, select = -c(jd))
for(i in 1:nrow(updatedallSL))
  {updatedallSL$jd[i] = dailyTemps[which(dailyTemps$Date == updatedallSL$Date[i]),]$jd_cont}

sizeResponse1 <- lm(formula = AverageSL ~ specificTemp + jd + Location + specificTemp*Location + jd*Location + Year, 
                   data = updatedallSL[which(updatedallSL$stage=="G"),])
sizeResponse2 <- lm(formula = AverageSL ~ specificTemp + jd + Location + specificTemp*Location + jd*Location + Year, 
                    data = updatedallSL[which(updatedallSL$stage=="H-"),])
sizeResponse3 <- lm(formula = AverageSL ~ specificTemp + jd + Location + specificTemp*Location + jd*Location + Year, 
                    data = updatedallSL[which(updatedallSL$stage=="H"),])
sizeResponse4 <- lm(formula = AverageSL ~ specificTemp + jd + Location + specificTemp*Location + jd*Location + Year, 
                    data = updatedallSL[which(updatedallSL$stage=="H+"),])
sizeResponse5 <- lm(formula = AverageSL ~ specificTemp + jd + Location + specificTemp*Location + jd*Location + Year, 
                    data = updatedallSL[which(updatedallSL$stage=="I"),])


# Looks like it is using "North" as the baseline and comparing "south" to it

#these mess around with location (see notes for results)
size.simple1 <- lm(formula = AverageSL ~ specificTemp + jd + Year, 
                   data = updatedallSL[updatedallSL$stage=="G",]) 
size.simpleLocation <- lm(formula = AverageSL ~ specificTemp + jd + Location + Year, 
   data = updatedallSL[updatedallSL$stage=="G",])

#trying to see if I can add stage in as a predictor
#trends look ok (higher stage = larger size (except for h and h-))

# Center all continuous data
updatedallSL$specificTemp.c <- scale(updatedallSL$specificTemp,center = T,scale = F)
updatedallSL$AverageSL.c <- scale(updatedallSL$AverageSL,center = T,scale = F)
updatedallSL$jd.c <- scale(updatedallSL$jd,center = T,scale = F)

test <- lm(AverageSL.c ~ specificTemp.c + jd.c + Location + stage_f + specificTemp.c:Location + jd.c:Location + Year + Year:Location, 
   data = updatedallSL)

test2 <- gls(AverageSL.c ~ specificTemp.c + jd.c + Location + stage_f + specificTemp.c*Location + Year, 
             data = updatedallSL)

test3 <- gls(AverageSL.c ~ specificTemp.c + jd.c + Location + specificTemp.c*Location + Year, 
            data = updatedallSL)

test4 <- gls(AverageSL.c ~ specificTemp.c + jd.c + Location + specificTemp.c*Location, 
   data = updatedallSL)

#Probably going to roll with this one
test5 <- gls(AverageSL ~ specificTemp.c + jd.c + Year + Year:specificTemp.c, 
            data = updatedallSL)
require(fmsb)
library(Rcpp)

# Eh might not be smart to do this
# test6 <- lm(AverageSL.c ~ specificTemp.c + jd.c + Year + Year:specificTemp.c, 
#             data = updatedallSL[updatedallSL$Location=='North',])
# 
# test7 <- lm(AverageSL.c ~ jd.c + specificTemp.c + Year + Year:specificTemp.c + jd.c:specificTemp.c, 
#             data = updatedallSL[updatedallSL$Location=='North',])





#Staging model

stageModel <- multinom(stage_f ~ AverageSL + seasonalAnomaly + specificTemp.c + jd.c + Location + specificTemp.c:Location + jd.c:Location + Year, data = updatedallSL)
stageModel2 <- multinom(stage_f ~ jd.c + seasonalAnomaly, data = updatedallSL)
stageModel3 <- multinom(stage_f ~ jd.c, data = updatedallSL)
z <- summary(stageModel2)$coefficients/summary(stageModel3)$standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# stageModel <- polr(formula = stage_f ~ specificTemp + jd + Location + Year, data = updatedallSL)
# ctable <- coef(summary(stageModel))
# p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# ctable <- cbind(ctable, "p value" = p)
# ci <- confint(stageModel)
# exp(cbind(OR = coef(stageModel), ci))




