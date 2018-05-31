setwd("~/Desktop/Analysis/with 50 extras")
larvs <- read.csv('Larval PADE datasheet - Staging Data.csv') #reads in data
dim(larvs) #dimensions
class(larvs)
str(larvs)
#larvs$ = use to call columns
#larvs[1,n] use to call rows
#[row,column]....[row:row,column]
head(larvs) #default, first 6 rows
tail(larvs,20) #last 20 rows
names(larvs) #column names
colnames(larvs)
ncol(larvs) #number of columns
hist(larvs$Average.SL..mm.,breaks = 10)
plot(larvs$Average.SL..mm.~larvs$Date.Caught)
plot(larvs$Standard.Length..SL..of.Back..mm.~larvs$Standard.Length..SL..of.Front..mm.)
which(larvs$Date.Caught=="2/15/1989")
larvs[c(3,4,5,6,7),3]
larvs[,3]
larvs[order(as.Date(larvs$Date.Caught, format="%m/%d/%y")),]

#boxplot(larvs$Average.SL..mm.[1:46],larvs$Average.SL..mm.[47:100], names = c("north larvae", "south larvae"))
#title("Boxplots of 1989-1993 North and South Larve")

#boxplot(larvs$Average.SL..mm.[1:50],larvs$Average.SL..mm.[51:104], names = c("north larvae", "south larvae"))
#title("Boxplots of 1998-2003 North and South Larve")

#boxplot(larvs$Average.SL..mm.[1:56],larvs$Average.SL..mm.[57:173], names = c("north larvae", "south larvae"))
#title("Boxplots of 2008-2012 North and South Larve")
####Not sure if plots show actual data intended or if the numbers (i.e. [1:46]) are from the original data set
####If they are from original data set then I am just showing same data in different amounts


###################################################################################################################


#DATA AND PLOTS

#1989-1993 larvae
early <- which(larvs$Year==1989)
early2 <- which(larvs$Year==1990)
early3 <- which(larvs$Year==1991)
early4 <- which(larvs$Year==1992)
early5 <- which(larvs$Year==1993)
early.all <- c(early,early2,early3,early4,early5)
earlydata <- larvs[early.all,]
earlyn <- earlydata[which(earlydata$North.or.South=='n'),]
earlys <- earlydata[which(earlydata$North.or.South=='s'),]

#earlydata[order(as.Date(earlydata$Date.Caught, format="%m/%d/%Y")),]
#boxplot(earlyn$Average.SL..mm., earlys$Average.SL..mm., names = c("north larvae", "south larvae"))
#title("1989-1993")

##1998-2003 larvae
mid <- which(larvs$Year==1998)
mid2<- append(mid, which(larvs$Year==1999)) #links mid into mid2
mid3<- append(mid2, which(larvs$Year==2000))
mid4<- append(mid3, which(larvs$Year==2001))
mid5<- append(mid4, which(larvs$Year==2002))
mid6<- append(mid5, which(larvs$Year==2003))#links all mid data from 1-5 to mid 6
mid.all<- c(mid,mid2,mid3,mid4,mid5,mid6)
middata <-larvs[mid6,] #mid6 becomes middata
midn <- middata[which(middata$North.or.South=='n'),]
mids <- middata[which(middata$North.or.South=='s'),]
#boxplot2 <- boxplot(midn$Average.SL..mm., mids$Average.SL..mm., names = c("north larvae", "south larvae"))
#title("1998-2003")
#middata[order(as.Date(middata$Date.Caught, format="%m/%d/%Y")),]

##2008-2012 larvae
late <- which(larvs$Year==2008)
late2 <- append(late, which(larvs$Year==2009))
late3 <- append(late2, which(larvs$Year==2010))
late4 <- append(late3, which(larvs$Year==2011))
late5 <- append(late4, which(larvs$Year==2012))
late.all<- c(late,late2,late3,late4,late5)
latedata <- larvs[late5,]
laten <- latedata[which(latedata$North.or.South=='n'),]
lates <- latedata[which(latedata$North.or.South=='s'),]
#latedata[order(as.Date(latedata$Date.Caught, format="%m/%d/%y")),]
#boxplot3 <- boxplot(laten$Average.SL..mm., lates$Average.SL..mm., names = c("north larvae", "south larvae"))
#title("2008-2012")


#boxplot(earlyn$Average.SL..mm., earlys$Average.SL..mm., midn$Average.SL..mm., mids$Average.SL..mm., laten$Average.SL..mm., lates$Average.SL..mm., names = c("1989-1993 North","1989-1993 South","1998-2003 North","1998-2003 South","2008-2012 North","2008-2012 South" ), col = c("blue","tomato","blue","tomato","blue","tomato"), ylab="Average Standard Length (mm)")
#title("Northern and Southern Summer Flounder Larvae Standard Lengths over 3 Time Periods")  


a <- earlyn$Average.SL..mm.
b <- earlys$Average.SL..mm.
c <- midn$Average.SL..mm.
d <- mids$Average.SL..mm.
e <- laten$Average.SL..mm.
f <- lates$Average.SL..mm.


#jpeg(file="~/Desktop/Analysis/Standard Length over time", width = 10, height = 7.5, res = 300, units = "in")
#par(
 # mar=c(5, 5, 2, 5), # panel magin size in "line number" units
  #mgp=c(3, 1, 0), # default is c(3,1,0); line number for axis label, tick label, axis
 # tcl=-0.5, # size of tick marks as distance INTO figure (negative means pointing outward)
  #cex=1, # character expansion factor; keep as 1; if you have a many-panel figure, they start changing the default!
  #ps=18) # point size, which is the font size
 

 
#############################VIOLIN PLOTS OF ALL INDIVIDUALS##########################################
# All your plotting code
#plot(6,1,xlim=c(0.5,6.5),ylim=c(5,25),type="n",axes=FALSE,ylab = "Average Standard Length (mm)",xlab = "Time Period Caught")
#legend("topleft",legend = c("North","South"),pch = c(15,15), col = c("steelblue3","tomato"),cex=1.5 )
## bottom axis, with user-specified labels
#axis(side=1,at=c(1.5,3.5,5.5),labels=c("1989-1993","1998-2003","2008-2012"))
#axis(side=2)
 ##trying to change font size, not working
#vioplot(na.omit(a),at=1.15,col="steelblue3",add=TRUE)
#vioplot(a,at=1.15,col="steelblue3",add=TRUE,rectcol="grey3")
#vioplot(b,at=1.85,col="tomato",add=TRUE)
#vioplot(c,at=3.05,col="steelblue3",add=TRUE)
#vioplot(d,at=3.95,col="tomato",add=TRUE)
#vioplot(na.omit(e),at=5.05,col="steelblue3",add=TRUE)
#vioplot(na.omit(f),at=5.95,col="tomato",add=TRUE)

#text(x =1.15, y= 14.25, labels = "a")
#text(x =1.85, y= 15.5, labels = "b")
#text(x =3.05, y= 16.5, labels = "bc")
#text(x =3.95, y= 16.5, labels = "bc")
#text(x =5.05, y= 24, labels = "c")
#text(x =5.95, y= 16.5, labels = "bc")
#dev.off()
#########################################################################################################


#York River analysis
##sampling site further inland than others, individuals expected to be larger

#Take the mean average SL when larvs$location=='York River'
#mean(larvs$Average.SL..mm.[larvs$Location=='York River'])
# = 14.2432 AT YORK RIVER


#name data set for all late northern larvae not in york river
#late_north_notyork <- larvs[which(larvs$Time.Period=='late' & larvs$North.or.South=='n' & larvs$Location!='York River'),]
#summary(late_north_notyork$Average.SL..mm.)                          
#AVG SL= 13.02
                      




##############################Staging Data#################################

#group into stages
#gen<- larvs[which(larvs$Developmental.Stage=='G' & larvs$Time.Period=='early' & larvs$North.or.South=='n'),]

g<- larvs[which(larvs$Developmental.Stage=='G'),]
hminus<- larvs[which(larvs$Developmental.Stage=='H-'),]
h<- larvs[which(larvs$Developmental.Stage=='H'),]
hplus<- larvs[which(larvs$Developmental.Stage=='H+'),]
i<- larvs[which(larvs$Developmental.Stage=='I'),]

#make linear regression lines for each dataset [lm(y~x)]
gfit<- lm(g$Average.SL..mm.~g$Year)
hminusfit<- lm(hminus$Average.SL..mm.~hminus$Year)
hfit<- lm(h$Average.SL..mm.~h$Year)
hplusfit<- lm(hplus$Average.SL..mm.~hplus$Year)
ifit<- lm(i$Average.SL..mm.~i$Year)


#fit a linear regression line: lm(y~x)
#to plot the line type: abline(mod, col='red')
#make y range (8,23) and x range (1989,2012)

#Create a plot with the 5 stages subplotted
par(mfrow=c(3,2),pch=16,col.main='red') #graphical parameters-->3 rows, 2 columns, points are closed circles, titles are red
plot(g$Average.SL..mm.~g$Year,ylim=c(8,23),xlim=c(1989,2012),xlab='Year Caught',ylab = 'Average Standard Length (mm)')
text(par('usr')[1]+2,.9*par('usr')[4],labels=paste('slope=',0.07127))
abline(gfit,col='red')
title("G")
plot(hminus$Average.SL..mm.~hminus$Year,ylim=c(8,23),xlim=c(1989,2012),xlab='Year Caught',ylab = 'Average Standard Length (mm)')
text(par('usr')[1]+2,.9*par('usr')[4],labels=paste('slope=',0.04149))
abline(hminusfit,col='red')
title("H-")
plot(h$Average.SL..mm.~h$Year,ylim=c(8,23),xlim=c(1989,2012),xlab='Year Caught',ylab = 'Average Standard Length (mm)')
text(par('usr')[1]+2,.9*par('usr')[4],labels=paste('slope=',0.07373))
abline(hfit,col='red')
title("H")
plot(hplus$Average.SL..mm.~hplus$Year,ylim=c(8,23),xlim=c(1989,2012),xlab='Year Caught',ylab = 'Average Standard Length (mm)')
text(par('usr')[1]+2,.9*par('usr')[4],labels=paste('slope=',0.07254))
abline(hplusfit,col='red')
title("H+")
plot(i$Average.SL..mm.~i$Year,ylim=c(8,23),xlim=c(1989,2012),xlab='Year Caught',ylab = 'Average Standard Length (mm)')
text(par('usr')[1]+2,.9*par('usr')[4],labels=paste('slope=',-0.007525))
abline(ifit,col='red')
title("I")


####remove something from global environment: rm(test, list = character(),envir = globalenv())####


#separate north and south
##north and south stages
gn<- larvs[which(larvs$Developmental.Stage=='G' & larvs$North.or.South=='n'),]
gs<- larvs[which(larvs$Developmental.Stage=='G' & larvs$North.or.South=='s'),]
hminusn<- larvs[which(larvs$Developmental.Stage=='H-' & larvs$North.or.South=='n'),]
hminuss<- larvs[which(larvs$Developmental.Stage=='H-' & larvs$North.or.South=='s'),]
hn<- larvs[which(larvs$Developmental.Stage=='H' & larvs$North.or.South=='n'),]
hs<- larvs[which(larvs$Developmental.Stage=='H' & larvs$North.or.South=='s'),]
hplusn<- larvs[which(larvs$Developmental.Stage=='H+' & larvs$North.or.South=='n'),]
hpluss<- larvs[which(larvs$Developmental.Stage=='H+' & larvs$North.or.South=='s'),]
inorth<- larvs[which(larvs$Developmental.Stage=='I' & larvs$North.or.South=='n'),]
isouth<- larvs[which(larvs$Developmental.Stage=='I' & larvs$North.or.South=='s'),]

#linear regressions for separated data
gnfit<- lm(gn$Average.SL..mm.~gn$Year)
gsfit<- lm(gs$Average.SL..mm.~gs$Year)
hminusnfit<- lm(hminusn$Average.SL..mm.~hminusn$Year)
hminussfit<- lm(hminuss$Average.SL..mm.~hminuss$Year)
hnfit<- lm(hn$Average.SL..mm.~hn$Year)
hsfit<- lm(hs$Average.SL..mm.~hs$Year)
hplusnfit<- lm(hplusn$Average.SL..mm.~hplusn$Year)
hplussfit<- lm(hpluss$Average.SL..mm.~hpluss$Year)
inorthfit<- lm(inorth$Average.SL..mm.~inorth$Year)
isouthfit<- lm(isouth$Average.SL..mm.~isouth$Year)

jpeg(file="~/Desktop/Analysis/linearregression", width = 10, height = 7.5, res = 300, units = "in")
par(mfrow=c(3,2),lab=c(12,6,7),las=2, font.lab=3) #graphical parameters-->3 rows, 2 columns, points are closed circles, titles are red
#G plot
plot(gn$Average.SL..mm.~gn$Year,ylim=c(8,23),xlim=c(1989,2012),xlab='Year Caught',ylab = 'Average Standard Length (mm)',col='steelblue3',pch=16)
points(gs$Average.SL..mm.~gs$Year,pch=16,col='tomato')
abline(gnfit,col='steelblue3')
abline(gsfit,col='tomato')
title("G")
#legend(1,1,legend = c("North","South"),pch = c(15,15), col = c("steelblue3","tomato"))
#H- plot;
plot(hminusn$Average.SL..mm.~hminusn$Year,ylim=c(8,23),xlim=c(1989,2012),xlab='Year Caught',ylab = 'Average Standard Length (mm)',col='steelblue3',pch=16)
points(hminuss$Average.SL..mm.~hminuss$Year,pch=16,col='tomato')
abline(hminusnfit,col='steelblue3')
abline(hminussfit,col='tomato')
title("H-")
#H plot
plot(hn$Average.SL..mm.~hn$Year,ylim=c(8,23),xlim=c(1989,2012),xlab='Year Caught',ylab = 'Average Standard Length (mm)',col='steelblue3',pch=16)
points(hs$Average.SL..mm.~hs$Year,pch=16,col='tomato')
abline(hnfit,col='steelblue3')
abline(hsfit,col='tomato')
title("H")
#H+ plot
plot(hplusn$Average.SL..mm.~hplusn$Year,ylim=c(8,23),xlim=c(1989,2012),xlab='Year Caught',ylab = 'Average Standard Length (mm)',pch=16,col='steelblue3')
points(hpluss$Average.SL..mm.~hpluss$Year,pch=16,col='tomato')
abline(hplusnfit,col='steelblue3')
abline(hplussfit,col='tomato')
title("H+")
#I plot
plot(inorth$Average.SL..mm.~inorth$Year,ylim=c(8,23),xlim=c(1989,2012),xlab='Year Caught',ylab = 'Average Standard Length (mm)',pch=16,col='steelblue3')
points(isouth$Average.SL..mm.~isouth$Year,pch=16,col='tomato')
abline(inorthfit,col='steelblue3')
abline(isouthfit,col='tomato')
title("I")
dev.off()

#
plot.new()
legend("center",legend = c("North","South"),pch = c(15,15), col = c("steelblue3","tomato"),cex = 1.5,bty = "o")
#################################################################################################################################################################

##########FREQUENCY OF STAGES EACH YEAR################
g1989<-nrow(larvs[which(larvs$Year=='1989' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='1989'),])
g1990<-nrow(larvs[which(larvs$Year=='1990' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='1990'),])
g1991<-nrow(larvs[which(larvs$Year=='1991' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='1991'),])
g1992<-nrow(larvs[which(larvs$Year=='1992' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='1992'),])
g1993<-nrow(larvs[which(larvs$Year=='1993' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='1993'),])
g1994<-nrow(larvs[which(larvs$Year=='1994' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='1994'),])
g1995<-nrow(larvs[which(larvs$Year=='1995' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='1995'),])
g1996<-nrow(larvs[which(larvs$Year=='1996' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='1996'),])
g1997<-nrow(larvs[which(larvs$Year=='1997' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='1997'),])
g1998<-nrow(larvs[which(larvs$Year=='1998' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='1998'),])
g1999<-nrow(larvs[which(larvs$Year=='1999' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='1999'),])
g2000<-nrow(larvs[which(larvs$Year=='2000' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='2000'),])
g2001<-nrow(larvs[which(larvs$Year=='2001' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='2001'),])
g2002<-nrow(larvs[which(larvs$Year=='2002' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='2002'),])
g2003<-nrow(larvs[which(larvs$Year=='2003' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='2003'),])
g2004<-nrow(larvs[which(larvs$Year=='2004' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='2004'),])
g2005<-nrow(larvs[which(larvs$Year=='2005' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='2005'),])
g2006<-nrow(larvs[which(larvs$Year=='2006' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='2006'),])
g2007<-nrow(larvs[which(larvs$Year=='2007' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='2007'),])
g2008<-nrow(larvs[which(larvs$Year=='2008' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='2008'),])
g2009<-nrow(larvs[which(larvs$Year=='2009' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='2009'),])
g2010<-nrow(larvs[which(larvs$Year=='2010' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='2010'),])
g2011<-nrow(larvs[which(larvs$Year=='2011' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='2011'),])
g2012<-nrow(larvs[which(larvs$Year=='2012' & larvs$Developmental.Stage=='G'),])#/nrow(larvs[which(larvs$Year=='2012'),])
allthegs<- c(g1989,g1990,g1991,g1992,g1993,g1994,g1995,g1996,g1997,g1998,g1999,g2000,g2001,g2002,g2003,g2004,g2005,g2006,g2007,g2008,g2009,g2010,g2011,g2012)
allthegs[is.na(allthegs)]<-0

hminus1989<-nrow(larvs[which(larvs$Year=='1989' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='1989'),])
hminus1990<-nrow(larvs[which(larvs$Year=='1990' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='1990'),])
hminus1991<-nrow(larvs[which(larvs$Year=='1991' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='1991'),])
hminus1992<-nrow(larvs[which(larvs$Year=='1992' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='1992'),])
hminus1993<-nrow(larvs[which(larvs$Year=='1993' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='1993'),])
hminus1994<-nrow(larvs[which(larvs$Year=='1994' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='1994'),])
hminus1995<-nrow(larvs[which(larvs$Year=='1995' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='1995'),])
hminus1996<-nrow(larvs[which(larvs$Year=='1996' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='1996'),])
hminus1997<-nrow(larvs[which(larvs$Year=='1997' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='1997'),])
hminus1998<-nrow(larvs[which(larvs$Year=='1998' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='1998'),])
hminus1999<-nrow(larvs[which(larvs$Year=='1999' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='1999'),])
hminus2000<-nrow(larvs[which(larvs$Year=='2000' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='2000'),])
hminus2001<-nrow(larvs[which(larvs$Year=='2001' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='2001'),])
hminus2002<-nrow(larvs[which(larvs$Year=='2002' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='2002'),])
hminus2003<-nrow(larvs[which(larvs$Year=='2003' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='2003'),])
hminus2004<-nrow(larvs[which(larvs$Year=='2004' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='2004'),])
hminus2005<-nrow(larvs[which(larvs$Year=='2005' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='2005'),])
hminus2006<-nrow(larvs[which(larvs$Year=='2006' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='2006'),])
hminus2007<-nrow(larvs[which(larvs$Year=='2007' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='2007'),])
hminus2008<-nrow(larvs[which(larvs$Year=='2008' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='2008'),])
hminus2009<-nrow(larvs[which(larvs$Year=='2009' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='2009'),])
hminus2010<-nrow(larvs[which(larvs$Year=='2010' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='2010'),])
hminus2011<-nrow(larvs[which(larvs$Year=='2011' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='2011'),])
hminus2012<-nrow(larvs[which(larvs$Year=='2012' & larvs$Developmental.Stage=='H-'),])#/nrow(larvs[which(larvs$Year=='2012'),])
allthehminuss<- c(hminus1989,hminus1990,hminus1991,hminus1992,hminus1993,hminus1994,hminus1995,hminus1996,hminus1997,hminus1998,hminus1999,hminus2000,hminus2001,hminus2002,hminus2003,hminus2004,hminus2005,hminus2006,hminus2007,hminus2008,hminus2009,hminus2010,hminus2011,hminus2012)
allthehminuss[is.na(allthehminuss)]<-0

h1989<-nrow(larvs[which(larvs$Year=='1989' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='1989'),])
h1990<-nrow(larvs[which(larvs$Year=='1990' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='1990'),])
h1991<-nrow(larvs[which(larvs$Year=='1991' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='1991'),])
h1992<-nrow(larvs[which(larvs$Year=='1992' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='1992'),])
h1993<-nrow(larvs[which(larvs$Year=='1993' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='1993'),])
h1994<-nrow(larvs[which(larvs$Year=='1994' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='1994'),])
h1995<-nrow(larvs[which(larvs$Year=='1995' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='1995'),])
h1996<-nrow(larvs[which(larvs$Year=='1996' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='1996'),])
h1997<-nrow(larvs[which(larvs$Year=='1997' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='1997'),])
h1998<-nrow(larvs[which(larvs$Year=='1998' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='1998'),])
h1999<-nrow(larvs[which(larvs$Year=='1999' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='1999'),])
h2000<-nrow(larvs[which(larvs$Year=='2000' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='2000'),])
h2001<-nrow(larvs[which(larvs$Year=='2001' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='2001'),])
h2002<-nrow(larvs[which(larvs$Year=='2002' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='2002'),])
h2003<-nrow(larvs[which(larvs$Year=='2003' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='2003'),])
h2004<-nrow(larvs[which(larvs$Year=='2004' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='2004'),])
h2005<-nrow(larvs[which(larvs$Year=='2005' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='2005'),])
h2006<-nrow(larvs[which(larvs$Year=='2006' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='2006'),])
h2007<-nrow(larvs[which(larvs$Year=='2007' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='2007'),])
h2008<-nrow(larvs[which(larvs$Year=='2008' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='2008'),])
h2009<-nrow(larvs[which(larvs$Year=='2009' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='2009'),])
h2010<-nrow(larvs[which(larvs$Year=='2010' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='2010'),])
h2011<-nrow(larvs[which(larvs$Year=='2011' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='2011'),])
h2012<-nrow(larvs[which(larvs$Year=='2012' & larvs$Developmental.Stage=='H'),])#/nrow(larvs[which(larvs$Year=='2012'),])
allthehs<- c(h1989,h1990,h1991,h1992,h1993,h1994,h1995,h1996,h1997,h1998,h1999,h2000,h2001,h2002,h2003,h2004,h2005,h2006,h2007,h2008,h2009,h2010,h2011,h2012)
allthehs[is.na(allthehs)]<-0

hplus1989<-nrow(larvs[which(larvs$Year=='1989' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='1989'),])
hplus1990<-nrow(larvs[which(larvs$Year=='1990' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='1990'),])
hplus1991<-nrow(larvs[which(larvs$Year=='1991' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='1991'),])
hplus1992<-nrow(larvs[which(larvs$Year=='1992' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='1992'),])
hplus1993<-nrow(larvs[which(larvs$Year=='1993' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='1993'),])
hplus1994<-nrow(larvs[which(larvs$Year=='1994' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='1994'),])
hplus1995<-nrow(larvs[which(larvs$Year=='1995' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='1995'),])
hplus1996<-nrow(larvs[which(larvs$Year=='1996' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='1996'),])
hplus1997<-nrow(larvs[which(larvs$Year=='1997' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='1997'),])
hplus1998<-nrow(larvs[which(larvs$Year=='1998' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='1998'),])
hplus1999<-nrow(larvs[which(larvs$Year=='1999' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='1999'),])
hplus2000<-nrow(larvs[which(larvs$Year=='2000' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='2000'),])
hplus2001<-nrow(larvs[which(larvs$Year=='2001' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='2001'),])
hplus2002<-nrow(larvs[which(larvs$Year=='2002' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='2002'),])
hplus2003<-nrow(larvs[which(larvs$Year=='2003' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='2003'),])
hplus2004<-nrow(larvs[which(larvs$Year=='2004' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='2004'),])
hplus2005<-nrow(larvs[which(larvs$Year=='2005' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='2005'),])
hplus2006<-nrow(larvs[which(larvs$Year=='2006' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='2006'),])
hplus2007<-nrow(larvs[which(larvs$Year=='2007' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='2007'),])
hplus2008<-nrow(larvs[which(larvs$Year=='2008' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='2008'),])
hplus2009<-nrow(larvs[which(larvs$Year=='2009' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='2009'),])
hplus2010<-nrow(larvs[which(larvs$Year=='2010' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='2010'),])
hplus2011<-nrow(larvs[which(larvs$Year=='2011' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='2011'),])
hplus2012<-nrow(larvs[which(larvs$Year=='2012' & larvs$Developmental.Stage=='H+'),])#/nrow(larvs[which(larvs$Year=='2012'),])
allthehpluss<- c(hplus1989,hplus1990,hplus1991,hplus1992,hplus1993,hplus1994,hplus1995,hplus1996,hplus1997,hplus1998,hplus1999,hplus2000,hplus2001,hplus2002,hplus2003,hplus2004,hplus2005,hplus2006,hplus2007,hplus2008,hplus2009,hplus2010,hplus2011,hplus2012)
allthehpluss[is.na(allthehpluss)]<-0

i1989<-nrow(larvs[which(larvs$Year=='1989' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='1989'),])
i1990<-nrow(larvs[which(larvs$Year=='1990' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='1990'),])
i1991<-nrow(larvs[which(larvs$Year=='1991' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='1991'),])
i1992<-nrow(larvs[which(larvs$Year=='1992' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='1992'),])
i1993<-nrow(larvs[which(larvs$Year=='1993' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='1993'),])
i1994<-nrow(larvs[which(larvs$Year=='1994' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='1994'),])
i1995<-nrow(larvs[which(larvs$Year=='1995' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='1995'),])
i1996<-nrow(larvs[which(larvs$Year=='1996' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='1996'),])
i1997<-nrow(larvs[which(larvs$Year=='1997' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='1997'),])
i1998<-nrow(larvs[which(larvs$Year=='1998' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='1998'),])
i1999<-nrow(larvs[which(larvs$Year=='1999' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='1999'),])
i2000<-nrow(larvs[which(larvs$Year=='2000' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='2000'),])
i2001<-nrow(larvs[which(larvs$Year=='2001' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='2001'),])
i2002<-nrow(larvs[which(larvs$Year=='2002' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='2002'),])
i2003<-nrow(larvs[which(larvs$Year=='2003' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='2003'),])
i2004<-nrow(larvs[which(larvs$Year=='2004' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='2004'),])
i2005<-nrow(larvs[which(larvs$Year=='2005' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='2005'),])
i2006<-nrow(larvs[which(larvs$Year=='2006' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='2006'),])
i2007<-nrow(larvs[which(larvs$Year=='2007' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='2007'),])
i2008<-nrow(larvs[which(larvs$Year=='2008' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='2008'),])
i2009<-nrow(larvs[which(larvs$Year=='2009' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='2009'),])
i2010<-nrow(larvs[which(larvs$Year=='2010' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='2010'),])
i2011<-nrow(larvs[which(larvs$Year=='2011' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='2011'),])
i2012<-nrow(larvs[which(larvs$Year=='2012' & larvs$Developmental.Stage=='I'),])#/nrow(larvs[which(larvs$Year=='2012'),])
alltheis<- c(i1989,i1990,i1991,i1992,i1993,i1994,i1995,i1996,i1997,i1998,i1999,i2000,i2001,i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012)
alltheis[is.na(alltheis)]<-0


#g.df<- data.frame(c(g1989,g1990,g1991,g1992,g1993,g1994,g1995,g1996,g1997,g1998,g1999,g2000,g2001,g2002,g2003,g2004,g2005,g2006,g2007,g2008,g2009,g2010,g2011,g2012))
#g.df[is.na(d)] <- 0
years<- c(1989:2012)
stages<-c("G","H-","H","H+","I")
stages.df<- data.frame(stages)


#create variable containing all individuals for each year
count1989<- c(g1989,hminus1989,h1989,hplus1989,i1989)
count1990<- c(g1990,hminus1990,h1990,hplus1990,i1990)
count1991<- c(g1991,hminus1991,h1991,hplus1991,i1991)
count1992<- c(g1992,hminus1992,h1992,hplus1992,i1992)
count1993<- c(g1993,hminus1993,h1993,hplus1993,i1993)
count1994<- c(g1994,hminus1994,h1994,hplus1994,i1994)
count1995<- c(g1995,hminus1995,h1995,hplus1995,i1995)
count1996<- c(g1996,hminus1996,h1996,hplus1996,i1996)
count1997<- c(g1997,hminus1997,h1997,hplus1997,i1997)
count1998<- c(g1998,hminus1998,h1998,hplus1998,i1998)
count1999<- c(g1999,hminus1999,h1999,hplus1999,i1999)
count2000<- c(g2000,hminus2000,h2000,hplus2000,i2000)
count2001<- c(g2001,hminus2001,h2001,hplus2001,i2001)
count2002<- c(g2002,hminus2002,h2002,hplus2002,i2002)
count2003<- c(g2003,hminus2003,h2003,hplus2003,i2003)
count2004<- c(g2004,hminus2004,h2004,hplus2004,i2004)
count2005<- c(g2005,hminus2005,h2005,hplus2005,i2005)
count2006<- c(g2006,hminus2006,h2006,hplus2006,i2006)
count2007<- c(g2007,hminus2007,h2007,hplus2007,i2007)
count2008<- c(g2008,hminus2008,h2008,hplus2008,i2008)
count2009<- c(g2009,hminus2009,h2009,hplus2009,i2009)
count2010<- c(g2010,hminus2010,h2010,hplus2010,i2010)
count2011<- c(g2011,hminus2011,h2011,hplus2011,i2011)
count2012<- c(g2012,hminus2012,h2012,hplus2012,i2012)
#years.df<- data.frame(years)
#plot(larvs$Year,c(g1989,g1990,g1991,g1992,g1993,g1998,g1999,g2000,g2001,g2002,g2006,g2008,g2009,g2010,g2011,g2012))

counts<- c(count1989,count1990,count1991,count1992,count1993,count1994,count1995,count1996,count1997,count1998,count1999,count2000,count2001,count2002,count2003,count2004,count2005,count2006,count2007,count2008,count2009,count2010,count2011,count2012)
counts.df<- data.frame(c(counts,years,stages)) #for ggplot
#plot(years,allthegs) ##looks pretty shady right now
#individuals: min=6, max=63
#plot(stages,count1989)
#axis.labels <- c("G", "H-", "H", "H+","I")
#axis(1,1:5,LETTERS[1:5], pos=0, las=0)
#ggplot(larvs.df, aes(stages, count1989)) +  geom_bar(stat = 'identity')

#split years into 2 groups and then combined pdfs
##plots of each stage for every years (years with nothing have no individuals)
par(mfrow=c(2,6))
barplot(count1989,names.arg = stages,main = "1989 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count1990,names.arg = stages,main = "1990 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count1991,names.arg = stages,main = "1991 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count1992,names.arg = stages,main = "1992 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count1993,names.arg = stages,main = "1993 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count1994,names.arg = stages,main = "1994 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count1995,names.arg = stages,main = "1995 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count1996,names.arg = stages,main = "1996 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count1997,names.arg = stages,main = "1997 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count1998,names.arg = stages,main = "1998 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count1999,names.arg = stages,main = "1999 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count2000,names.arg = stages,main = "2000 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))

par(mfrow=c(2,6))
barplot(count2001,names.arg = stages,main = "2001 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count2002,names.arg = stages,main = "2002 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count2003,names.arg = stages,main = "2003 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count2004,names.arg = stages,main = "2004 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count2005,names.arg = stages,main = "2005 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count2006,names.arg = stages,main = "2006 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count2007,names.arg = stages,main = "2007 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count2008,names.arg = stages,main = "2008 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count2009,names.arg = stages,main = "2009 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count2010,names.arg = stages,main = "2010 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count2011,names.arg = stages,main = "2011 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
barplot(count2012,names.arg = stages,main = "2012 Counts",xlab = "Stage",ylab = "Count",ylim = c(0,25))
####

#create categories for each stage containing individuals from each year
gcounts<- c(g1989,g1990,g1991,g1992,g1993,g1994,g1995,g1996,g1997,g1998,g1999,g2000,g2001,g2002,g2003,g2004,g2005,g2006,g2007,g2008,g2009,g2010,g2011,g2012)
hminuscounts<- c(hminus1989,hminus1990,hminus1991,hminus1992,hminus1993,hminus1994,hminus1995,hminus1996,hminus1997,hminus1998,hminus1999,hminus2000,hminus2001,hminus2002,hminus2003,hminus2004,hminus2005,hminus2006,hminus2007,hminus2008,hminus2009,hminus2010,hminus2011,hminus2012)
hcounts<- c(h1989,h1990,h1991,h1992,h1993,h1994,h1995,h1996,h1997,h1998,h1999,h2000,h2001,h2002,h2003,h2004,h2005,h2006,h2007,h2008,h2009,h2010,h2011,h2012)
hpluscounts<- c(hplus1989,hplus1990,hplus1991,hplus1992,hplus1993,hplus1994,hplus1995,hplus1996,hplus1997,hplus1998,hplus1999,hplus2000,hplus2001,hplus2002,hplus2003,hplus2004,hplus2005,hplus2006,hplus2007,hplus2008,hplus2009,hplus2010,hplus2011,hplus2012)
icounts<- c(i1989,i1990,i1991,i1992,i1993,i1994,i1995,i1996,i1997,i1998,i1999,i2000,i2001,i2002,i2003,i2004,i2005,i2006,i2007,i2008,i2009,i2010,i2011,i2012)
##make single plot for each stage and plot the year counts
par(mfrow=c(3,2),mar=c(5,5,5,5),las=2,lab=c(12,6,5),font.lab=2)
barplot(gcounts,names.arg = years,main = "G",xlab = "Year",ylab = "Count",ylim = c(0,25),density=100,angle = 45)
barplot(hminuscounts,names.arg = years,main = "H-",xlab = "Year",ylab = "Count",ylim = c(0,25),density=100,angle = 45)
barplot(hcounts,names.arg = years,main = "H",xlab = "Year",ylab = "Count",ylim = c(0,25),density=100,angle = 45)
barplot(hpluscounts,names.arg = years,main = "H+",xlab = "Year",ylab = "Count",ylim = c(0,25),density=100,angle = 45)
barplot(icounts,names.arg = years,main = "I",xlab = "Year",ylab = "Count",ylim = c(0,25),density=100,angle = 45)
###


#group years into early, mid, and late
gcountsearly<- (g1989+g1990+g1991+g1992+g1993)
gcountsmid<- (g1998+g1999+g2000+g2001+g2002+g2003)
gcountslate<- c(g2008+g2009+g2010+g2011+g2012)
totalgcounts<- (gcountsearly+gcountsmid+gcountslate)
allgcounts<- c(gcountsearly,gcountsmid,gcountslate)

hminuscountsearly<- (hminus1989+hminus1990+hminus1991+hminus1992+hminus1993)
hminuscountsmid<- (hminus1998+hminus1999+hminus2000+hminus2001+hminus2002+hminus2003)
hminuscountslate<- c(hminus2008+hminus2009+hminus2010+hminus2011+hminus2012)
totalhminuscounts<- (hminuscountsearly+hminuscountsmid+hminuscountslate)
allhminuscounts<- c(hminuscountsearly,hminuscountsmid,hminuscountslate)

hcountsearly<- (h1989+h1990+h1991+h1992+h1993)
hcountsmid<- (h1998+h1999+h2000+h2001+h2002+h2003)
hcountslate<- c(h2008+h2009+h2010+h2011+h2012)
totalhcounts<- (hcountsearly+hcountsmid+hcountslate)
allhcounts<- c(hcountsearly,hcountsmid,hcountslate)

hpluscountsearly<- (hplus1989+hplus1990+hplus1991+hplus1992+hplus1993)
hpluscountsmid<- (hplus1998+hplus1999+hplus2000+hplus2001+hplus2002+hplus2003)
hpluscountslate<- c(hplus2008+hplus2009+hplus2010+hplus2011+hplus2012)
totalhpluscounts<- (hpluscountsearly+hpluscountsmid+hpluscountslate)
allhpluscounts<- c(hpluscountsearly,hpluscountsmid,hpluscountslate)

icountsearly<- (i1989+i1990+i1991+i1992+i1993)
icountsmid<- (i1998+i1999+i2000+i2001+i2002+i2003)
icountslate<- c(i2008+i2009+i2010+i2011+i2012)
totalicounts<- (icountsearly+icountsmid+icountslate)
allicounts<- c(icountsearly,icountsmid,icountslate)

par(mfrow=c(3,2),font.lab=2,cex.main=1.5)
barplot(allgcounts,names.arg = c("Early","Mid","Late"),xlab = "Year Group",ylab = "Total Count",main = "G",ylim = c(0,70))
barplot(allhminuscounts,names.arg = c("Early","Mid","Late"),xlab = "Year Group",ylab = "Total Count",main = "H-",ylim = c(0,70))
barplot(allhcounts,names.arg = c("Early","Mid","Late"),xlab = "Year Group",ylab = "Total Count",main = "H",ylim = c(0,70))
barplot(allhpluscounts,names.arg = c("Early","Mid","Late"),xlab = "Year Group",ylab = "Total Count",main = "H+",ylim = c(0,70))
barplot(allicounts,names.arg = c("Early","Mid","Late"),xlab = "Year Group",ylab = "Total Count",main = "I",ylim = c(0,70))
###



###
#break previous barplots up into north and south
gnearly<- nrow(larvs[which(larvs$Developmental.Stage=='G' & larvs$North.or.South=='n' & larvs$Time.Period=="early"),])
gnmid<- nrow(larvs[which(larvs$Developmental.Stage=='G' & larvs$North.or.South=='n' & larvs$Time.Period=="half"),])
gnlate<- nrow(larvs[which(larvs$Developmental.Stage=='G' & larvs$North.or.South=='n' & larvs$Time.Period=="late"),])
gsearly<- nrow(larvs[which(larvs$Developmental.Stage=='G' & larvs$North.or.South=='s' & larvs$Time.Period=="early"),])
gsmid<- nrow(larvs[which(larvs$Developmental.Stage=='G' & larvs$North.or.South=='s' & larvs$Time.Period=="half"),])
gslate<- nrow(larvs[which(larvs$Developmental.Stage=='G' & larvs$North.or.South=='s' & larvs$Time.Period=="late"),])

hminusnearly<- nrow(larvs[which(larvs$Developmental.Stage=='H-' & larvs$North.or.South=='n' & larvs$Time.Period=="early"),])
hminusnmid<- nrow(larvs[which(larvs$Developmental.Stage=='H-' & larvs$North.or.South=='n' & larvs$Time.Period=="half"),])
hminusnlate<- nrow(larvs[which(larvs$Developmental.Stage=='H-' & larvs$North.or.South=='n' & larvs$Time.Period=="late"),])
hminussearly<- nrow(larvs[which(larvs$Developmental.Stage=='H-' & larvs$North.or.South=='s' & larvs$Time.Period=="early"),])
hminussmid<- nrow(larvs[which(larvs$Developmental.Stage=='H-' & larvs$North.or.South=='s' & larvs$Time.Period=="half"),])
hminusslate<- nrow(larvs[which(larvs$Developmental.Stage=='H-' & larvs$North.or.South=='s' & larvs$Time.Period=="late"),])

hnearly<- nrow(larvs[which(larvs$Developmental.Stage=='H' & larvs$North.or.South=='n' & larvs$Time.Period=="early"),])
hnmid<- nrow(larvs[which(larvs$Developmental.Stage=='H' & larvs$North.or.South=='n' & larvs$Time.Period=="half"),])
hnlate<- nrow(larvs[which(larvs$Developmental.Stage=='H' & larvs$North.or.South=='n' & larvs$Time.Period=="late"),])
hsearly<- nrow(larvs[which(larvs$Developmental.Stage=='H' & larvs$North.or.South=='s' & larvs$Time.Period=="early"),])
hsmid<- nrow(larvs[which(larvs$Developmental.Stage=='H' & larvs$North.or.South=='s' & larvs$Time.Period=="half"),])
hslate<- nrow(larvs[which(larvs$Developmental.Stage=='H' & larvs$North.or.South=='s' & larvs$Time.Period=="late"),])

hplusnearly<- nrow(larvs[which(larvs$Developmental.Stage=='H+' & larvs$North.or.South=='n' & larvs$Time.Period=="early"),])
hplusnmid<- nrow(larvs[which(larvs$Developmental.Stage=='H+' & larvs$North.or.South=='n' & larvs$Time.Period=="half"),])
hplusnlate<- nrow(larvs[which(larvs$Developmental.Stage=='H+' & larvs$North.or.South=='n' & larvs$Time.Period=="late"),])
hplussearly<- nrow(larvs[which(larvs$Developmental.Stage=='H+' & larvs$North.or.South=='s' & larvs$Time.Period=="early"),])
hplussmid<- nrow(larvs[which(larvs$Developmental.Stage=='H+' & larvs$North.or.South=='s' & larvs$Time.Period=="half"),])
hplusslate<- nrow(larvs[which(larvs$Developmental.Stage=='H+' & larvs$North.or.South=='s' & larvs$Time.Period=="late"),])

inearly<- nrow(larvs[which(larvs$Developmental.Stage=='I' & larvs$North.or.South=='n' & larvs$Time.Period=="early"),])
inmid<- nrow(larvs[which(larvs$Developmental.Stage=='I' & larvs$North.or.South=='n' & larvs$Time.Period=="half"),])
inlate<- nrow(larvs[which(larvs$Developmental.Stage=='I' & larvs$North.or.South=='n' & larvs$Time.Period=="late"),])
isearly<- nrow(larvs[which(larvs$Developmental.Stage=='I' & larvs$North.or.South=='s' & larvs$Time.Period=="early"),])
ismid<- nrow(larvs[which(larvs$Developmental.Stage=='I' & larvs$North.or.South=='s' & larvs$Time.Period=="half"),])
islate<- nrow(larvs[which(larvs$Developmental.Stage=='I' & larvs$North.or.South=='s' & larvs$Time.Period=="late"),])

gns<- c(gnearly,gsearly,gnmid,gsmid,gnlate,gslate)
hminusns<- c(hminusnearly,hminussearly,hminusnmid,hminussmid,hminusnlate,hminusslate)
hns<- c(hnearly,hsearly,hnmid,hsmid,hnlate,hslate)
hplusns<- c(hplusnearly,hplussearly,hplusnmid,hplussmid,hplusnlate,hplusslate)
ins<- c(inearly,isearly,inmid,ismid,inlate,islate)

par(mfrow=c(3,2),font.lab=2)
barplot(gns,names.arg = c("N.Early","S.Early","N.Mid","S.Mid","N.Late","S.Late"),xlab = "Year Group",ylab = "Total Count",main = "G",ylim = c(0,70),col = c('steelblue3','tomato','steelblue3','tomato','steelblue3','tomato'))
barplot(hminusns,names.arg = c("N.Early","S.Early","N.Mid","S.Mid","N.Late","S.Late"),xlab = "Year Group",ylab = "Total Count",main = "H-",ylim = c(0,70),col = c('steelblue3','tomato','steelblue3','tomato','steelblue3','tomato'))
barplot(hns,names.arg = c("N.Early","S.Early","N.Mid","S.Mid","N.Late","S.Late"),xlab = "Year Group",ylab = "Total Count",main = "H",ylim = c(0,70),col = c('steelblue3','tomato','steelblue3','tomato','steelblue3','tomato'))
barplot(hplusns,names.arg = c("N.Early","S.Early","N.Mid","S.Mid","N.Late","S.Late"),xlab = "Year Group",ylab = "Total Count",main = "H+",ylim = c(0,70),col = c('steelblue3','tomato','steelblue3','tomato','steelblue3','tomato'))
barplot(ins,names.arg = c("N.Early","S.Early","N.Mid","S.Mid","N.Late","S.Late"),xlab = "Year Group",ylab = "Total Count",main = "I",ylim = c(0,70),col = c('steelblue3','tomato','steelblue3','tomato','steelblue3','tomato'))
###work in progress...
#want to attach bars for each time period and just label early,mid,late and use colors for n & s


###
#plot counts for each stage of grouped years
earlycounts<- c(gcountsearly,hminuscountsearly,hcountsearly,hpluscountsearly,icountsearly)
midcounts<- c(gcountsmid,hminuscountsmid,hcountsmid,hpluscountsmid,icountsmid)
latecounts<- c(gcountslate,hminuscountslate,hcountslate,hpluscountslate,icountslate)

par(mfrow=c(2,2),font.lab=2)
barplot(earlycounts,names.arg = c("G","H-","H","H+","I"),xlab = "Stage",ylab = "Count",main = "Early",ylim = c(0,70))
barplot(midcounts,names.arg = c("G","H-","H","H+","I"),xlab = "Stage",ylab = "Count",main = "Mid",ylim = c(0,70))
barplot(latecounts,names.arg = c("G","H-","H","H+","I"),xlab = "Stage",ylab = "Count",main = "Late",ylim = c(0,70))
###

###
#break up into north and south
##################################this is wrong fix the order of ns variables############################
earlyncounts<- c(gnearly,hminusnearly,hnearly,hplusnearly,inearly)
earlyscounts<- c(gsearly,hminussearly,hsearly,hplussearly,isearly)
midncounts<- c(gnmid,hminusnmid,hnmid,hplusnmid,inmid)
midscounts<- c(gsmid,hminussmid,hsmid,hplussmid,ismid)
latencounts<- c(gnlate,hminusnlate,hnlate,hplusnlate,inlate)
latescounts<- c(gslate,hminusslate,hslate,hplusslate,islate)

earlynscounts<- c(gnearly,gsearly,hminusnearly,hminussearly,hnearly,hsearly,hplusnearly,hplussearly,inearly,isearly)
midnscounts<- c(gnmid,gsmid,hminusnmid,hminussmid,hnmid,hsmid,hplusnmid,hplussmid,inmid,ismid)
latenscounts<- c(gnlate,gslate,hminusnlate,hminusslate,hnlate,hslate,hplusnlate,hplusslate,inlate,islate)

par(mfrow=c(2,2))
barplot(earlynscounts,names.arg = c("N.G","S.G","N.H-","S.H-","N.H","S.H","N.H+","S.H+","N.I","S.I"),main = "Early",xlab = "Stage",ylab = "Count",ylim = c(0,70),col = c('steelblue3','tomato','steelblue3','tomato','steelblue3','tomato'))
barplot(midnscounts,names.arg = c("N.G","S.G","N.H-","S.H-","N.H","S.H","N.H+","S.H+","N.I","S.I"),main = "Mid",xlab = "Stage",ylab = "Count",ylim = c(0,70),col = c('steelblue3','tomato','steelblue3','tomato','steelblue3','tomato'))
barplot(latenscounts,names.arg = c("N.G","S.G","N.H-","S.H-","N.H","S.H","N.H+","S.H+","N.I","S.I"),main = "Late",xlab = "Stage",ylab = "Count",ylim = c(0,70),col = c('steelblue3','tomato','steelblue3','tomato','steelblue3','tomato'))
###



#### blah blah ####
#create line graphs of each stage for each year group
#proportions of counts (p. stands for proportion)
####
##p.gcountsearly<- (gcountsearly/totalgcounts)
#p.hminuscountsearly<- (hminuscountsearly/totalhminuscounts)
#p.hcountsearly<- (hcountsearly/totalhcounts)
#p.hpluscountsearly<- (hpluscountsearly/totalhpluscounts)
#p.icountsearly<- (icountsearly/totalicounts)

#p.gcountsmid<- (gcountsmid/totalgcounts)
#p.hminuscountsmid<- (hminuscountsmid/totalhminuscounts)
#p.hcountsmid<- (hcountsmid/totalhcounts)
#p.hpluscountsmid<- (hpluscountsmid/totalhpluscounts)
#p.icountsmid<- (icountsmid/totalicounts)

#p.gcountslate<- (gcountslate/totalgcounts)
#p.hminuscountslate<- (hminuscountslate/totalhminuscounts)
#p.hcountslate<- (hcountslate/totalhcounts)
#p.hpluscountslate<- (hpluscountslate/totalhpluscounts)
#p.icountslate<- (icountslate/totalicounts)
totalearlycounts<- sum(earlycounts)
totalmidcounts<- sum(midcounts)
totallatecounts<- sum(latecounts)

p.gcountsearly<- (gcountsearly/totalearlycounts)
p.hminuscountsearly<- (hminuscountsearly/totalearlycounts)
p.hcountsearly<- (hcountsearly/totalearlycounts)
p.hpluscountsearly<- (hpluscountsearly/totalearlycounts)
p.icountsearly<- (icountsearly/totalearlycounts)

p.gcountsmid<- (gcountsmid/totalmidcounts)
p.hminuscountsmid<- (hminuscountsmid/totalmidcounts)
p.hcountsmid<- (hcountsmid/totalmidcounts)
p.hpluscountsmid<- (hpluscountsmid/totalmidcounts)
p.icountsmid<- (icountsmid/totalmidcounts)

p.gcountslate<- (gcountslate/totallatecounts)
p.hminuscountslate<- (hminuscountslate/totallatecounts)
p.hcountslate<- (hcountslate/totallatecounts)
p.hpluscountslate<- (hpluscountslate/totallatecounts)
p.icountslate<- (icountslate/totallatecounts)


jpeg(file="~/Desktop/Analysis/proportionplot", width = 10, height = 7.5, res = 300, units = "in")
plot(3,1,xlim = c(1,3),ylim = c(0,1),type = "n",axes = FALSE,xlab = "Year Group",ylab = "% of Individuals")
axis(side = 1,labels = c("Early","Mid","Late"),at = c(1,2,3))
axis(side = 2)
points(c(p.gcountsearly,p.gcountsmid,p.gcountslate),pch = 16,col = 'blue')
lines(c(p.gcountsearly,p.gcountsmid,p.gcountslate),col = 'blue')
points(c(p.hminuscountsearly,p.hminuscountsmid,p.hminuscountslate),pch = 16,col = 'red')
lines(c(p.hminuscountsearly,p.hminuscountsmid,p.hminuscountslate),col = 'red')
points(c(p.hcountsearly,p.hcountsmid,p.hcountslate),pch = 16,col = 'green')
lines(c(p.hcountsearly,p.hcountsmid,p.hcountslate),col = 'green')
points(c(p.hpluscountsearly,p.hpluscountsmid,p.hpluscountslate),pch = 16,col = 'yellow')
lines(c(p.hpluscountsearly,p.hpluscountsmid,p.hpluscountslate),col = 'yellow')
points(c(p.icountsearly,p.icountsmid,p.icountslate),pch = 16,col = 'cyan')
lines(c(p.icountsearly,p.icountsmid,p.icountslate),col = 'cyan')
legend("topleft",legend = c("G (=84)","H- (=77)","H (=119)","H+ (=110)","I (=7)"),pch = c(15,15),col = c('blue','red','green','yellow','cyan'))
dev.off()
###



#Do same thing but make a north and south plot
###creating denom. for n/s proportion plot###
totalearlyncounts<- sum(earlyncounts)
totalearlyscounts<- sum(earlyscounts)
totalmidncounts<- sum(midncounts)
totalmidscounts<- sum(midscounts)
totallatencounts<- sum(latencounts)
totallatescounts<- sum(latescounts)
###









###making barplot of proportions using original larvs spreadsheet####
testtable<- table(larvs$Developmental.Stage,larvs$Time.Period)
testtable<- testtable[-c(1,7),]#removes row 1 and 7
barplot(testtable)
proportiontable<- t(t(testtable)/rowSums(t(testtable)))#dividing columns by their sums (for proportion)
proportionbarplot<- barplot(proportiontable,xlab = 'Year Group',col = c('deepskyblue','firebrick','darkolivegreen','gold','blueviolet'))


jpeg(file="~/Desktop/Analysis/proportionbarplot", width = 10, height = 7.5, res = 300, units = "in")
barplot(proportiontable,xlab = 'Year Group',col = c('deepskyblue','firebrick','darkolivegreen','gold','blueviolet'),legend = rownames(proportiontable))
dev.off()
##########################################################








#split proportion barplot into north and south
newtable<- table(larvs$Developmental.Stage,larvs$Time.Period,larvs$North.or.South)
newtable<- newtable[-c(1,7),,]#removes row 1 and 7 from both tables


full.nsTable<- cbind(newtable[,,1],newtable[,,2])
swappedtable<- full.nsTable[,c(1,4,2,5,3,6)]
ns.propTable<- t(t(swappedtable)/rowSums(t(swappedtable)))

#plot the ns barplot
jpeg(file="~/Desktop/Analysis/nsProportionBarplot", width = 10, height = 7.5, res = 300, units = "in")
barplot(ns.propTable,names.arg = c('Early N.','Early S.','Mid N.','Mid S.','Late N.','Late S.'), #names on x axis
        xlim=c(0, ncol(ns.propTable) + 3), #creates room after last bar for legend
        space = c(0.5,0), #groups every 2 bars together and leaves 0.5 space between
        col=brewer.pal(nrow(ns.propTable), "Paired"), #loaded from the library(RColorBrewer) package
        ylab="Proportion",
        legend.text=TRUE,
        args.legend=list(
          x=ncol(ns.propTable) + 3,
          y=max(colSums(ns.propTable)),
          bty = "n"
        )
)
dev.off()        
###################

#making ns proportion plot but with lines
par(mfrow=c(3,2))
plot(6,0.5,xlim = c(1,6),ylim = c(0,0.5))
points(ns.propTable[1,c(1,3,5)])
















p.gnearly<- (gnearly/totalgcounts)
p.hminusnearly<- (hminusnearly/totalhminuscounts) 
p.hnearly<- (hnearly/totalhcounts)
p.hplusnearly<- (hplusnearly/totalhpluscounts)
p.inearly<- (inearly/totalicounts)

p.gnmid<- (gnmid/totalgcounts)
p.hminusnmid<- (hminusnmid/totalhminuscounts)
p.hnmid<- (hnmid/totalhcounts)
p.hplusnmid<- (hplusnmid/totalhpluscounts)
p.inmid<- (inmid/totalicounts)

p.gnlate<- (gnlate/totalgcounts)
p.hminusnlate<- (hminusnlate/totalhminuscounts)
p.hnlate<- (hnlate/totalhcounts)
p.hplusnlate<- (hplusnlate/totalhpluscounts)
p.inlate<- (inlate/totalicounts)

#SOUTH
p.gsearly<- (gsearly/totalgcounts)
p.hminussearly<- (hminussearly/totalhminuscounts) 
p.hsearly<- (hsearly/totalhcounts)
p.hplussearly<- (hplussearly/totalhpluscounts)
p.isearly<- (isearly/totalicounts)

p.gsmid<- (gsmid/totalgcounts)
p.hminussmid<- (hminussmid/totalhminuscounts)
p.hsmid<- (hsmid/totalhcounts)
p.hplussmid<- (hplussmid/totalhpluscounts)
p.ismid<- (ismid/totalicounts)

p.gslate<- (gslate/totalgcounts)
p.hminusslate<- (hminusslate/totalhminuscounts)
p.hslate<- (hslate/totalhcounts)
p.hplusslate<- (hplusslate/totalhpluscounts)
p.islate<- (islate/totalicounts)


par(mfrow=c(2,1),
    oma=c(3.5,1,1,1),
    mar=c(4,4,4,2),
    xpd=TRUE)
plot(3,1,xlim = c(1,3),ylim = c(0,1),type = "n",axes = FALSE,xlab = "Year Group",ylab = "Proportion of Individuals",main = "North")
axis(side = 1,labels = c("Early","Mid","Late"),at = c(1,2,3))
axis(side = 2)
points(c(p.gnearly,p.gnmid,p.gnlate),pch = 16,col = 'blue')#north
lines(c(p.gnearly,p.gnmid,p.gnlate),col = 'blue')
points(c(p.hminusnearly,p.hminusnmid,p.hminusnlate),pch = 16,col = 'red')
lines(c(p.hminusnearly,p.hminusnmid,p.hminusnlate),col = 'red')
points(c(p.hnearly,p.hnmid,p.hnlate),pch = 16,col = 'green')
lines(c(p.hnearly,p.hnmid,p.hnlate),col = 'green')
points(c(p.hplusnearly,p.hplusnmid,p.hplusnlate),pch = 16,col = 'yellow')
lines(c(p.hplusnearly,p.hplusnmid,p.hplusnlate),col = 'yellow')
points(c(p.inearly,p.inmid,p.inlate),pch = 16,col = 'cyan')
lines(c(p.inearly,p.inmid,p.inlate),col = 'cyan')
legend(1.3,1,cex=0.75,legend = c("G","H-","H","H+","I"),pch = c(15,15),col = c('blue','red','green','yellow','cyan'),horiz = TRUE,bty = "n")            

plot(3,1,xlim = c(1,3),ylim = c(0,1),type = "n",axes = FALSE,xlab = "Year Group",ylab = "Proportion of Individuals",main = "South")
axis(side = 1,labels = c("Early","Mid","Late"),at = c(1,2,3))
axis(side = 2)
points(c(p.gsearly,p.gsmid,p.gslate),pch = 16,col = 'blue')#south
lines(c(p.gsearly,p.gsmid,p.gslate),col = 'blue')
points(c(p.hminussearly,p.hminussmid,p.hminusslate),pch = 16,col = 'red')
lines(c(p.hminussearly,p.hminussmid,p.hminusslate),col = 'red')
points(c(p.hsearly,p.hsmid,p.hslate),pch = 16,col = 'green')
lines(c(p.hsearly,p.hsmid,p.hslate),col = 'green')
points(c(p.hplussearly,p.hplussmid,p.hplusslate),pch = 16,col = 'yellow')
lines(c(p.hplussearly,p.hplussmid,p.hplusslate),col = 'yellow')
points(c(p.isearly,p.ismid,p.islate),pch = 16,col = 'cyan')
lines(c(p.isearly,p.ismid,p.islate),col = 'cyan')
#figuring out legend still not looking good























write.csv(tns.propTable,file = "Transposed nsPropTable.csv")

test<- read.csv("test.csv")
#ns.propTable<- read.csv("nsProportionTable.csv")
#tns.propTable<- read.csv("Transposed nsPropTable.csv")
nsProp.df<- as.data.frame(test)
tnsProp.df<- as.data.frame(tns.propTable)

nsProp.df <- ddply(nsProp.df, .(time),
                     transform, pos = cumsum(prop) - (0.5 * prop))


library(ggplot2)
p <- ggplot(nsProp.df[order(nsProp.df$stage,decreasing = TRUE),]) + 
  geom_bar(aes(y = prop, x = factor(time,levels = c("en","es","mn","ms","ln","ls")), 
               fill = factor(stage,levels = c("i","h+","h","h-","g"))), data = nsProp.df,
                         stat = "identity")+
  xlab("Time Period")+
  ylab("Proportion")#+
  #geom_text(data = nsProp.df,aes(x = time,y = pos,
           # label = paste0(prop)),size=4)
 

  

  
  
  
  
