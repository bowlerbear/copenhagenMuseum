#read in data file
datafile<-read.csv("Thomsen_Jogensen_et_al._JAE_All_data_1992-2009.csv",as.is=T)
#note: the "o" in Jogensen of the file name was anglicized because git didnt like it.

#tidy species names
namessplit<-sapply(datafile$name,function(x)strsplit(x," "))
datafile$Species<-sapply(namessplit,function(x)paste(x[1],x[2],sep=" "))
#correct mistakes
datafile$Species[which(datafile$Species=="Prays f.")]<-"Prays ruficeps"
datafile$Species[which(datafile$Species=="Euxoa 'tritici'")]<-"Euxoa tritici"

#format time data
library(lubridate)
datafile$date1<-as.Date(datafile$date1,format="%m/%d/%y")
datafile$date2<-as.Date(datafile$date2,format="%m/%d/%y")
datafile$Year<-year(datafile$date1)
datafile$yDay<-yday(datafile$date1)
datafile$Month<-month(datafile$date1)

#give the count data a nicer name
names(datafile)[which(names(datafile)=="individuals")]<-"Count"

#just begin with look at beetles
beetles<-subset(datafile,order=="COLEOPTERA")

#like paper exclude 1992 and 2009 data
beetles<-subset(beetles,Year!=1992&Year!=2009)

#for the moment just consider annual totals
library(plyr)
beetles<-ddply(beetles,.(Species,Year),summarise,Count=sum(Count),.drop=FALSE)

#Plotting species number
beetlesSN<-ddply(beetles,.(Year),summarise,nuSpecies=length(unique(Species[Count!=0])))

library(ggplot2)
qplot(Year,nuSpecies,data=beetlesSN,geom=c("point","line"))+theme_bw()

#Plotting total abundance
beetlesTC<-ddply(beetles,.(Year),summarise,totCount=sum(Count))

qplot(Year,totCount,data=beetlesTC,geom=c("point","line"))+theme_bw()
