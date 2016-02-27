#read in data file
datafile<-read.csv("Thomsen_J?rgensen_et_al._JAE_All_data_1992-2009.csv",as.is=T)

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
beetles<-subset(datafile,Year!=1992&Year!=2009)

#How often is there any data on any species
#surveys<-unique(datafile[,c("year","date1","date2")])
#surveys<-surveys[order(surveys$date1),]
#hmm not sure I understand 

#for the moment just consider annual totals
library(plyr)
beetles<-ddply(beetles,.(Species,Year),summarise,Count=sum(Count),.drop=FALSE)

################################################################################

#Plotting species number
beetlesSN<-ddply(beetles,.(Year),summarise,nuSpecies=length(unique(Species[Count!=0])))

library(ggplot2)
qplot(Year,nuSpecies,data=beetlesSN,geom=c("point","line"))+theme_bw()
#variable but perhaps increasing

################################################################################

#Plotting total abundance
beetlesTC<-ddply(beetles,.(Year),summarise,totCount=sum(Count))

qplot(Year,totCount,data=beetlesTC,geom=c("point","line"))+theme_bw()
#variable - large peak 2006

################################################################################

#Plotting community similarity through time from year 1 (1993)

#get 1993 data and merge with data frame
beetles1993<-subset(beetles,Year==1993)
beetles$Count1993<-beetles1993$Count[match(beetles$Species,beetles1993$Species)]

#library(poilog)#for the bipoilogMLE function 
#beetlesCS<-ddply(beetles,.(Year),function(x){
#est<-bipoilogMLE(x[,c("Count","Count1993")])
#return(est$par[5])
#})
#above code doesnt seem to work

library(fossil)#used to get Chao's Jaccard and Sorenson shared species estimators

beetlesCS<-ddply(beetles,.(Year),function(x){
chao.sorenson(x[,"Count"],x[,"Count1993"])
})
names(beetlesCS)[2]<-"sorenson"

beetlesJ<-ddply(beetles,.(Year),function(x){
chao.jaccard(x[,"Count"],x[,"Count1993"])
})
names(beetlesJ)[2]<-"jaccard"

beetlesCS<-merge(beetlesCS,beetlesJ,by="Year")
library(reshape2)
beetlesCS<-melt(beetlesCS,id="Year")
names(beetlesCS)[2:3]<-c("Index","Value")

ggplot(beetlesCS)+geom_line(aes(x=Year,y=Value,colour=Index))+theme_bw()
#why a flat line at 1995 and 1996????? Problem with code or data????

################################################################################
