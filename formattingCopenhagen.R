#read in data file
datafile<-read.csv("Thomsen_Jogensen_et_al._JAE_All_data_1992-2009.csv",as.is=T)
#note: the "o" in Jogensen of the file name was anglicized because git didnt like it.

#tidy species names
namessplit<-sapply(datafile$name,function(x)strsplit(x," "))
datafile$Species<-sapply(namessplit,function(x)paste(x[1],x[2],sep=" "))
#correct mistakes
datafile$Species[which(datafile$Species=="Prays f.")]<-"Prays ruficeps"
datafile$Species[which(datafile$Species=="Euxoa 'tritici'")]<-"Euxoa tritici"