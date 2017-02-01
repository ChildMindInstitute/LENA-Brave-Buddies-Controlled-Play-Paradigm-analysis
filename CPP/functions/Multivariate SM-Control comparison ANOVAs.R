#Multivariate ANOVAS for SM vs Control-Controlled Play Paradigm
#Jacob Stroud, 12/15/16

library("sqldf")
library("tidyr")

##Vocalizations
#control
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean")
myfiles=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean", pattern = "M00*", ignore.case = TRUE)
wordsAll= c()
mylabels= c("A", "A", "B", "B","A", "A","C", "C","A", "A" )
mycolors=rainbow(n= length(myfiles))

temp_sum_words_session=  c(rep(0, 10))
wordsA= matrix( nrow = length(myfiles), ncol = 2)
wordsA[,2]=c(rep(1,length(myfiles)))

wordsB= matrix( nrow = length(myfiles), ncol = 2)
wordsB[,2]=c(rep(2,length(myfiles)))

wordsC= matrix( nrow = length(myfiles), ncol = 2)
wordsC[,2]=c(rep(3,length(myfiles)))

for (j in 1:length(myfiles) ){
  myLena=read.csv(myfiles[j])
  temp_sum_words_session= temp_sum_words_session+myLena$Child_Voc_Count
  wordsA[j,1]= sum(myLena$Child_Voc_Count[c(1,2,5,6,9,10)])/6
  wordsB[j,1]= sum(myLena$Child_Voc_Count[c(3,4)])/2
  wordsC[j,1]= sum(myLena$Child_Voc_Count[c(7,8)])/2
}

boxData=cbind(wordsA[,1], wordsB[,1], wordsC[,1])
colnames(boxData)=c("A","B", "C")

#Adds column indicating SM diagnosis
boxData <- as.data.frame(boxData)
boxData$SM_dx <- rep(0,nrow(boxData)) 

#SM
rm(list=setdiff(ls(), "boxData"))
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean")
myfiles=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean", pattern = "M00*", ignore.case = TRUE)
wordsAll= c()
mylabels= c("A", "A", "B", "B","A", "A","C", "C","A", "A" )
mycolors=rainbow(n= length(myfiles))

temp_sum_words_session=  c(rep(0, 10))
wordsA= matrix( nrow = length(myfiles), ncol = 2)
wordsA[,2]=c(rep(1,length(myfiles)))

wordsB= matrix( nrow = length(myfiles), ncol = 2)
wordsB[,2]=c(rep(2,length(myfiles)))

wordsC= matrix( nrow = length(myfiles), ncol = 2)
wordsC[,2]=c(rep(3,length(myfiles)))

for (j in 1:length(myfiles) ){
  myLena=read.csv(myfiles[j])
  temp_sum_words_session= temp_sum_words_session+myLena$Child_Voc_Count
  wordsA[j,1]= sum(myLena$Child_Voc_Count[c(1,2,5,6,9,10)])/6
  wordsB[j,1]= sum(myLena$Child_Voc_Count[c(3,4)])/2
  wordsC[j,1]= sum(myLena$Child_Voc_Count[c(7,8)])/2
}

SMboxData=cbind(wordsA[,1], wordsB[,1], wordsC[,1])
colnames(SMboxData)=c("A","B", "C")

#Adds column indicating SM diagnosis
SMboxData <- as.data.frame(SMboxData)
SMboxData$SM_dx <- rep(1,nrow(SMboxData)) 

#Combine data frames into a single df
anovadata <- rbind(boxData, SMboxData)

#Add ID column
anovadata$ID <- seq.int(nrow(anovadata))

#Convert data into long form
anovadatalong <- gather(anovadata, block, avgVox, A:C)

#ANOVA
voxanova <- aov(avgVox~(block*SM_dx)+Error(ID/(block))+(SM_dx), data = anovadatalong)
summary(voxanova)

##Conversational Turns
rm(list=ls())
#control
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean")
myfiles=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean", pattern = "M00*", ignore.case = TRUE)
turnsAll= c()
mylabels= c("A", "A", "B", "B","A", "A","C", "C","A", "A" )
mycolors=rainbow(n= length(myfiles))

temp_sum_turn_session=  c(rep(0, 10))

turnsA= matrix( nrow = length(myfiles), ncol = 2)
turnsA[,2]=c(rep(1,length(myfiles)))

turnsB= matrix( nrow = length(myfiles), ncol = 2)
turnsB[,2]=c(rep(2,length(myfiles)))

turnsC= matrix( nrow = length(myfiles), ncol = 2)
turnsC[,2]=c(rep(3,length(myfiles)))

for (j in 1:length(myfiles) ){
  myLena=read.csv(myfiles[j])
  temp_sum_turn_session= temp_sum_turn_session+myLena$Turn_Count
  turnsA[j,1]= sum(myLena$Turn_Count[c(1,2,5,6,9,10)])/6
  turnsB[j,1]= sum(myLena$Turn_Count[c(3,4)])/2
  turnsC[j,1]= sum(myLena$Turn_Count[c(7,8)])/2
}

boxData_turn=cbind(turnsA[,1], turnsB[,1],turnsC[,1])
colnames(boxData_turn)=c("A","B", "C")

boxData_turn <- as.data.frame(boxData_turn)
boxData_turn$SM_dx <- rep(0,nrow(boxData_turn)) 

#SM
rm(list=setdiff(ls(), "boxData_turn"))
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean")
myfiles=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean", pattern = "M00*", ignore.case = TRUE)
turnsAll= c()
mylabels= c("A", "A", "B", "B","A", "A","C", "C","A", "A" )
mycolors=rainbow(n= length(myfiles))

temp_sum_turn_session=  c(rep(0, 10))

turnsA= matrix( nrow = length(myfiles), ncol = 2)
turnsA[,2]=c(rep(1,length(myfiles)))

turnsB= matrix( nrow = length(myfiles), ncol = 2)
turnsB[,2]=c(rep(2,length(myfiles)))

turnsC= matrix( nrow = length(myfiles), ncol = 2)
turnsC[,2]=c(rep(3,length(myfiles)))

for (j in 1:length(myfiles) ){
  myLena=read.csv(myfiles[j])
  temp_sum_turn_session= temp_sum_turn_session+myLena$Turn_Count
  turnsA[j,1]= sum(myLena$Turn_Count[c(1,2,5,6,9,10)])/6
  turnsB[j,1]= sum(myLena$Turn_Count[c(3,4)])/2
  turnsC[j,1]= sum(myLena$Turn_Count[c(7,8)])/2
}

SMboxData_turn=cbind(turnsA[,1], turnsB[,1],turnsC[,1])
colnames(SMboxData_turn)=c("A","B", "C")

SMboxData_turn <- as.data.frame(SMboxData_turn)
SMboxData_turn$SM_dx <- rep(1,nrow(SMboxData_turn))

#Combine data frames into a single df
turnanovadata <- rbind(boxData_turn, SMboxData_turn)

#Add ID column
turnanovadata$ID <- seq.int(nrow(turnanovadata))

#Convert data into long form
turnanovadatalong <- gather(turnanovadata, block, avgTurns, A:C)

#ANOVA
turnanova <- aov(avgTurns~(block*SM_dx)+Error(ID/(block))+(SM_dx), data = turnanovadatalong)
summary(turnanova)

##Vocalization Duration
rm(list=ls())
#control
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean")
myfiles=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean", pattern = "M00*", ignore.case = TRUE)
durAll= c()
mylabels= c("A1", "A1", "B", "B","A2", "A2","C", "C","A3", "A3" )
mycolors=rainbow(n= length(myfiles))

temp_sum_dur_session=  c(rep(0, 10))

durA= matrix( nrow = length(myfiles), ncol = 2)
durA[,2]=c(rep(1,length(myfiles)))

durB= matrix( nrow = length(myfiles), ncol = 2)
durB[,2]=c(rep(2,length(myfiles)))

durC= matrix( nrow = length(myfiles), ncol = 2)
durC[,2]=c(rep(3,length(myfiles)))

for (j in 1:length(myfiles) ){
  myLena=read.csv(myfiles[j])
  temp_sum_dur_session= temp_sum_dur_session+myLena$Child_Voc_Duration
  durA[j,1]= sum(myLena$Child_Voc_Duration[c(1,2,5,6,9,10)])/6
  durB[j,1]= sum(myLena$Child_Voc_Duration[c(3,4)])/2
  durC[j,1]= sum(myLena$Child_Voc_Duration[c(7,8)])/2
}

boxData_dur=cbind(durA[,1], durB[,1], durC[,1])
colnames(boxData_dur)=c("A","B", "C")

boxData_dur <- as.data.frame(boxData_dur)
boxData_dur$SM_dx <- rep(0,nrow(boxData_dur))

#SM
rm(list=setdiff(ls(), "boxData_dur"))
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean")
myfiles=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean", pattern = "M00*", ignore.case = TRUE)
durAll= c()
mylabels= c("A", "A", "B", "B","A", "A","C", "C","A", "A" )
mycolors=rainbow(n= length(myfiles))

temp_sum_dur_session=  c(rep(0, 10))

durA= matrix( nrow = length(myfiles), ncol = 2)
durA[,2]=c(rep(1,length(myfiles)))

durB= matrix( nrow = length(myfiles), ncol = 2)
durB[,2]=c(rep(2,length(myfiles)))

durC= matrix( nrow = length(myfiles), ncol = 2)
durC[,2]=c(rep(3,length(myfiles)))

for (j in 1:length(myfiles) ){
  myLena=read.csv(myfiles[j])
  temp_sum_dur_session= temp_sum_dur_session+myLena$Child_Voc_Duration
  durA[j,1]= sum(myLena$Child_Voc_Duration[c(1,2,5,6,9,10)])/6
  durB[j,1]= sum(myLena$Child_Voc_Duration[c(3,4)])/2
  durC[j,1]= sum(myLena$Child_Voc_Duration[c(7,8)])/2
}

SMboxData_dur=cbind(durA[,1], durB[,1], durC[,1])
colnames(SMboxData_dur)=c("A","B", "C")

SMboxData_dur <- as.data.frame(SMboxData_dur)
SMboxData_dur$SM_dx <- rep(1,nrow(SMboxData_dur))

#Combine data frames into a single df
duranovadata <- rbind(boxData_dur, SMboxData_dur)

#Add ID column
duranovadata$ID <- seq.int(nrow(duranovadata))

#Convert data into long form
duranovadatalong <- gather(duranovadata, block, avgDur, A:C)

#ANOVA
duranova <- aov(avgDur~(block*SM_dx)+Error(ID/(block))+(SM_dx), data = duranovadatalong)
summary(duranova)
