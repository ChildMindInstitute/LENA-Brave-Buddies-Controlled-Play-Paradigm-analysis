##CONTROLLED PLAY ANALYSIS-CONTROL COMPARISONS
##JAKE STROUD
##11/30/16

library("dplyr")
library("ggplot2")
library("gplots")
library("cowplot")
library("gridExtra")

##Vocalizations
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean")
myfiles=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean", pattern = "M00*", ignore.case = TRUE)
wordsAll= c()
mylabels= c("A1", "A1", "B", "B","A2", "A2","C", "C","A3", "A3" )
mycolors=rainbow(n= length(myfiles))

temp_sum_words=  c(rep(0, 10))
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Control vs SM Comparisons/VocalizationControlvsSM.png",width=700, height=600)
plot(c(0,10), c(0, 110), type="n" , xlab ="Session", main = "Child Vocalizations", cex.main=2, xaxt = "n",ylab = "Vocalizations")
axis(side = 1,at= c(1:10),labels =mylabels,  las=1)

for (i in 1:length(myfiles) ){
  myLena=read.csv(myfiles[i])
  points(x= myLena$Clock_Time_TZAdj, y= myLena$Child_Voc_Count, type = "p", pch=20, col= "red")
  temp_sum_words= temp_sum_words+myLena$Child_Voc_Count
  wordsAll=rbind(wordsAll, myLena$Child_Voc_Count[1:length(mylabels)])
}

temp_ave_words= temp_sum_words/length(myfiles)
lines(x= myLena$Clock_Time_TZAdj, y=temp_ave_words, type="o", pch=20, col= "red")
arrows(x0=1:10, y0=apply(wordsAll,2,mean), x1=1:10, y1=apply(wordsAll,2,mean)+apply(wordsAll,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "red")
arrows(x0=1:10, y0=apply(wordsAll,2,mean), x1=1:10, y1=apply(wordsAll,2,mean)-apply(wordsAll,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "red")

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean")
SMwordsAll= c()
SMfiles=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean", pattern = "M00*", ignore.case = TRUE)
SM_sum_words=  c(rep(0, 10))
for (i in 1:length(SMfiles) ){
  SMLena=read.csv(SMfiles[i])
  points(x= SMLena$Clock_Time_TZAdj, y= SMLena$Child_Voc_Count, type = "p", pch=20, col= "blue")
  SM_sum_words= SM_sum_words+SMLena$Child_Voc_Count
  SMwordsAll=rbind(SMwordsAll, SMLena$Child_Voc_Count[1:length(mylabels)])
}

SM_ave_words= SM_sum_words/length(SMfiles)
lines(x= SMLena$Clock_Time_TZAdj, y=SM_ave_words, type="o", pch=20, col= "blue")
legend(x=.5, y= 100, c("Subject's vocalization count per session", "Average vocalization count across subjects per session"), lty=c(0,1),col = c(mycolors[1], "black") ,pch=20, text.font = 10, bty = "n", cex = 0.7)
arrows(x0=1:10, y0=apply(SMwordsAll,2,mean), x1=1:10, y1=apply(SMwordsAll,2,mean)+apply(SMwordsAll,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "blue")
arrows(x0=1:10, y0=apply(SMwordsAll,2,mean), x1=1:10, y1=apply(SMwordsAll,2,mean)-apply(SMwordsAll,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "blue")

dev.off()

##Boxplot

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean")
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

temp_ave_words_session= temp_sum_words_session/length(myfiles)
aveA=sum(temp_ave_words_session[c(1,2,5,6,9,10)])/6
aveB=sum(temp_ave_words_session[c(3,4)])/2
aveC=sum(temp_ave_words_session[c(7,8)])/2

boxData=cbind(wordsA[,1], wordsB[,1], wordsC[,1])
colnames(boxData)=c("A","B", "C")
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Vocalization/boxplot_control_child_vocalization.png",width=700, height=600)
boxplot(boxData, main= "Control Child Vocalizations", ylab="words", xlab="condition", ylim = c(0,90))
legend(x=1.5, y= 75, c("Median of the group", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)
dev.off()

#Differential Boxplots
difData=cbind(boxData[,1]-boxData[,2], boxData[,1]- boxData[,3], boxData[,1]-((boxData[,2]+boxData[,3])/2), boxData[,2]-boxData[,3])
colnames(difData)=c("A-B","A-C", "A-mean(B,C)", "B-C")

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Vocalization/Control_Vocalizations_Differentials.png",width=700, height=600)
boxplot(difData, main= "Difference in word counts between baseline and experimental conditions", ylab="words", xlab="condition differentials")
legend(x=2.5, y= -20, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)
dev.off()

##ANOVA
myModel=lm(boxData~1)
myLevels= c(1,2,3)
groupFactors=as.factor(myLevels)
groupFrame <- data.frame(groupFactors)
library("car")
analysis <- Anova(myModel, idata = groupFrame, idesign = ~groupFactors)
summary(analysis)

##Turn Count
rm(list=ls())
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean")
myfiles=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean", pattern = "M00*", ignore.case = TRUE)
turnsAll= c()
mylabels= c("A1", "A1", "B", "B","A2", "A2","C", "C","A3", "A3" )
mycolors=rainbow(n= length(myfiles))

temp_sum_turns=  c(rep(0, 10))

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Control vs SM Comparisons/ConversationalTurnsControlvsSM.png",width=700, height=600)
plot(c(0,10), c(0, 60), type="n" , xlab ="Session", main = "Conversational Turns by Block", cex.main=2, xaxt = "n",ylab = "Turns")
axis(side = 1,at= c(1:10),labels =mylabels,  las=1)

for (i in 1:length(myfiles) ){
  myLena=read.csv(myfiles[i])
  points(x= myLena$Clock_Time_TZAdj, y= myLena$Turn_Count, type = "p", pch=20, col= "red")
  temp_sum_turns= temp_sum_turns+myLena$Turn_Count
  turnsAll=rbind(turnsAll, myLena$Turn_Count[1:length(mylabels)])
}

temp_ave_turns= temp_sum_turns/length(myfiles)
lines(x= myLena$Clock_Time_TZAdj, y=temp_ave_turns, type="o", pch=20, col= "red")
arrows(x0=1:10, y0=apply(turnsAll,2,mean), x1=1:10, y1=apply(turnsAll,2,mean)+apply(turnsAll,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "red")
arrows(x0=1:10, y0=apply(turnsAll,2,mean), x1=1:10, y1=apply(turnsAll,2,mean)-apply(turnsAll,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "red")

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean")
SMturnsAll= c()
SMfiles=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean", pattern = "M00*", ignore.case = TRUE)
SM_sum_turns=  c(rep(0, 10))
for (i in 1:length(SMfiles) ){
  SMLena=read.csv(SMfiles[i])
  points(x= SMLena$Clock_Time_TZAdj, y= SMLena$Turn_Count, type = "p", pch=20, col= "blue")
  SM_sum_turns= SM_sum_turns+SMLena$Turn_Count
  SMturnsAll=rbind(SMturnsAll, SMLena$Turn_Count[1:length(mylabels)])
}

SM_ave_turns= SM_sum_turns/length(SMfiles)
lines(x= SMLena$Clock_Time_TZAdj, y=SM_ave_turns, type="o", pch=20, col= "blue")
legend(x=.5, y= 50, c("Conversational Turn count per session", "Average conversational turn count across subjects per session"), lty=c(0,1),col = c(mycolors[1], "black") ,pch=20, text.font = 10, bty = "n", cex = 0.7)
arrows(x0=1:10, y0=apply(SMturnsAll,2,mean), x1=1:10, y1=apply(SMturnsAll,2,mean)+apply(SMturnsAll,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "blue")
arrows(x0=1:10, y0=apply(SMturnsAll,2,mean), x1=1:10, y1=apply(SMturnsAll,2,mean)-apply(SMturnsAll,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "blue")

dev.off()

#boxplot
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean")
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
temp_ave_turn_session= temp_sum_turn_session/length(myfiles)
aveA=sum(temp_ave_turn_session[c(1,2,5,6,9,10)])/6
aveB=sum(temp_ave_turn_session[c(3,4)])/2
aveC=sum(temp_ave_turn_session[c(7,8)])/2

boxData_turn=cbind(turnsA[,1], turnsB[,1],turnsC[,1])
colnames(boxData_turn)=c("A","B", "C")
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Conversational Turns/boxplot_control_Child_conversational_turns.png",width=700, height=600)
boxplot(boxData_turn, main= "Control Child conversational turns", ylab="Turn", xlab="condition", ylim = c(0, 40))
legend(x=1.5, y= 32, c("Median of the group", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = .8)
dev.off()

#Differential Measures
difData_turn=cbind(boxData_turn[,1]-boxData_turn[,2], boxData_turn[,1]- boxData_turn[,3], boxData_turn[,1]-((boxData_turn[,2]+boxData_turn[,3])/2), boxData_turn[,2]-boxData_turn[,3])
colnames(difData_turn)=c("A-B","A-C", "A-mean(B,C)", "B-C")

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Conversational Turns/Control_conversational_turns_Differentials.png",width=700, height=600)
boxplot(difData_turn, main= "Difference in conversational turns between baseline and experimental conditions", ylab="turns", xlab="condition differentials")
legend(x=2.5, y= -10, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)
dev.off()

#ANOVA, Repeated
myModelTurns=lm(boxData_turn~1)
myLevels= c(1,2,3)
groupFactors=as.factor(myLevels)
groupFrame <- data.frame(groupFactors)
library("car")
analysis <- Anova(myModelTurns, idata = groupFrame, idesign = ~groupFactors)
summary(analysis)

#Vocalization Duration
rm(list=ls())
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean")
myfiles=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean", pattern = "M00*", ignore.case = TRUE)
durAll= c()
mylabels= c("A1", "A1", "B", "B","A2", "A2","C", "C","A3", "A3" )
mycolors=rainbow(n= length(myfiles))

temp_sum_dur=  c(rep(0, 10))

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Control vs SM Comparisons/VocDurationControlvsSM.png",width=700, height=600)
plot(c(0,10), c(0, 85), type="n" , xlab ="Session", main = "Vocalization Duration by Block", cex.main=2, xaxt = "n",ylab = "Child Vocalization Duration")
axis(side = 1,at= c(1:10),labels =mylabels,  las=1)

for (i in 1:length(myfiles) ){
  myLena=read.csv(myfiles[i])
  points(x= myLena$Clock_Time_TZAdj, y= myLena$Child_Voc_Duration, type = "p", pch=20, col= "red")
  temp_sum_dur= temp_sum_dur+myLena$Child_Voc_Duration
  durAll=rbind(durAll, myLena$Child_Voc_Duration[1:length(mylabels)])
}

temp_ave_dur= temp_sum_dur/length(myfiles)
lines(x= myLena$Clock_Time_TZAdj, y=temp_ave_dur, type="o", pch=20, col= "red")
arrows(x0=1:10, y0=apply(durAll,2,mean), x1=1:10, y1=apply(durAll,2,mean)+apply(durAll,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "red")
arrows(x0=1:10, y0=apply(durAll,2,mean), x1=1:10, y1=apply(durAll,2,mean)-apply(durAll,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "red")

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean")
SMdurAll= c()
SMfiles=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean", pattern = "M00*", ignore.case = TRUE)
SM_sum_dur=  c(rep(0, 10))
for (i in 1:length(SMfiles) ){
  SMLena=read.csv(SMfiles[i])
  points(x= SMLena$Clock_Time_TZAdj, y= SMLena$Child_Voc_Duration, type = "p", pch=20, col= "blue")
  SM_sum_dur= SM_sum_dur+SMLena$Child_Voc_Duration
  SMdurAll=rbind(SMdurAll, SMLena$Child_Voc_Duration[1:length(mylabels)])
}

SM_ave_dur= SM_sum_dur/length(SMfiles)
lines(x= SMLena$Clock_Time_TZAdj, y=SM_ave_dur, type="o", pch=20, col= "blue")
legend(x=.5, y= 80, c("Vocalization Duration per session", "Average Vocalization Duration across subjects per session"), lty=c(0,1),col = c(mycolors[1], "black") ,pch=20, text.font = 10, bty = "n", cex = 0.7)
arrows(x0=1:10, y0=apply(SMdurAll,2,mean), x1=1:10, y1=apply(SMdurAll,2,mean)+apply(SMdurAll,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "blue")
arrows(x0=1:10, y0=apply(SMdurAll,2,mean), x1=1:10, y1=apply(SMdurAll,2,mean)-apply(SMdurAll,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "blue")

dev.off()

#Boxplot
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Clean")
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
temp_ave_turn_session= temp_sum_dur_session/length(myfiles)
aveA=sum(temp_ave_turn_session[c(1,2,5,6,9,10)])/6
aveB=sum(temp_ave_turn_session[c(3,4)])/2
aveC=sum(temp_ave_turn_session[c(7,8)])/2

boxData_dur=cbind(durA[,1], durB[,1], durC[,1])
colnames(boxData_dur)=c("A","B", "C")
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Vocalization Duration/boxplot_Control_Child_vocalization_duration.png",width=700, height=600)
boxplot(boxData_dur, main= "Control Child Vocalization Duration", ylab="Time", xlab="condition", ylim = c(0, 80))
legend(x=1.5, y= 65, c("Median of the group", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)
dev.off()

#Differential Measures
difData_dur=cbind(boxData_dur[,1]-boxData_dur[,2], boxData_dur[,1]- boxData_dur[,3], boxData_dur[,1]-((boxData_dur[,2]+boxData_dur[,3])/2), boxData_dur[,2]-boxData_dur[,3])
colnames(difData_dur)=c("A-B","A-C", "A-mean(B,C)", "B-C")

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Vocalization Duration/Control_vocalization_duration_Differentials.png",width=700, height=600)
boxplot(difData_dur, main= "Difference in vocalization durations between baseline and experimental conditions", ylab="time", xlab="condition differentials")
legend(x=2.5, y= -20, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)
dev.off()

#Anova repeated
myModelDur=lm(boxData_dur~1)
myLevels= c(1,2,3)
groupFactors=as.factor(myLevels)
groupFrame <- data.frame(groupFactors)
library("car")
analysis <- Anova(myModelDur, idata = groupFrame, idesign = ~groupFactors)
summary(analysis)

##Audio Level Analyses
rm(list=ls())
#Control
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Segment Level Detail")
filenames <- list.files(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Segment Level Detail", pattern = "M00*", full.names = FALSE)
numfiles <- length(filenames)
for(i in c(1:numfiles)){
  filenames[i] <- paste("",filenames[i],sep="")  
  assign(gsub("[.]csv$","",filenames[i]),read.csv(filenames[i], header=TRUE, stringsAsFactors = FALSE))
}

full_list = list(M00418353_controlledplay_detailed, M00424384_controlledplay_detailed, M00446613_controlledplay_detailed,
                 M00466567_controlledplay_detailed, M00470412_controlledplay_detailed, M00473061_controlledplay_detailed,
                 M00488575_controlledplay_detailed, M00490364_controlledplay_detailed, M00492101_controlledplay_detailed,
                 M00490836_controlledplay_detailed)
avgaudio <- do.call(rbind, lapply(full_list, function(x) x[, c(1, 2, 37)])) ##Bind all Session and Audio Level columns into a single data frame
peakaudio <- do.call(rbind, lapply(full_list, function(x) x[, c(1, 38)]))
#SM
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Segment Level Detail")
SMfilenames <- list.files(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Segment Level Detail", pattern = "M00*", full.names = FALSE)
SMnumfiles <- length(SMfilenames)
for(i in c(1:SMnumfiles)){
  SMfilenames[i] <- paste("",SMfilenames[i],sep="")  
  assign(gsub("[.]csv$","",SMfilenames[i]),read.csv(SMfilenames[i], header=TRUE, stringsAsFactors = FALSE))
}

SMfull_list = list(M00402147_controlledplay_detailed, M00403142_controlledplay_detailed, M00409047_controlledplay_detailed,
                   M00412434_controlledplay_detailed, M00413464_controlledplay_detailed, M00426908_controlledplay_detailed,
                   M00440011_controlledplay_detailed, M00440728_controlledplay_detailed, M00441664_controlledplay_detailed, 
                   M00445929_controlledplay_detailed, M00475465_controlledplay_detailed, M00494954_controlledplay_detailed) 
                  
SMavgaudio <- do.call(rbind, lapply(SMfull_list, function(x) x[, c(1, 2, 21)])) ##Bind all Session and Audio Level columns into a single data frame
SMpeakaudio <- do.call(rbind, lapply(SMfull_list, function(x) x[, c(1, 2, 22)]))

targetA <- c("A")
targetB <- c("B")
targetC <- c("C")

#Average

avgAdf <- filter(avgaudio, Session %in% targetA)
SMavgAdf <- filter(SMavgaudio, Session %in% targetA)

avgBdf <- filter(avgaudio, Session %in% targetB)
SMavgBdf <- filter(SMavgaudio, Session %in% targetB)

avgCdf <- filter(avgaudio, Session %in% targetC)
SMavgCdf <- filter(SMavgaudio, Session %in% targetC)

densityA <- ggplot() + geom_density(data = avgAdf, aes(Average_SignalLevel, fill = "Control"), alpha = 0.3) +
  geom_density(data = SMavgAdf, aes(Average_SignalLevel, fill = "SM"), alpha = 0.3) + 
  ggtitle("Block A") + scale_x_continuous(name = "Average Child Segment dB SPL")

densityB <-ggplot() + geom_density(data = avgBdf, aes(Average_SignalLevel, fill = "Control"), alpha = 0.3) +
  geom_density(data = SMavgBdf, aes(Average_SignalLevel, fill = "SM"), alpha = 0.3)+
  ggtitle("Block B") + scale_x_continuous(name = "Average Child Segment dB SPL")
  
densityC <- ggplot() + geom_density(data = avgCdf, aes(Average_SignalLevel, fill = "Control"), alpha = 0.3) +
  geom_density(data = SMavgCdf, aes(Average_SignalLevel, fill = "SM"), alpha = 0.3)+
  ggtitle("Block C") + scale_x_continuous(name = "Average Child Segment dB SPL")

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Control vs SM Comparisons/ControlvsSMAverageAudioDensityPlots.png",width=800, height=800)
grid.arrange(densityA, densityB, densityC, ncol = 1, nrow = 3)
dev.off()

#Peak

peakAdf <- filter(peakaudio, Session %in% targetA)
SMpeakAdf <- filter(SMpeakaudio, Session %in% targetA)

peakBdf <- filter(peakaudio, Session %in% targetB)
SMpeakBdf <- filter(SMpeakaudio, Session %in% targetB)

peakCdf <- filter(peakaudio, Session %in% targetC)
SMpeakCdf <- filter(SMpeakaudio, Session %in% targetC)

peakdensityA <- ggplot() + geom_density(data = peakAdf, aes(Peak_SignalLevel, fill = "Control"), alpha = 0.3) +
  geom_density(data = SMpeakAdf, aes(Peak_SignalLevel, fill = "SM"), alpha = 0.3) + 
  ggtitle("Block A") + scale_x_continuous(name = "Peak Child Segment dB SPL")

peakdensityB <-ggplot() + geom_density(data = peakBdf, aes(Peak_SignalLevel, fill = "Control"), alpha = 0.3) +
  geom_density(data = SMpeakBdf, aes(Peak_SignalLevel, fill = "SM"), alpha = 0.3)+
  ggtitle("Block B") + scale_x_continuous(name = "Peak Child Segment dB SPL")

peakdensityC <- ggplot() + geom_density(data = peakCdf, aes(Peak_SignalLevel, fill = "Control"), alpha = 0.3) +
  geom_density(data = SMpeakCdf, aes(Peak_SignalLevel, fill = "SM"), alpha = 0.3)+
  ggtitle("Block C") + scale_x_continuous(name = "Peak Child Segment dB SPL")

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Control vs SM Comparisons/ControlvsSMPeakAudioDensityPlots.png",width=800, height=800)
grid.arrange(peakdensityA, peakdensityB, peakdensityC, ncol = 1, nrow = 3)
dev.off()

