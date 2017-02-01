#LENA data analysis. 1-hr paradigm data.
#08/11/16

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean")

myfiles=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean", pattern = "M00*", ignore.case = TRUE)

mylabels= c("A", "A", "B", "B","A", "A","C", "C","A", "A" )
mycolors=rainbow(n= length(myfiles))

################ WORD COUNT
#plot child vocalization vs sessions

temp_sum_words=  c(rep(0, 10))

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Vocalization/Child_Vocalization.png",width=1400, height=600)
par(mfrow=c(1,2))
plot(c(0,10), c(0, 80), type="n" , xlab ="Session", main = "Child Vocalizations", cex.main=2, xaxt = "n",ylab = "Vocalizations")
axis(side = 1,at= c(1:10),labels =mylabels,  las=1)

for (i in 1:length(myfiles) ){
  myLena=read.csv(myfiles[i])
  points(x= myLena$Clock_Time_TZAdj, y= myLena$Child_Voc_Count, type = "p", pch=20, col= mycolors[i])
  temp_sum_words= temp_sum_words+myLena$Child_Voc_Count
  }
temp_ave_words= temp_sum_words/length(myfiles)
lines(x= myLena$Clock_Time_TZAdj, y=temp_ave_words, type="o", pch=20)
legend(x=.5, y= 80, c("Subject's vocalization count per session", "Average vocalization count across subjects per session"), lty=c(0,1),col = c(mycolors[1], "black") ,pch=20, text.font = 10, bty = "n", cex = 1)



#Child vocalization vs condition
temp_sum_words_session=  c(rep(0, 10))

wordsA= matrix( nrow = length(myfiles), ncol = 2)
wordsA[,2]=c(rep(1,length(myfiles)))

wordsB= matrix( nrow = length(myfiles), ncol = 2)
wordsB[,2]=c(rep(2,length(myfiles)))


wordsC= matrix( nrow = length(myfiles), ncol = 2)
wordsC[,2]=c(rep(3,length(myfiles)))


plot(c(0,4), c(0, 80), type="n" , xlab ="Condition", main = "Child Vocalizations", cex.main=2, xaxt = "n",ylab = "Vocalizations")
axis(side = 1,at= c(1:3),labels =c("A", "B", "C"),  las=1)

for (j in 1:length(myfiles) ){
  myLena=read.csv(myfiles[j])
  points(x= myLena$Session, y= myLena$Child_Voc_Count, type = "p", pch=20, col= mycolors[j])
  temp_sum_words_session= temp_sum_words_session+myLena$Child_Voc_Count
  wordsA[j,1]= sum(myLena$Child_Voc_Count[c(1,2,5,6,9,10)])/6
  wordsB[j,1]= sum(myLena$Child_Voc_Count[c(3,4)])/2
  wordsC[j,1]= sum(myLena$Child_Voc_Count[c(7,8)])/2
  
}

temp_ave_words_session= temp_sum_words_session/length(myfiles)
aveA=sum(temp_ave_words_session[c(1,2,5,6,9,10)])/6
aveB=sum(temp_ave_words_session[c(3,4)])/2
aveC=sum(temp_ave_words_session[c(7,8)])/2

lines(x= 1:3, y=c(aveA, aveB, aveC), type="o", pch=20)
legend(x=0, y= 80, c("Vocalization count per condition", "Average vocalization count across subjects per condition"), lty=c(0,1),col = c(mycolors[1], "black") ,pch=20, text.font = 10, bty = "n", cex = 1)
dev.off()


#boxplot
boxData=cbind(wordsA[,1], wordsB[,1], wordsC[,1])
colnames(boxData)=c("A","B", "C")

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Vocalization/boxplot_child_vocalization.png",width=700, height=600)
boxplot(boxData, main= "Child Vocalization", ylab="words", xlab="condition", ylim = c(0,90))
legend(x=2.5, y= 60, c("Median of the group", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)
dev.off()



#barplot
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Vocalization/barplot_Child_vocalization.png",width=700, height=600)
barplot(apply(boxData,2,mean), col="green", main ="Child's Vocalization", ylim = c(0,30) )
arrows(x0=1:3, y0= apply(boxData,2,mean), x1=1:3, y1=apply(boxData,2,mean)+apply(boxData,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2)
arrows(x0=1:3, y0=apply(boxData,2,mean), x1=1:3, y1=apply(boxData,2,mean)-apply(boxData,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2)

dev.off()

# dif plots (a - b), (a - c), (a - mean(b,c)), B-C
difData=cbind(boxData[,1]-boxData[,2], boxData[,1]- boxData[,3], boxData[,1]-((boxData[,2]+boxData[,3])/2), boxData[,2]-boxData[,3])
colnames(difData)=c("A-B","A-C", "A-mean(B,C)", "B-C")

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Vocalization/boxplot_Child_vocalizationDifferentials.png",width=700, height=600)
boxplot(difData, main= "Difference in word counts between baseline and experimental conditions", ylab="words", xlab="condition differentials")
legend(x=2.5, y= 12, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)
dev.off()



#ANOVA, Repeated
myModel=lm(boxData~1)
myLevels= c(1,2,3)
groupFactors=as.factor(myLevels)
groupFrame <- data.frame(groupFactors)
library("car")
analysis <- Anova(myModel, idata = groupFrame, idesign = ~groupFactors)
summary(analysis)

####################### CONVERSATIONAL TURNS ########
#plot Turn counts  vs sessions

temp_sum_turns=  c(rep(0, 10))

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Conversational Turns/Child_conversational_turns.png",width=1400, height=600)
par(mfrow=c(1,2))
plot(c(0,10), c(0, 40), type="n" , xlab ="Session", main = "Conversational Turns", cex.main=2, xaxt = "n",ylab = "Turns")
axis(side = 1,at= c(1:10),labels =mylabels,  las=1)

for (i in 1:length(myfiles) ){
  myLena=read.csv(myfiles[i])
  points(x= myLena$Clock_Time_TZAdj, y= myLena$Turn_Count, type = "p", pch=20, col= mycolors[i])
  temp_sum_turns= temp_sum_turns+myLena$Turn_Count
 
}
temp_ave_turns= temp_sum_turns/length(myfiles)
lines(x= myLena$Clock_Time_TZAdj, y=temp_ave_turns, type="o", pch=20)
legend(x=.5, y= 40, c("Subject's turn counts per session", "Average turn count across subjects per session"), lty=c(0,1),col = c(mycolors[1], "black") ,pch=20, text.font = 10, bty = "n", cex = 1)



#Child turn count vs condition
temp_sum_turn_session=  c(rep(0, 10))

turnsA= matrix( nrow = length(myfiles), ncol = 2)
turnsA[,2]=c(rep(1,length(myfiles)))

turnsB= matrix( nrow = length(myfiles), ncol = 2)
turnsB[,2]=c(rep(2,length(myfiles)))


turnsC= matrix( nrow = length(myfiles), ncol = 2)
turnsC[,2]=c(rep(3,length(myfiles)))

plot(c(0,4), c(0, 40), type="n" , xlab ="Condition", main = "Conversational Turns", cex.main=2, xaxt = "n",ylab = "Turns")
axis(side = 1,at= c(1:3),labels =c("A", "B", "C"),  las=1)

for (j in 1:length(myfiles) ){
  myLena=read.csv(myfiles[j])
  points(x= myLena$Session, y= myLena$Turn_Count, type = "p", pch=20, col= mycolors[j])
  temp_sum_turn_session= temp_sum_turn_session+myLena$Turn_Count
  turnsA[j,1]= sum(myLena$Turn_Count[c(1,2,5,6,9,10)])/6
  turnsB[j,1]= sum(myLena$Turn_Count[c(3,4)])/2
  turnsC[j,1]= sum(myLena$Turn_Count[c(7,8)])/2
}
temp_ave_turn_session= temp_sum_turn_session/length(myfiles)
aveA=sum(temp_ave_turn_session[c(1,2,5,6,9,10)])/6
aveB=sum(temp_ave_turn_session[c(3,4)])/2
aveC=sum(temp_ave_turn_session[c(7,8)])/2

lines(x= 1:3, y=c(aveA, aveB, aveC), type="o", pch=20)
legend(x=0, y= 40, c("Subject's turn count per condition", "Average turn count across subjects per condition"), lty=c(0,1),col = c(mycolors[1], "black") ,pch=20, text.font = 10, bty = "n", cex = 1)
dev.off()

#boxplot
boxData_turn=cbind(turnsA[,1], turnsB[,1],turnsC[,1])
colnames(boxData_turn)=c("A","B", "C")
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Conversational Turns/boxplot_Child_conversational_turns.png",width=700, height=600)
boxplot(boxData_turn, main= "Child conversational turns", ylab="Turn", xlab="condition", ylim = c(0, 40))
legend(x=2.5, y= 30, c("Median of the group", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = .8)
dev.off()

#barplot
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Conversational Turns/barplot_Child_turn_count.png",width=700, height=600)
barplot(apply(boxData_turn,2,mean), col="purple", main ="Conversational Turns", ylim = c(0,20) )
arrows(x0=1:3, y0= apply(boxData_turn,2,mean), x1=1:3, y1=apply(boxData_turn,2,mean)+apply(boxData_turn,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2)
arrows(x0=1:3, y0=apply(boxData_turn,2,mean), x1=1:3, y1=apply(boxData_turn,2,mean)-apply(boxData_turn,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2)
dev.off()


# dif plots (a - b), (a - c) and (a - mean(b,c))
difData_turn=cbind(boxData_turn[,1]-boxData_turn[,2], boxData_turn[,1]- boxData_turn[,3], boxData_turn[,1]-((boxData_turn[,2]+boxData_turn[,3])/2))
colnames(difData_turn)=c("A-B","A-C", "A-mean(B,C)")

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Conversational Turns/boxplot_conversational_turns_Differentials.png",width=700, height=600)
boxplot(difData_turn, main= "Difference in conversational turns between baseline and experimental conditions", ylab="turns", xlab="condition differentials")
legend(x=2.5, y= 10, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)
dev.off()



#ANOVA, Repeated
myModelTurns=lm(boxData_turn~1)
myLevels= c(1,2,3)
groupFactors=as.factor(myLevels)
groupFrame <- data.frame(groupFactors)
library("car")
analysis <- Anova(myModelTurns, idata = groupFrame, idesign = ~groupFactors)
summary(analysis)


##################### VOCALIZATION DURATION ###########
#plot child vocalization duration vs session

temp_sum_dur=  c(rep(0, 10))

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Vocalization Duration/Child_voc_duration.png",width=1400, height=600)
par(mfrow=c(1,2))
plot(c(0,10), c(0, 40), type="n" , xlab ="Session", main = "Child Vocalization Duration", cex.main=2, xaxt = "n",ylab = "time (s)")
axis(side = 1,at= c(1:10),labels =mylabels,  las=1)

for (i in 1:length(myfiles) ){
  myLena=read.csv(myfiles[i])
  points(x= myLena$Clock_Time_TZAdj, y= myLena$Child_Voc_Duration, type = "p", pch=20, col= mycolors[i])
  temp_sum_dur= temp_sum_dur+myLena$Child_Voc_Duration
}
temp_ave_dur= temp_sum_dur/length(myfiles)
lines(x= myLena$Clock_Time_TZAdj, y=temp_ave_dur, type="o", pch=20)
legend(x=.5, y= 40, c("Subject's vocalization duration per session", "Average vocalization duration across subjects per session"), lty=c(0,1),col = c(mycolors[1], "black") ,pch=20, text.font = 10, bty = "n", cex = 1)



#Child vocalization vs condition
temp_sum_dur_session=  c(rep(0, 10))


durA= matrix( nrow = length(myfiles), ncol = 2)
durA[,2]=c(rep(1,length(myfiles)))

durB= matrix( nrow = length(myfiles), ncol = 2)
durB[,2]=c(rep(2,length(myfiles)))


durC= matrix( nrow = length(myfiles), ncol = 2)
durC[,2]=c(rep(3,length(myfiles)))

plot(c(0,4), c(0, 40), type="n" , xlab ="Condition", main = "Child vocalization duration", cex.main=2, xaxt = "n",ylab = "Time")
axis(side = 1,at= c(1:3),labels =c("A", "B", "C"),  las=1)

for (j in 1:length(myfiles) ){
  myLena=read.csv(myfiles[j])
  points(x= myLena$Session, y= myLena$Child_Voc_Duration, type = "p", pch=20, col= mycolors[j])
  temp_sum_dur_session= temp_sum_dur_session+myLena$Child_Voc_Duration
  durA[j,1]= sum(myLena$Child_Voc_Duration[c(1,2,5,6,9,10)])/6
  durB[j,1]= sum(myLena$Child_Voc_Duration[c(3,4)])/2
  durC[j,1]= sum(myLena$Child_Voc_Duration[c(7,8)])/2
}
temp_ave_turn_session= temp_sum_dur_session/length(myfiles)
aveA=sum(temp_ave_turn_session[c(1,2,5,6,9,10)])/6
aveB=sum(temp_ave_turn_session[c(3,4)])/2
aveC=sum(temp_ave_turn_session[c(7,8)])/2

lines(x= 1:3, y=c(aveA, aveB, aveC), type="o", pch=20)
legend(x=0, y= 40, c("Subject's vocalization duration per condition", "Average vocalization duration across subjects per condition"), lty=c(0,1),col = c(mycolors[1], "black") ,pch=20, text.font = 10, bty = "n", cex = 1)
dev.off()

#boxplot
boxData_dur=cbind(durA[,1], durB[,1], durC[,1])
colnames(boxData_dur)=c("A","B", "C")
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Vocalization Duration/boxplot_Child_vocalization_duration.png",width=700, height=600)
boxplot(boxData_dur, main= "Child Vocalization Duration", ylab="Time", xlab="condition", ylim = c(0, 80))
legend(x=2.5, y= 65, c("Median of the group", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)
dev.off()


#barplot
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Vocalization Duration/barplot_Child_vocalization_duration.png",width=700, height=600)
barplot(apply(boxData_dur,2,mean), col="blue", main ="Child Vocalization Duration", ylim = c(0,30) )
arrows(x0=1:3, y0= apply(boxData_dur,2,mean), x1=1:3, y1=apply(boxData_dur,2,mean)+apply(boxData_dur,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2)
arrows(x0=1:3, y0=apply(boxData_dur,2,mean), x1=1:3, y1=apply(boxData_dur,2,mean)-apply(boxData_dur,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2)

dev.off()

# dif plots (a - b), (a - c) and (a - mean(b,c))
difData_dur=cbind(boxData_dur[,1]-boxData_dur[,2], boxData_dur[,1]- boxData_dur[,3], boxData_dur[,1]-((boxData_dur[,2]+boxData_dur[,3])/2))
colnames(difData_dur)=c("A-B","A-C", "A-mean(B,C)")

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Vocalization Duration/boxplot_Child_vocalization_duration_Differentials.png",width=700, height=600)
boxplot(difData_dur, main= "Difference in vocalization durations between baseline and experimental conditions", ylab="time", xlab="condition differentials")
legend(x=2.5, y= 40, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)
dev.off()


#Anova repeated
myModelDur=lm(boxData_dur~1)
myLevels= c(1,2,3)
groupFactors=as.factor(myLevels)
groupFrame <- data.frame(groupFactors)
library("car")
analysis <- Anova(myModelDur, idata = groupFrame, idesign = ~groupFactors)
summary(analysis)

#anova(lm(y ~ group , data=mydurs))


################ SEGMENT DURATION ###############
#plot child SEGMENT vs sessions

temp_sum_seg=  c(rep(0, 10))

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Segment Duration/segment.png",width=1400, height=600)
par(mfrow=c(1,2))
plot(c(0,10), c(0, 80), type="n" , xlab ="Session", main = "Child Segment Duration", cex.main=2, xaxt = "n",ylab = "combined voice and words")
axis(side = 1,at= c(1:10),labels =mylabels,  las=1)

for (i in 1:length(myfiles) ){
  myLena=read.csv(myfiles[i])
  points(x= myLena$Clock_Time_TZAdj, y= myLena$CHN, type = "p", pch=20, col= mycolors[i])
  temp_sum_seg= temp_sum_seg+myLena$CHN
}
temp_ave_seg= temp_sum_seg/length(myfiles)
lines(x= myLena$Clock_Time_TZAdj, y=temp_ave_seg, type="o", pch=20)
legend(x=.5, y= 80, c("Subject's segments per session", "Average segment across subjects per session"), lty=c(0,1),col = c(mycolors[1], "black") ,pch=20, text.font = 10, bty = "n", cex = 1)



#Child vocalization vs condition
temp_sum_seg_session=  c(rep(0, 10))

segA= matrix( nrow = length(myfiles), ncol = 2)
segA[,2]=c(rep(1,length(myfiles)))

segB= matrix( nrow = length(myfiles), ncol = 2)
segB[,2]=c(rep(2,length(myfiles)))


segC= matrix( nrow = length(myfiles), ncol = 2)
segC[,2]=c(rep(3,length(myfiles)))


plot(c(0,4), c(0, 80), type="n" , xlab ="Condition", main = "Child Segment Duration", cex.main=2, xaxt = "n",ylab = "combined voice and words")
axis(side = 1,at= c(1:3),labels =c("A", "B", "C"),  las=1)

for (j in 1:length(myfiles) ){
  myLena=read.csv(myfiles[j])
  points(x= myLena$Session, y= myLena$CHN, type = "p", pch=20, col= mycolors[j])
  temp_sum_seg_session= temp_sum_seg_session+myLena$CHN
  segA[j,1]= sum(myLena$CHN[c(1,2,5,6,9,10)])/6
  segB[j,1]= sum(myLena$CHN[c(3,4)])/2
  segC[j,1]= sum(myLena$CHN[c(7,8)])/2
  
}

temp_ave_seg_session= temp_sum_seg_session/length(myfiles)
aveA=sum(temp_ave_seg_session[c(1,2,5,6,9,10)])/6
aveB=sum(temp_ave_seg_session[c(3,4)])/2
aveC=sum(temp_ave_seg_session[c(7,8)])/2

lines(x= 1:3, y=c(aveA, aveB, aveC), type="o", pch=20)
legend(x=0, y= 80, c("Subject's segment duration per condition", "Average segment duration across subjects per condition"), lty=c(0,1),col = c(mycolors[1], "black") ,pch=20, text.font = 10, bty = "n", cex = 1)
dev.off()


#boxplot
boxData_seg=cbind(segA[,1], segB[,1], segC[,1])
colnames(boxData_seg)=c("A","B", "C")

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Segment Duration/boxplot_segment.png",width=700, height=600)
boxplot(boxData_seg, main= "Child segment duration", ylab="segment duration", xlab="condition")
legend(x=2.5, y= 40, c("Median of the group", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)
dev.off()


#barplot
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Segment Duration/barplot_segment.png",width=700, height=600)
barplot(apply(boxData_seg,2,mean), col="orange", main ="Child's segment duration", ylim = c(0,30) )
arrows(x0=1:3, y0= apply(boxData_seg,2,mean), x1=1:3, y1=apply(boxData_seg,2,mean)+apply(boxData_seg,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2)
arrows(x0=1:3, y0=apply(boxData_seg,2,mean), x1=1:3, y1=apply(boxData_seg,2,mean)-apply(boxData_seg,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2)

dev.off()

# dif plots (a - b), (a - c) and (a - mean(b,c))
difData_seg=cbind(boxData_seg[,1]-boxData_seg[,2], boxData_seg[,1]- boxData_seg[,3], boxData_seg[,1]-((boxData_seg[,2]+boxData_seg[,3])/2))
colnames(difData_seg)=c("A-B","A-C", "A-mean(B,C)")

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Segment Duration/boxplot_segmentDifferentials.png",width=700, height=600)
boxplot(difData_seg, main= "Differece in segment duration between baseline and experimental conditions", ylab="segment duration", xlab="condition differentials")
legend(x=2.5, y= 10, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)
dev.off()

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Segment Duration/barplot_segmentDifferentials.png",width=700, height=600)
barplot(apply(difData_seg,2,mean), col="orange", main ="Differece in segment duration between baseline and experimental conditions", ylim = c(0,6),cex.axis=1, cex.main=1, cex.lab=1, cex.names = 1, ylab = "average difference in segment duration")
arrows(x0=1:3, y0= apply(difData_seg,2,mean), x1=1:3, y1=apply(difData_seg,2,mean)+apply(difData_seg,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "red")
arrows(x0=1:3, y0=apply(difData_seg,2,mean), x1=1:3, y1=apply(difData_seg,2,mean)-apply(difData_seg,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "red")
legend(x=2.5, y= 6, c( "Standard Error"), lty=1, lwd = 2 , text.font = 10, bty = "n", cex = 1, col = "red")
dev.off()


#ANOVA, Repeated
myModel=lm(boxData_seg~1)
myLevels= c(1,2,3)
groupFactors=as.factor(myLevels)
groupFrame <- data.frame(groupFactors)
library("car")
analysis <- Anova(myModel, idata = groupFrame, idesign = ~groupFactors)
summary(analysis)



##################### dif boxplots: all in one
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/Differentials.png",width=2100, height=800)
par(mfrow=c(2,3))
boxplot(difData, main= "Difference in word counts between baseline and experimental conditions", ylab="words", xlab="condition differentials", cex.axis=2, cex.main=2, cex.lab=2, col="green")
legend(x=2.5, y= 10, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 2)

boxplot(difData_dur, main= "Difference in vocalization durations between baseline and experimental conditions", ylab="time", xlab="condition differentials",cex.axis=2, cex.main=2, cex.lab=2, col= "lightblue")
legend(x=2.5, y= 10, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 2)

boxplot(difData_turn, main= "Difference in conversational turns between baseline and experimental conditions", ylab="turns", xlab="condition differentials",cex.axis=2, cex.main=2, cex.lab=2, col="purple")
legend(x=2.5, y= 10, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 2)

dev.off()

##### diff barplots: all in one
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and plots/barplot_differential.png",width=2100, height=700)
par(mfrow=c(1,3))

barplot(apply(difData,2,mean), col="green", main ="Difference in word counts between baseline and experimental conditions", ylim = c(0,6),cex.axis=2, cex.main=2, cex.lab=2, cex.names = 2, ylab = "average difference in word counts")
arrows(x0=1:4, y0= apply(difData,2,mean), x1=1:4, y1=apply(difData,2,mean)+apply(difData,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "darkgreen")
arrows(x0=1:4, y0=apply(difData,2,mean), x1=1:4, y1=apply(difData,2,mean)-apply(difData,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "darkgreen")
legend(x=2.5, y= 6, c( "Standard Error"), lty=1, lwd = 2 , text.font = 10, bty = "n", cex = 2, col = "darkgreen")

barplot(apply(difData_dur,2,mean), col="lightblue", main ="Difference in vocalization durations between baseline and experimental conditions", ylim = c(0,6),cex.axis=2, cex.main=2, cex.lab=2, cex.names = 2, ylab= "average difference in vocalization duration")
arrows(x0=1:3, y0= apply(difData_dur,2,mean), x1=1:3, y1=apply(difData_dur,2,mean)+apply(difData_dur,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "blue")
arrows(x0=1:3, y0=apply(difData_dur,2,mean), x1=1:3, y1=apply(difData_dur,2,mean)-apply(difData_dur,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "blue")
legend(x=2.5, y= 6, c( "Standard Error"), lty=1, lwd = 2 , text.font = 10, bty = "n", cex = 2, col="blue")

barplot(apply(difData_turn,2,mean), col="purple", main ="Difference in conversational turns between baseline and experimental conditions", ylim = c(0,6),cex.axis=2, cex.main=2, cex.lab=2,cex.names = 2, ylab = "average difference in conversational turns")
arrows(x0=1:3, y0= apply(difData_turn,2,mean), x1=1:3, y1=apply(difData_turn,2,mean)+apply(difData_turn,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "pink")
arrows(x0=1:3, y0=apply(difData_turn,2,mean), x1=1:3, y1=apply(difData_turn,2,mean)-apply(difData_turn,2,sd)/sqrt(length(myfiles)),length=.05,angle = 90,lty=1,lwd=2, col = "pink")
legend(x=2.5, y= 6, c( "Standard Error"), lty=1, lwd = 2 , text.font = 10, bty = "n", cex = 2, col = "pink")


dev.off()




