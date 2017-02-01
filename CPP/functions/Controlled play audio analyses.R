##Controlled play paradigm audio level analyses

library("ggplot2")
library("gplots")
library("plyr")
library("car")
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Segment Level Detail")

myfiles=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Segment Level Detail", pattern = "M00*", ignore.case = TRUE)

avg <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/AVGAudio.csv")

SessionA <- data.frame(avg$A)

SessionB <- data.frame(avg$B)

SessionC <- data.frame(avg$C)

boxData <- cbind(SessionA, SessionB, SessionC)

##BOXPLOT##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and Plots/Audio Levels/Boxplot.png",width=1500, height=600) #Opens a PNG file#
par(mfrow=c(1,2)) #Splits png into 2 sections

difData=cbind(boxData[,1]-boxData[,2], boxData[,1]- boxData[,3], boxData[,1]-((boxData[,2]+boxData[,3])/2), boxData[,2]-boxData[,3])
colnames(difData)=c("A-B","A-C", "A-mean(B,C)", "B-C")
boxplot(boxData, main= "Average Child Segment dB SPL vs Condition", ylab="dB SPL", xlab="condition")
legend(x=0.5, y=80, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)
boxplot(difData, main= "Difference in Average Child Segment dB SPL between baseline and experimental conditions", ylab="Average Child Segment dB SPL", xlab="condition differentials")
legend(x=0.5, y= -10, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)

dev.off()

##ANOVA-Repeated Measures##
myLevels= c(1,2,3)
groupFactors=as.factor(myLevels)
groupFrame <- data.frame(groupFactors)
boxData <- as.matrix(boxData)
myModel=lm(boxData~1)
analysis <- Anova(myModel, idata = groupFrame, idesign = ~groupFactors)
summary(analysis)

#Density Plot-AVG Values
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and Plots/Audio Levels/Average Density Plot.png",width=1000, height=600) #Opens a PNG file#

names(SessionA) <- c("Avg")
names(SessionB) <- c("Avg")
names(SessionC) <- c("Avg")

#Adds Label Column to data frames
SessionA$label <- 'A'
SessionB$label <- 'B'
SessionC$label <- 'C'

avgAll <- rbind(SessionA, SessionB, SessionC)
ggplot(avgAll, aes(Avg, fill = label))+ geom_density(alpha = 0.2)
dev.off()

##LINE GRAPH##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and Plots/Audio Levels/Line Graph.png",width=1000, height=600) #Opens a PNG file#

linedata <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/linegraphdata.csv")
mylabels= c("A", "B","C")
plot_colors <- c("blue", "red", "green", "yellow", "orange", "purple", "pink", "turquoise", "grey")
plot(c(1,3), c(60, 90), type="n" , xlab ="Condition", main = "Child Volume Across Conditions", cex.main=2, xaxt = "n",ylab = "Average Segment dB SPL")
axis(side = 1,at= c(1:3),labels =mylabels,  las=0)

lines(linedata$M00412434, type = "o",pch=22, lty=2,
      col = plot_colors[1])
lines(linedata$M00413464, type = "o",pch=22, lty=2,
      col = plot_colors[2])
lines(linedata$M00426908, type = "o",pch=22, lty=2,
      col = plot_colors[3])
lines(linedata$M00440011, type = "o",pch=22, lty=2,
      col = plot_colors[4])
lines(linedata$M00440728, type = "o",pch=22, lty=2,
      col = plot_colors[5])
lines(linedata$M00441664, type = "o",pch=22, lty=2,
      col = plot_colors[6])
lines(linedata$M00445929, type = "o",pch=22, lty=2,
      col = plot_colors[7])
lines(linedata$M00475465, type = "o",pch=22, lty=2,
      col = plot_colors[8])
lines(linedata$M00494954, type = "o",pch=22, lty=2,
      col = plot_colors[9])
##Avg and Stdev
fit <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/stdevavg.csv")
lines(fit$avgtotal, type = "o", pch = 20, col="black")
x <- 1:3
arrows(x, fit$avgtotal-fit$stdev, x, fit$avgtotal+fit$stdev, length = 0.05, angle = 90, code = 3)
dev.off()

##Average Level Density Plot-Totals

filenames <- list.files(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Segment Level Detail", pattern = "M00*", full.names = FALSE)
numfiles <- length(filenames)
for(i in c(1:numfiles)){
  filenames[i] <- paste("",filenames[i],sep="")  
  assign(gsub("[.]csv$","",filenames[i]),read.csv(filenames[i], header=TRUE, stringsAsFactors = FALSE))
}

full_list = list(M00402147_controlledplay_detailed, M00403142_controlledplay_detailed, M00409047_controlledplay_detailed,
                 M00412434_controlledplay_detailed, M00413464_controlledplay_detailed, M00426908_controlledplay_detailed,
                 M00440011_controlledplay_detailed, M00440728_controlledplay_detailed, M00441664_controlledplay_detailed, 
                 M00445929_controlledplay_detailed, M00475465_controlledplay_detailed, M00494954_controlledplay_detailed) 
avgaudio <- do.call(rbind, lapply(full_list, function(x) x[, c(1, 21)])) ##Bind all Session and Audio Level columns into a single data frame

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and Plots/Audio Levels/Density Plot-Average.png",width=1000, height=600) #Opens a PNG file#

ggplot(avgaudio, aes(Average_SignalLevel, fill = Session))+ geom_density(alpha= 0.4)+
  ggtitle("Average Segment dB SPL Density By Condition") + 
  scale_x_continuous(name = "Average Child Segment dB SPL")
dev.off()

##Peak Density Plot-totals
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and Plots/Audio Levels/Density Plot-Peak.png",width=1000, height=600) #Opens a PNG file#

peakaudio <- do.call(rbind, lapply(full_list, function(x) x[, c(1, 22)]))
ggplot(peakaudio, aes(Peak_SignalLevel, fill = Session))+ geom_density(alpha= 0.4)+
  ggtitle("Peak Segment dB SPL Density By Condition")+
  scale_x_continuous(name = "Peak Child Segment dB SPL")
dev.off()

#############Vocalizations x Volume############

##Avg
avgcomp <- do.call(rbind, lapply(full_list, function(x) x[, c(1, 11, 30)]))
avgcomp$composite <- avgcomp$Child_Voc_Count * avgcomp$Average_SignalLevel
avgxvoc <- cbind(avgcomp$Session, avgcomp$composite)

Adata <- subset(avgcomp, Session == "A", composite)
Bdata <- subset(avgcomp, Session =="B", composite)
Cdata <- subset(avgcomp, Session == "C", composite)

Adata$Session <- "A"
Bdata$Session <- "B"
Cdata$Session <- "C"

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and Plots/Audio Levels/AVGxVOC Violin Plot.png",width=1000, height=600) #Opens a PNG file#

avgviolin <- ggplot()+geom_violin(data = Adata, aes(x = Session, y = composite, colour = "A")) + 
  geom_violin(data = Bdata, aes(x = Session, y = composite, colour ="B")) +
  geom_violin(data = Cdata, aes(x = Session, y = composite, colour ="C")) +
  scale_y_continuous(name = "Average Child Segment dB SPL x # of Vocalizations per Segment") +
  ggtitle("Weighted Child Segment Average dB SPL")
avgviolin
dev.off()

##Peak
peakcomp <- do.call(rbind, lapply(full_list, function(x) x[, c(1, 11, 31)]))
peakcomp$composite <- peakcomp$Child_Voc_Count * peakcomp$Peak_SignalLevel
peakxvoc <- cbind(peakcomp$Session, peakcomp$composite)

AdataP <- subset(peakcomp, Session == "A", composite)
BdataP <- subset(peakcomp, Session =="B", composite)
CdataP <- subset(peakcomp, Session == "C", composite)

AdataP$Session <- "A"
BdataP$Session <- "B"
CdataP$Session <- "C"

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and Plots/Audio Levels/PEAKxVOC Violin Plot.png",width=1000, height=600) #Opens a PNG file#
peakviolin <- ggplot()+geom_violin(data = AdataP, aes(x = Session, y = composite, colour = "A")) + 
  geom_violin(data = BdataP, aes(x = Session, y = composite, colour ="B")) +
  geom_violin(data = CdataP, aes(x = Session, y = composite, colour ="C")) +
  scale_y_continuous(name = "Peak Child Segment dB SPL x # of Vocalizations per Segment") +
  ggtitle("Weighted Child Segment Peak dB SPL")
peakviolin
dev.off()

#Combined Violin Plots
library("gridExtra")
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Results and Plots/Audio Levels/VOLxVOC Violin Plots.png",width=1200, height=600) #Opens a PNG file#

grid.arrange(avgviolin, peakviolin, ncol = 2)

dev.off()