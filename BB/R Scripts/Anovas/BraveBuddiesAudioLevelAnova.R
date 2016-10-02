###Brave Buddies Audio Level ANOVAs####
#Jacob Stroud
#September 2016

library(car)

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs")
sub_folders=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE)
sub_folders_paths=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE, full.names = TRUE)

days=c("Mon", "Tue", "Wed", "Thur")

###Average dB Level####

avgAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Audio Level View", sep = ""))
  dailyFile=dir(pattern="*_AudioLevels", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  avgAll=rbind(avgAll, bravebuds_counts$Average_SignalLevel[1:length(days)])
}

write.csv(avgAll, "avgAllAverage.csv")

avgAllAverage <- read.csv("avgAllAverage.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
avgBind <- cbind(avgAllAverage$V1, avgAllAverage$V2, avgAllAverage$V3, avgAllAverage$V4)
avgModel <- lm(avgBind~1)

avgAnova <- Anova(avgModel, idata = daysFrame, idesign =~ daysFactor)
summary(avgAnova)

######Peak dB Level######

peakAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Audio Level View", sep = ""))
  dailyFile=dir(pattern="*_AudioLevels", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  peakAll=rbind(peakAll, bravebuds_counts$Peak_SignalLevel[1:length(days)])
}

write.csv(peakAll, "peakAllAverage.csv")

peakAllAverage <- read.csv("peakAllAverage.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
peakBind <- cbind(peakAllAverage$V1, peakAllAverage$V2, peakAllAverage$V3, peakAllAverage$V4)
peakModel <- lm(peakBind~1)

peakAnova <- Anova(peakModel, idata = daysFrame, idesign =~ daysFactor)
summary(peakAnova)
