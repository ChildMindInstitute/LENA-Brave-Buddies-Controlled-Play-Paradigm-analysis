###Brave Buddies Morning Meeting Audio Level ANOVAs####
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
  dailyFile=dir(pattern="*_MorningMeetingAudio", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  avgAll=rbind(avgAll, bravebuds_counts$Average_SignalLevel[1:length(days)])
}
avgAll <- na.omit(avgAll)

write.csv(avgAll, "avgAllMorningMeeting.csv")

avgAllMorningMeeting <- read.csv("avgAllMorningMeeting.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
avgBind <- cbind(avgAllMorningMeeting$V1, avgAllMorningMeeting$V2, avgAllMorningMeeting$V3, avgAllMorningMeeting$V4)
avgModel <- lm(avgBind~1)

avgAnova <- Anova(avgModel, idata = daysFrame, idesign =~ daysFactor)
summary(avgAnova)

######Peak dB Level######

peakAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Audio Level View", sep = ""))
  dailyFile=dir(pattern="*_MorningMeetingAudio", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  peakAll=rbind(peakAll, bravebuds_counts$Peak_SignalLevel[1:length(days)])
}
peakAll <- na.omit(peakAll)

write.csv(peakAll, "peakAllMorningMeeting.csv")

peakAllMorningMeeting <- read.csv("peakAllMorningMeeting.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
peakBind <- cbind(peakAllMorningMeeting$V1, peakAllMorningMeeting$V2, peakAllMorningMeeting$V3, peakAllMorningMeeting$V4)
peakModel <- lm(peakBind~1)

peakAnova <- Anova(peakModel, idata = daysFrame, idesign =~ daysFactor)
summary(peakAnova)
