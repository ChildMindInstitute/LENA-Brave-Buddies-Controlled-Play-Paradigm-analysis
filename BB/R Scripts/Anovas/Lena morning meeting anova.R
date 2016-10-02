##LENA ANOVA-MORNING MEETING##

library(car)

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs")
sub_folders=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE)
sub_folders_paths=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE, full.names = TRUE)

days=c("Mon", "Tue", "Wed", "Thur")

###Vocalization Duration####

durAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Event View", sep = ""))
  dailyFile=dir(pattern="*_MorningMeeting", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  durAll=rbind(durAll, bravebuds_counts$Child_Voc_Duration[1:length(days)])
}
durAll <- na.omit(durAll)

write.csv(durAll, "durAllMorningMeeting.csv")

durAllMorningMeeting <- read.csv("durAllMorningMeeting.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
durBind <- cbind(durAllMorningMeeting$V1, durAllMorningMeeting$V2, durAllMorningMeeting$V3, durAllMorningMeeting$V4)
durModel <- lm(durBind~1)

durAnova <- Anova(durModel, idata = daysFrame, idesign =~ daysFactor)
summary(durAnova)

##Segment Duration##

segAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Event View", sep = ""))
  dailyFile=dir(pattern="*_MorningMeeting", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  segAll=rbind(segAll, bravebuds_counts$CHN[1:length(days)])
}
segAll <- na.omit(segAll)

write.csv(segAll, "segAllMorningMeeting.csv")

segAllMorningMeeting <- read.csv("segAllMorningMeeting.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
segBind <- cbind(segAllMorningMeeting$V1, segAllMorningMeeting$V2, segAllMorningMeeting$V3, segAllMorningMeeting$V4)
segModel <- lm(segBind~1)

segAnova <- Anova(segModel, idata = daysFrame, idesign =~ daysFactor)
summary(segAnova)

##Vocalizations##

wordsAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Event View", sep = ""))
  dailyFile=dir(pattern="*_MorningMeeting", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  wordsAll=rbind(wordsAll, bravebuds_counts$Child_Voc_Count[1:length(days)])
}
wordsAll <- na.omit(wordsAll)

write.csv(wordsAll, "wordsAllMorningMeeting.csv")

wordsAllMorningMeeting <- read.csv("wordsAllMorningMeeting.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
wordsBind <- cbind(wordsAllMorningMeeting$V1, wordsAllMorningMeeting$V2, wordsAllMorningMeeting$V3, wordsAllMorningMeeting$V4)
wordsModel <- lm(wordsBind~1)

wordsAnova <- Anova(wordsModel, idata = daysFrame, idesign =~ daysFactor)
summary(wordsAnova)

##Conversational Turns##

turnsAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Event View", sep = ""))
  dailyFile=dir(pattern="*_MorningMeeting", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  turnsAll=rbind(turnsAll, bravebuds_counts$Turn_Count[1:length(days)])
}
turnsAll <- na.omit(turnsAll)

write.csv(turnsAll, "turnsAllMorningMeeting.csv")

turnsAllMorningMeeting <- read.csv("turnsAllMorningMeeting.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
turnsBind <- cbind(turnsAllMorningMeeting$V1, turnsAllMorningMeeting$V2, turnsAllMorningMeeting$V3, turnsAllMorningMeeting$V4)
turnsModel <- lm(turnsBind~1)

turnsAnova <- Anova(turnsModel, idata = daysFrame, idesign =~ daysFactor)
summary(turnsAnova)