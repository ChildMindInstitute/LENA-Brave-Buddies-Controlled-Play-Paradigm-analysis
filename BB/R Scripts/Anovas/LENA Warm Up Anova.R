##LENA WARM UP ANOVA##

library(car)

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs")
sub_folders=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE)
sub_folders_paths=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE, full.names = TRUE)

days=c("Mon", "Tue", "Wed", "Thur")

###Vocalization Duration####

durAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Event View", sep = ""))
  dailyFile=dir(pattern="*_WarmUp", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  durAll=rbind(durAll, bravebuds_counts$Child_Voc_Duration[1:length(days)])
}
durAll <- na.omit(durAll)

write.csv(durAll, "durAllWarmUp.csv")

durAllWarmUp <- read.csv("durAllWarmUp.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
durBind <- cbind(durAllWarmUp$V1, durAllWarmUp$V2, durAllWarmUp$V3, durAllWarmUp$V4)
durModel <- lm(durBind~1)

durAnova <- Anova(durModel, idata = daysFrame, idesign =~ daysFactor)
summary(durAnova)

##Segment Duration##

segAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Event View", sep = ""))
  dailyFile=dir(pattern="*_WarmUp", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  segAll=rbind(segAll, bravebuds_counts$CHN[1:length(days)])
}
segAll <- na.omit(segAll)

write.csv(segAll, "segAllWarmUp.csv")

segAllWarmUp <- read.csv("segAllWarmUp.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
segBind <- cbind(segAllWarmUp$V1, segAllWarmUp$V2, segAllWarmUp$V3, segAllWarmUp$V4)
segModel <- lm(segBind~1)

segAnova <- Anova(segModel, idata = daysFrame, idesign =~ daysFactor)
summary(segAnova)

##Vocalizations##

wordsAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Event View", sep = ""))
  dailyFile=dir(pattern="*_WarmUp", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  wordsAll=rbind(wordsAll, bravebuds_counts$Child_Voc_Count[1:length(days)])
}
wordsAll <- na.omit(wordsAll)

write.csv(wordsAll, "wordsAllWarmUp.csv")

wordsAllWarmUp <- read.csv("wordsAllWarmUp.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
wordsBind <- cbind(wordsAllWarmUp$V1, wordsAllWarmUp$V2, wordsAllWarmUp$V3, wordsAllWarmUp$V4)
wordsModel <- lm(wordsBind~1)

wordsAnova <- Anova(wordsModel, idata = daysFrame, idesign =~ daysFactor)
summary(wordsAnova)

##Conversational Turns##

turnsAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Event View", sep = ""))
  dailyFile=dir(pattern="*_WarmUp", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  turnsAll=rbind(turnsAll, bravebuds_counts$Turn_Count[1:length(days)])
}
turnsAll <- na.omit(turnsAll)

write.csv(turnsAll, "turnsAllWarmUp.csv")

turnsAllWarmUp <- read.csv("turnsAllWarmUp.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
turnsBind <- cbind(turnsAllWarmUp$V1, turnsAllWarmUp$V2, turnsAllWarmUp$V3, turnsAllWarmUp$V4)
turnsModel <- lm(turnsBind~1)

turnsAnova <- Anova(turnsModel, idata = daysFrame, idesign =~ daysFactor)
summary(turnsAnova)

