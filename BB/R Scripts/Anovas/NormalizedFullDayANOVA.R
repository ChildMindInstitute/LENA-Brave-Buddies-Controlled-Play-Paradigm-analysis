##LENA Normalized Full Day ANOVA##

library(car)

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs")
sub_folders=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE)
sub_folders_paths=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE, full.names = TRUE)

days=c("Mon", "Tue", "Wed", "Thur")

###Vocalization Duration####

durAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Normalized Daily View", sep = ""))
  dailyFile=dir(pattern="*_Average", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  durAll=rbind(durAll, bravebuds_counts$Child_Voc_Duration[1:length(days)])
}
durAll <- na.omit(durAll)

write.csv(durAll, "durAllAverage.csv")

durAllAverage <- read.csv("durAllAverage.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
durBind <- cbind(durAllAverage$V1, durAllAverage$V2, durAllAverage$V3, durAllAverage$V4)
durModel <- lm(durBind~1)

durAnova <- Anova(durModel, idata = daysFrame, idesign =~ daysFactor)
summary(durAnova)

##Segment Duration##

segAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Normalized Daily View", sep = ""))
  dailyFile=dir(pattern="*_Average", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  segAll=rbind(segAll, bravebuds_counts$CHN[1:length(days)])
}
segAll <- na.omit(segAll)

write.csv(segAll, "segAllAverage.csv")

segAllAverage <- read.csv("segAllAverage.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
segBind <- cbind(segAllAverage$V1, segAllAverage$V2, segAllAverage$V3, segAllAverage$V4)
segModel <- lm(segBind~1)

segAnova <- Anova(segModel, idata = daysFrame, idesign =~ daysFactor)
summary(segAnova)

##Vocalizations##

wordsAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Normalized Daily View", sep = ""))
  dailyFile=dir(pattern="*_Average", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  wordsAll=rbind(wordsAll, bravebuds_counts$Child_Voc_Count[1:length(days)])
}
wordsAll <- na.omit(wordsAll)

write.csv(wordsAll, "wordsAllAverage.csv")

wordsAllAverage <- read.csv("wordsAllAverage.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
wordsBind <- cbind(wordsAllAverage$V1, wordsAllAverage$V2, wordsAllAverage$V3, wordsAllAverage$V4)
wordsModel <- lm(wordsBind~1)

wordsAnova <- Anova(wordsModel, idata = daysFrame, idesign =~ daysFactor)
summary(wordsAnova)

##Conversational Turns##

turnsAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Normalized Daily View", sep = ""))
  dailyFile=dir(pattern="*_Average", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  turnsAll=rbind(turnsAll, bravebuds_counts$Turn_Count[1:length(days)])
}
turnsAll <- na.omit(turnsAll)

write.csv(turnsAll, "turnsAllAverage.csv")

turnsAllAverage <- read.csv("turnsAllAverage.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
turnsBind <- cbind(turnsAllAverage$V1, turnsAllAverage$V2, turnsAllAverage$V3, turnsAllAverage$V4)
turnsModel <- lm(turnsBind~1)

turnsAnova <- Anova(turnsModel, idata = daysFrame, idesign =~ daysFactor)
summary(turnsAnova)

