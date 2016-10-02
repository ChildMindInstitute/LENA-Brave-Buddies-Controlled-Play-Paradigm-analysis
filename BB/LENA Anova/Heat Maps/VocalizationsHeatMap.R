##LENA ANOVA-Vocalization Heat Map##

library(car)

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs")
sub_folders=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE)
sub_folders_paths=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE, full.names = TRUE)

activities=c("Warm Up", "Morning Meeting", "Lunch", "Outside Play", "Prize Store")

###Monday####

monAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Heat Map View", sep = ""))
  dailyFile=dir(pattern="*_Vocalization", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  monAll=rbind(monAll, bravebuds_counts$Monday[1:length(activities)])
}
monAll <- na.omit(monAll)

write.csv(monAll, "monAllVocalHeatMap.csv")

monAllVocalHeatMap <- read.csv("monAllVocalHeatMap.csv")

activities <- c(1, 2, 3, 4)
activitiesFactor <- as.factor(activities)
activitiesFrame <- data.frame(activitiesFactor)
monBind <- cbind(monAllVocalHeatMap$V1, monAllVocalHeatMap$V2, monAllVocalHeatMap$V3, monAllVocalHeatMap$V4)
monModel <- lm(monBind~1)

monAnova <- Anova(monModel, idata = activitiesFrame, idesign =~ activitiesFactor)
summary(monAnova)

##Tuesday##

tuesAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Heat Map View", sep = ""))
  dailyFile=dir(pattern="*_Vocalization", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  tuesAll=rbind(tuesAll, bravebuds_counts$Tuesday[1:length(activities)])
}
tuesAll <- na.omit(tuesAll)

write.csv(tuesAll, "tuesAllVocalHeatMap.csv")

tuesAllVocalHeatMap <- read.csv("tuesAllVocalHeatMap.csv")

activities <- c(1, 2, 3, 4)
activitiesFactor <- as.factor(activities)
activitiesFrame <- data.frame(activitiesFactor)
tuesBind <- cbind(tuesAllVocalHeatMap$V1, tuesAllVocalHeatMap$V2, tuesAllVocalHeatMap$V3, tuesAllVocalHeatMap$V4)
tuesModel <- lm(tuesBind~1)

tuesAnova <- Anova(tuesModel, idata = activitiesFrame, idesign =~ activitiesFactor)
summary(tuesAnova)

##Wednesday##

wedsAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Heat Map View", sep = ""))
  dailyFile=dir(pattern="*_Vocalization", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  wedsAll=rbind(wedsAll, bravebuds_counts$Wednesday[1:length(activities)])
}
wedsAll <- na.omit(wedsAll)

write.csv(wedsAll, "wedsAllVocalHeatMap.csv")

wedsAllVocalHeatMap <- read.csv("wedsAllVocalHeatMap.csv")

activities <- c(1, 2, 3, 4)
activitiesFactor <- as.factor(activities)
activitiesFrame <- data.frame(activitiesFactor)
wedsBind <- cbind(wedsAllVocalHeatMap$V1, wedsAllVocalHeatMap$V2, wedsAllVocalHeatMap$V3, wedsAllVocalHeatMap$V4)
wedsModel <- lm(wedsBind~1)

wedsAnova <- Anova(wedsModel, idata = activitiesFrame, idesign =~ activitiesFactor)
summary(wedsAnova)

##Thursday##

thursAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Heat Map View", sep = ""))
  dailyFile=dir(pattern="*_Vocalization", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  thursAll=rbind(thursAll, bravebuds_counts$Thursday[1:length(activities)])
}
thursAll <- na.omit(thursAll)

write.csv(thursAll, "thursAllVocalHeatMap.csv")

thursAllVocalHeatMap <- read.csv("thursAllVocalHeatMap.csv")

activities <- c(1, 2, 3, 4)
activitiesFactor <- as.factor(activities)
activitiesFrame <- data.frame(activitiesFactor)
thursBind <- cbind(thursAllVocalHeatMap$V1, thursAllVocalHeatMap$V2, thursAllVocalHeatMap$V3, thursAllVocalHeatMap$V4)
thursModel <- lm(thursBind~1)

thursAnova <- Anova(thursModel, idata = activitiesFrame, idesign =~ activitiesFactor)
summary(thursAnova)