###Brave Buddies Lunch Audio Level ANOVAs####
#Jacob Stroud
#September 2016

library(car)

setwd("/Users/helen.xu/Desktop/LENA_analyses/Brave Buddies/LENA Outputs")
sub_folders=dir(path = "/Users/helen.xu/Desktop/LENA_analyses/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE)
sub_folders_paths=dir(path = "/Users/helen.xu/Desktop/LENA_analyses/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE, full.names = TRUE)

days=c("Mon", "Tue", "Wed", "Thur")

###Average dB Level####

avgAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Audio Level View", sep = ""))
  dailyFile=dir(pattern="*_LunchAudio.csv", full.names = FALSE)
  bravebuds_counts= read.csv(DF)
  avgAll=rbind(avgAll, bravebuds_counts$Average_SignalLevel[1:length(days)])
}
avgAll <- na.omit(avgAll)

write.csv(avgAll, "avgAllLunch.csv")

avgAllLunch <- read.csv("avgAllLunch.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
avgBind <- cbind(avgAllLunch$V1, avgAllLunch$V2, avgAllLunch$V3, avgAllLunch$V4)
avgModel <- lm(avgBind~1)

avgAnova <- Anova(avgModel, idata = daysFrame, idesign =~ daysFactor)
summary(avgAnova)

######Peak dB Level######

peakAll=c()

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Audio Level View", sep = ""))
  dailyFile=dir(pattern="*_LunchAudio", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  peakAll=rbind(peakAll, bravebuds_counts$Peak_SignalLevel[1:length(days)])
}
peakAll <- na.omit(peakAll)

write.csv(peakAll, "peakAllLunch.csv")

peakAllLunch <- read.csv("peakAllLunch.csv")

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
peakBind <- cbind(peakAllLunch$V1, peakAllLunch$V2, peakAllLunch$V3, peakAllLunch$V4)
peakModel <- lm(peakBind~1)

peakAnova <- Anova(peakModel, idata = daysFrame, idesign =~ daysFactor)
summary(peakAnova)
