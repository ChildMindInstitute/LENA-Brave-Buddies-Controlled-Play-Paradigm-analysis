library(car)

setwd("/Users/helen.xu/Desktop/LENA_analyses/Brave Buddies/LENA Anova/Lunch")
durAll<-read.csv("durAll.csv")
segAll<-read.csv("segAll.csv")
turnsAll<-read.csv("turnsAll.csv")
wordsAll<-read.csv("wordsAll.csv")

######Vocalization Duration Anova######
days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
durBind <- cbind(durAll$V1, durAll$V2, durAll$V3, durAll$V4)
durModel <- lm(durBind~1)

durAnova <- Anova(durModel, idata = daysFrame, idesign =~ daysFactor)
summary(durAnova)

####Segment Duration Anova######

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
segBind <- cbind(segAll$V1, segAll$V2, segAll$V3, segAll$V4)
segModel <- lm(segBind~1)

segAnova <- Anova(segModel, idata = daysFrame, idesign =~ daysFactor)
summary(segAnova)

#####Conversational Turns Anova######

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
turnsBind <- cbind(turnsAll$V1, turnsAll$V2, turnsAll$V3, turnsAll$V4)
turnsModel <- lm(turnsBind~1)

turnsAnova <- Anova(turnsModel, idata = daysFrame, idesign =~ daysFactor)
summary(turnsAnova)

#####Vocalizations Anova#####

days <- c(1, 2, 3, 4)
daysFactor <- as.factor(days)
daysFrame <- data.frame(daysFactor)
wordsBind <- cbind(wordsAll$V1, wordsAll$V2, wordsAll$V3, wordsAll$V4)
wordsModel <- lm(wordsBind~1)

wordsAnova <- Anova(wordsModel, idata = daysFrame, idesign =~ daysFactor)
summary(wordsAnova)