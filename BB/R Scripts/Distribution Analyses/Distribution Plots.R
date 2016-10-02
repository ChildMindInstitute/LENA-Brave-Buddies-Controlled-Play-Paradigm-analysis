##Brave Buddies Average Audio Level Distribution Plots
#Jacob Stroud
#October 2016

library("ggplot2")
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs")

####M00494954

monday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00494954/Distributions/M00494954_monday.csv")

tuesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00494954/Distributions/M00494954_tuesday.csv")

wednesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00494954/Distributions/M00494954_wednesday.csv")

thursday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00494954/Distributions/M00494954_thursday.csv")

#creates a label column
monday$label <- 'mon'
tuesday$label <- 'tues'
wednesday$label <- 'wed'
thursday$label <- 'thurs'

###Full Day Plot###
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00494954/M00494954_Full.png",width=1000, height=600) #Opens a PNG file#

#create new data frames with just full day values and labels
mondata <- data.frame(monday$Full.Day, monday$label)
names(mondata) <- c("Full.Day", "label") #renames columns to a consistent label that does not contain the day
tuesdata <- data.frame(tuesday$Full.Day, tuesday$label)
names(tuesdata) <- c("Full.Day", "label")
weddata <- data.frame(wednesday$Full.Day, wednesday$label)
names(weddata) <- c("Full.Day", "label")
thursdata <- data.frame(thursday$Full.Day, thursday$label)
names(thursdata) <- c("Full.Day", "label")

histData <- rbind(mondata, tuesdata, weddata, thursdata) #creates data frame with histogram data

ggplot(histData, aes(Full.Day, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Warm Up##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00494954/M00494954_WarmUp.png",width=1000, height=600) #Opens a PNG file#

mondataWU <- data.frame(monday$Warm.Up, monday$label)
names(mondataWU) <- c("Warm.Up", "label")
tuesdataWU <- data.frame(tuesday$Warm.Up, tuesday$label)
names(tuesdataWU) <- c("Warm.Up", "label")
weddataWU <- data.frame(wednesday$Warm.Up, wednesday$label)
names(weddataWU) <- c("Warm.Up", "label")
thursdataWU <- data.frame(thursday$Warm.Up, thursday$label)
names(thursdataWU) <- c("Warm.Up", "label")

histDataWU <- rbind(mondataWU, tuesdataWU, weddataWU, thursdataWU) #creates data frame with histogram data
histDataWU <- na.omit(histDataWU)
ggplot(histDataWU, aes(Warm.Up, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Morning Meeting##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00494954/M00494954_MM.png",width=1000, height=600) #Opens a PNG file#

mondataMM <- data.frame(monday$Morning.Meeting, monday$label)
names(mondataMM) <- c("Morning.Meeting", "label")
tuesdataMM <- data.frame(tuesday$Morning.Meeting, tuesday$label)
names(tuesdataMM) <- c("Morning.Meeting", "label")
weddataMM <- data.frame(wednesday$Morning.Meeting, wednesday$label)
names(weddataMM) <- c("Morning.Meeting", "label")
thursdataMM <- data.frame(thursday$Morning.Meeting, thursday$label)
names(thursdataMM) <- c("Morning.Meeting", "label")

histDataMM <- rbind(mondataMM, tuesdataMM, weddataMM, thursdataMM) #creates data frame with histogram data
histDataMM <- na.omit(histDataMM)
ggplot(histDataMM, aes(Morning.Meeting, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Lunch##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00494954/M00494954_Lunch.png",width=1000, height=600) #Opens a PNG file#

mondataL <- data.frame(monday$Lunch, monday$label)
names(mondataL) <- c("Lunch", "label")
tuesdataL <- data.frame(tuesday$Lunch, tuesday$label)
names(tuesdataL) <- c("Lunch", "label")
weddataL <- data.frame(wednesday$Lunch, wednesday$label)
names(weddataL) <- c("Lunch", "label")
thursdataL <- data.frame(thursday$Lunch, thursday$label)
names(thursdataL) <- c("Lunch", "label")

histDataL <- rbind(mondataL, tuesdataL, weddataL, thursdataL) #creates data frame with histogram data
histDataL <- na.omit(histDataL)
ggplot(histDataL, aes(Lunch, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Outside Play##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00494954/M00494954_OutsidePlay.png",width=1000, height=600) #Opens a PNG file#

mondataOP <- data.frame(monday$Outside.Play, monday$label)
names(mondataOP) <- c("Outside.Play", "label")
tuesdataOP <- data.frame(tuesday$Outside.Play, tuesday$label)
names(tuesdataOP) <- c("Outside.Play", "label")
weddataOP <- data.frame(wednesday$Outside.Play, wednesday$label)
names(weddataOP) <- c("Outside.Play", "label")
thursdataOP <- data.frame(thursday$Outside.Play, thursday$label)
names(thursdataOP) <- c("Outside.Play", "label")

histDataOP <- rbind(mondataOP, tuesdataOP, weddataOP, thursdataOP) #creates data frame with histogram data
histDataOP <- na.omit(histDataOP)
ggplot(histDataOP, aes(Outside.Play, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Prize Store##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00494954/M00494954_PrizeStore.png",width=1000, height=600) #Opens a PNG file#

mondataPS <- data.frame(monday$Prize.Store, monday$label)
names(mondataPS) <- c("Prize.Store", "label")
tuesdataPS <- data.frame(tuesday$Prize.Store, tuesday$label)
names(tuesdataPS) <- c("Prize.Store", "label")
weddataPS <- data.frame(wednesday$Prize.Store, wednesday$label)
names(weddataPS) <- c("Prize.Store", "label")
thursdataPS <- data.frame(thursday$Prize.Store, thursday$label)
names(thursdataPS) <- c("Prize.Store", "label")

histDataPS <- rbind(mondataPS, tuesdataPS, weddataPS, thursdataPS) #creates data frame with histogram data
histDataPS <- na.omit(histDataPS)
ggplot(histDataPS, aes(Prize.Store, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

rm(list=ls()) #resets Environment for the next subject
##M00402344

monday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00402344/Distributions/M00402344_monday.csv")

tuesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00402344/Distributions/M00402344_tuesday.csv")

wednesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00402344/Distributions/M00402344_wednesday.csv")

thursday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00402344/Distributions/M00402344_thursday.csv")

#creates a label column
monday$label <- 'mon'
tuesday$label <- 'tues'
wednesday$label <- 'wed'
thursday$label <- 'thurs'

###Full Day Plot###
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00402344/M00402344_Full.png",width=1000, height=600) #Opens a PNG file#

#create new data frames with just full day values and labels
mondata <- data.frame(monday$Full.Day, monday$label)
names(mondata) <- c("Full.Day", "label")
tuesdata <- data.frame(tuesday$Full.Day, tuesday$label)
names(tuesdata) <- c("Full.Day", "label")
weddata <- data.frame(wednesday$Full.Day, wednesday$label)
names(weddata) <- c("Full.Day", "label")
thursdata <- data.frame(thursday$Full.Day, thursday$label)
names(thursdata) <- c("Full.Day", "label")

histData <- rbind(mondata, tuesdata, weddata, thursdata) #creates data frame with histogram data

ggplot(histData, aes(Full.Day, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Warm Up##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00402344/M00402344_WarmUp.png",width=1000, height=600) #Opens a PNG file#

mondataWU <- data.frame(monday$Warm.Up, monday$label)
names(mondataWU) <- c("Warm.Up", "label")
tuesdataWU <- data.frame(tuesday$Warm.Up, tuesday$label)
names(tuesdataWU) <- c("Warm.Up", "label")
weddataWU <- data.frame(wednesday$Warm.Up, wednesday$label)
names(weddataWU) <- c("Warm.Up", "label")
thursdataWU <- data.frame(thursday$Warm.Up, thursday$label)
names(thursdataWU) <- c("Warm.Up", "label")

histDataWU <- rbind(mondataWU, tuesdataWU, weddataWU, thursdataWU) #creates data frame with histogram data
histDataWU <- na.omit(histDataWU)
ggplot(histDataWU, aes(Warm.Up, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Morning Meeting##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00402344/M00402344_MM.png",width=1000, height=600) #Opens a PNG file#

mondataMM <- data.frame(monday$Morning.Meeting, monday$label)
names(mondataMM) <- c("Morning.Meeting", "label")
tuesdataMM <- data.frame(tuesday$Morning.Meeting, tuesday$label)
names(tuesdataMM) <- c("Morning.Meeting", "label")
weddataMM <- data.frame(wednesday$Morning.Meeting, wednesday$label)
names(weddataMM) <- c("Morning.Meeting", "label")
thursdataMM <- data.frame(thursday$Morning.Meeting, thursday$label)
names(thursdataMM) <- c("Morning.Meeting", "label")

histDataMM <- rbind(mondataMM, tuesdataMM, weddataMM, thursdataMM) #creates data frame with histogram data
histDataMM <- na.omit(histDataMM)
ggplot(histDataMM, aes(Morning.Meeting, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Lunch##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00402344/M00402344_Lunch.png",width=1000, height=600) #Opens a PNG file#

mondataL <- data.frame(monday$Lunch, monday$label)
names(mondataL) <- c("Lunch", "label")
tuesdataL <- data.frame(tuesday$Lunch, tuesday$label)
names(tuesdataL) <- c("Lunch", "label")
weddataL <- data.frame(wednesday$Lunch, wednesday$label)
names(weddataL) <- c("Lunch", "label")
thursdataL <- data.frame(thursday$Lunch, thursday$label)
names(thursdataL) <- c("Lunch", "label")

histDataL <- rbind(mondataL, tuesdataL, weddataL, thursdataL) #creates data frame with histogram data
histDataL <- na.omit(histDataL)
ggplot(histDataL, aes(Lunch, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Outside Play##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00402344/M00402344_OutsidePlay.png",width=1000, height=600) #Opens a PNG file#

mondataOP <- data.frame(monday$Outside.Play, monday$label)
names(mondataOP) <- c("Outside.Play", "label")
tuesdataOP <- data.frame(tuesday$Outside.Play, tuesday$label)
names(tuesdataOP) <- c("Outside.Play", "label")
weddataOP <- data.frame(wednesday$Outside.Play, wednesday$label)
names(weddataOP) <- c("Outside.Play", "label")
thursdataOP <- data.frame(thursday$Outside.Play, thursday$label)
names(thursdataOP) <- c("Outside.Play", "label")

histDataOP <- rbind(mondataOP, tuesdataOP, weddataOP, thursdataOP) #creates data frame with histogram data
histDataOP <- na.omit(histDataOP)
ggplot(histDataOP, aes(Outside.Play, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Prize Store##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00402344/M00402344_PrizeStore.png",width=1000, height=600) #Opens a PNG file#

mondataPS <- data.frame(monday$Prize.Store, monday$label)
names(mondataPS) <- c("Prize.Store", "label")
tuesdataPS <- data.frame(tuesday$Prize.Store, tuesday$label)
names(tuesdataPS) <- c("Prize.Store", "label")
weddataPS <- data.frame(wednesday$Prize.Store, wednesday$label)
names(weddataPS) <- c("Prize.Store", "label")
thursdataPS <- data.frame(thursday$Prize.Store, thursday$label)
names(thursdataPS) <- c("Prize.Store", "label")

histDataPS <- rbind(mondataPS, tuesdataPS, weddataPS, thursdataPS) #creates data frame with histogram data
histDataPS <- na.omit(histDataPS)
ggplot(histDataPS, aes(Prize.Store, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()
rm(list=ls())

##M00412434

monday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00412434/Distributions/M00412434_monday.csv")

tuesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00412434/Distributions/M00412434_tuesday.csv")

wednesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00412434/Distributions/M00412434_wednesday.csv")

thursday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00412434/Distributions/M00412434_thursday.csv")

#creates a label column
monday$label <- 'mon'
tuesday$label <- 'tues'
wednesday$label <- 'wed'
thursday$label <- 'thurs'

###Full Day Plot###
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00412434/M00412434_Full.png",width=1000, height=600) #Opens a PNG file#

#create new data frames with just full day values and labels
mondata <- data.frame(monday$Full.Day, monday$label)
names(mondata) <- c("Full.Day", "label")
tuesdata <- data.frame(tuesday$Full.Day, tuesday$label)
names(tuesdata) <- c("Full.Day", "label")
weddata <- data.frame(wednesday$Full.Day, wednesday$label)
names(weddata) <- c("Full.Day", "label")
thursdata <- data.frame(thursday$Full.Day, thursday$label)
names(thursdata) <- c("Full.Day", "label")

histData <- rbind(mondata, tuesdata, weddata, thursdata) #creates data frame with histogram data

ggplot(histData, aes(Full.Day, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Warm Up##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00412434/M00412434_WarmUp.png",width=1000, height=600) #Opens a PNG file#

mondataWU <- data.frame(monday$Warm.Up, monday$label)
names(mondataWU) <- c("Warm.Up", "label")
tuesdataWU <- data.frame(tuesday$Warm.Up, tuesday$label)
names(tuesdataWU) <- c("Warm.Up", "label")
weddataWU <- data.frame(wednesday$Warm.Up, wednesday$label)
names(weddataWU) <- c("Warm.Up", "label")
thursdataWU <- data.frame(thursday$Warm.Up, thursday$label)
names(thursdataWU) <- c("Warm.Up", "label")

histDataWU <- rbind(mondataWU, tuesdataWU, weddataWU, thursdataWU) #creates data frame with histogram data
histDataWU <- na.omit(histDataWU)
ggplot(histDataWU, aes(Warm.Up, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Morning Meeting##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00412434/M00412434_MM.png",width=1000, height=600) #Opens a PNG file#

mondataMM <- data.frame(monday$Morning.Meeting, monday$label)
names(mondataMM) <- c("Morning.Meeting", "label")
tuesdataMM <- data.frame(tuesday$Morning.Meeting, tuesday$label)
names(tuesdataMM) <- c("Morning.Meeting", "label")
weddataMM <- data.frame(wednesday$Morning.Meeting, wednesday$label)
names(weddataMM) <- c("Morning.Meeting", "label")
thursdataMM <- data.frame(thursday$Morning.Meeting, thursday$label)
names(thursdataMM) <- c("Morning.Meeting", "label")

histDataMM <- rbind(mondataMM, tuesdataMM, weddataMM, thursdataMM) #creates data frame with histogram data
histDataMM <- na.omit(histDataMM)
ggplot(histDataMM, aes(Morning.Meeting, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Lunch##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00412434/M00412434_Lunch.png",width=1000, height=600) #Opens a PNG file#

mondataL <- data.frame(monday$Lunch, monday$label)
names(mondataL) <- c("Lunch", "label")
tuesdataL <- data.frame(tuesday$Lunch, tuesday$label)
names(tuesdataL) <- c("Lunch", "label")
weddataL <- data.frame(wednesday$Lunch, wednesday$label)
names(weddataL) <- c("Lunch", "label")
thursdataL <- data.frame(thursday$Lunch, thursday$label)
names(thursdataL) <- c("Lunch", "label")

histDataL <- rbind(mondataL, tuesdataL, weddataL, thursdataL) #creates data frame with histogram data
histDataL <- na.omit(histDataL)
ggplot(histDataL, aes(Lunch, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Outside Play##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00412434/M00412434_OutsidePlay.png",width=1000, height=600) #Opens a PNG file#

mondataOP <- data.frame(monday$Outside.Play, monday$label)
names(mondataOP) <- c("Outside.Play", "label")
tuesdataOP <- data.frame(tuesday$Outside.Play, tuesday$label)
names(tuesdataOP) <- c("Outside.Play", "label")
weddataOP <- data.frame(wednesday$Outside.Play, wednesday$label)
names(weddataOP) <- c("Outside.Play", "label")
thursdataOP <- data.frame(thursday$Outside.Play, thursday$label)
names(thursdataOP) <- c("Outside.Play", "label")

histDataOP <- rbind(mondataOP, tuesdataOP, weddataOP, thursdataOP) #creates data frame with histogram data
histDataOP <- na.omit(histDataOP)
ggplot(histDataOP, aes(Outside.Play, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Prize Store##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00412434/M00412434_PrizeStore.png",width=1000, height=600) #Opens a PNG file#

mondataPS <- data.frame(monday$Prize.Store, monday$label)
names(mondataPS) <- c("Prize.Store", "label")
tuesdataPS <- data.frame(tuesday$Prize.Store, tuesday$label)
names(tuesdataPS) <- c("Prize.Store", "label")
weddataPS <- data.frame(wednesday$Prize.Store, wednesday$label)
names(weddataPS) <- c("Prize.Store", "label")
thursdataPS <- data.frame(thursday$Prize.Store, thursday$label)
names(thursdataPS) <- c("Prize.Store", "label")

histDataPS <- rbind(mondataPS, tuesdataPS, weddataPS, thursdataPS) #creates data frame with histogram data
histDataPS <- na.omit(histDataPS)
ggplot(histDataPS, aes(Prize.Store, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()
rm(list=ls())

##M00413464

monday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00413464/Distributions/M00413464_monday.csv")

tuesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00413464/Distributions/M00413464_tuesday.csv")

wednesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00413464/Distributions/M00413464_wednesday.csv")

thursday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00413464/Distributions/M00413464_thursday.csv")

#creates a label column
monday$label <- 'mon'
tuesday$label <- 'tues'
wednesday$label <- 'wed'
thursday$label <- 'thurs'

###Full Day Plot###
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00413464/M00413464_Full.png",width=1000, height=600) #Opens a PNG file#

#create new data frames with just full day values and labels
mondata <- data.frame(monday$Full.Day, monday$label)
names(mondata) <- c("Full.Day", "label")
tuesdata <- data.frame(tuesday$Full.Day, tuesday$label)
names(tuesdata) <- c("Full.Day", "label")
weddata <- data.frame(wednesday$Full.Day, wednesday$label)
names(weddata) <- c("Full.Day", "label")
thursdata <- data.frame(thursday$Full.Day, thursday$label)
names(thursdata) <- c("Full.Day", "label")

histData <- rbind(mondata, tuesdata, weddata, thursdata) #creates data frame with histogram data

ggplot(histData, aes(Full.Day, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Warm Up##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00413464/M00413464_WarmUp.png",width=1000, height=600) #Opens a PNG file#

mondataWU <- data.frame(monday$Warm.Up, monday$label)
names(mondataWU) <- c("Warm.Up", "label")
tuesdataWU <- data.frame(tuesday$Warm.Up, tuesday$label)
names(tuesdataWU) <- c("Warm.Up", "label")
weddataWU <- data.frame(wednesday$Warm.Up, wednesday$label)
names(weddataWU) <- c("Warm.Up", "label")
thursdataWU <- data.frame(thursday$Warm.Up, thursday$label)
names(thursdataWU) <- c("Warm.Up", "label")

histDataWU <- rbind(mondataWU, tuesdataWU, weddataWU, thursdataWU) #creates data frame with histogram data
histDataWU <- na.omit(histDataWU)
ggplot(histDataWU, aes(Warm.Up, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Morning Meeting##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00413464/M00413464_MM.png",width=1000, height=600) #Opens a PNG file#

mondataMM <- data.frame(monday$Morning.Meeting, monday$label)
names(mondataMM) <- c("Morning.Meeting", "label")
tuesdataMM <- data.frame(tuesday$Morning.Meeting, tuesday$label)
names(tuesdataMM) <- c("Morning.Meeting", "label")
weddataMM <- data.frame(wednesday$Morning.Meeting, wednesday$label)
names(weddataMM) <- c("Morning.Meeting", "label")
thursdataMM <- data.frame(thursday$Morning.Meeting, thursday$label)
names(thursdataMM) <- c("Morning.Meeting", "label")

histDataMM <- rbind(mondataMM, tuesdataMM, weddataMM, thursdataMM) #creates data frame with histogram data
histDataMM <- na.omit(histDataMM)
ggplot(histDataMM, aes(Morning.Meeting, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Lunch##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00413464/M00413464_Lunch.png",width=1000, height=600) #Opens a PNG file#

mondataL <- data.frame(monday$Lunch, monday$label)
names(mondataL) <- c("Lunch", "label")
tuesdataL <- data.frame(tuesday$Lunch, tuesday$label)
names(tuesdataL) <- c("Lunch", "label")
weddataL <- data.frame(wednesday$Lunch, wednesday$label)
names(weddataL) <- c("Lunch", "label")
thursdataL <- data.frame(thursday$Lunch, thursday$label)
names(thursdataL) <- c("Lunch", "label")

histDataL <- rbind(mondataL, tuesdataL, weddataL, thursdataL) #creates data frame with histogram data
histDataL <- na.omit(histDataL)
ggplot(histDataL, aes(Lunch, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Outside Play##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00413464/M00413464_OutsidePlay.png",width=1000, height=600) #Opens a PNG file#

mondataOP <- data.frame(monday$Outside.Play, monday$label)
names(mondataOP) <- c("Outside.Play", "label")
tuesdataOP <- data.frame(tuesday$Outside.Play, tuesday$label)
names(tuesdataOP) <- c("Outside.Play", "label")
weddataOP <- data.frame(wednesday$Outside.Play, wednesday$label)
names(weddataOP) <- c("Outside.Play", "label")
thursdataOP <- data.frame(thursday$Outside.Play, thursday$label)
names(thursdataOP) <- c("Outside.Play", "label")

histDataOP <- rbind(mondataOP, tuesdataOP, weddataOP, thursdataOP) #creates data frame with histogram data
histDataOP <- na.omit(histDataOP)
ggplot(histDataOP, aes(Outside.Play, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Prize Store##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00413464/M00413464_PrizeStore.png",width=1000, height=600) #Opens a PNG file#

mondataPS <- data.frame(monday$Prize.Store, monday$label)
names(mondataPS) <- c("Prize.Store", "label")
tuesdataPS <- data.frame(tuesday$Prize.Store, tuesday$label)
names(tuesdataPS) <- c("Prize.Store", "label")
weddataPS <- data.frame(wednesday$Prize.Store, wednesday$label)
names(weddataPS) <- c("Prize.Store", "label")
thursdataPS <- data.frame(thursday$Prize.Store, thursday$label)
names(thursdataPS) <- c("Prize.Store", "label")

histDataPS <- rbind(mondataPS, tuesdataPS, weddataPS, thursdataPS) #creates data frame with histogram data
histDataPS <- na.omit(histDataPS)
ggplot(histDataPS, aes(Prize.Store, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()
rm(list=ls())

##M00440011

monday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00440011/Distributions/M00440011_monday.csv")

tuesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00440011/Distributions/M00440011_tuesday.csv")

wednesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00440011/Distributions/M00440011_wednesday.csv")

thursday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00440011/Distributions/M00440011_thursday.csv")

#creates a label column
monday$label <- 'mon'
tuesday$label <- 'tues'
wednesday$label <- 'wed'
thursday$label <- 'thurs'

###Full Day Plot###
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00440011/M00440011_Full.png",width=1000, height=600) #Opens a PNG file#

#create new data frames with just full day values and labels
mondata <- data.frame(monday$Full.Day, monday$label)
names(mondata) <- c("Full.Day", "label")
tuesdata <- data.frame(tuesday$Full.Day, tuesday$label)
names(tuesdata) <- c("Full.Day", "label")
weddata <- data.frame(wednesday$Full.Day, wednesday$label)
names(weddata) <- c("Full.Day", "label")
thursdata <- data.frame(thursday$Full.Day, thursday$label)
names(thursdata) <- c("Full.Day", "label")

histData <- rbind(mondata, tuesdata, weddata, thursdata) #creates data frame with histogram data

ggplot(histData, aes(Full.Day, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Warm Up##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00440011/M00440011_WarmUp.png",width=1000, height=600) #Opens a PNG file#

mondataWU <- data.frame(monday$Warm.Up, monday$label)
names(mondataWU) <- c("Warm.Up", "label")
tuesdataWU <- data.frame(tuesday$Warm.Up, tuesday$label)
names(tuesdataWU) <- c("Warm.Up", "label")
weddataWU <- data.frame(wednesday$Warm.Up, wednesday$label)
names(weddataWU) <- c("Warm.Up", "label")
thursdataWU <- data.frame(thursday$Warm.Up, thursday$label)
names(thursdataWU) <- c("Warm.Up", "label")

histDataWU <- rbind(mondataWU, tuesdataWU, weddataWU, thursdataWU) #creates data frame with histogram data
histDataWU <- na.omit(histDataWU)
ggplot(histDataWU, aes(Warm.Up, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Morning Meeting##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00440011/M00440011_MM.png",width=1000, height=600) #Opens a PNG file#

mondataMM <- data.frame(monday$Morning.Meeting, monday$label)
names(mondataMM) <- c("Morning.Meeting", "label")
tuesdataMM <- data.frame(tuesday$Morning.Meeting, tuesday$label)
names(tuesdataMM) <- c("Morning.Meeting", "label")
weddataMM <- data.frame(wednesday$Morning.Meeting, wednesday$label)
names(weddataMM) <- c("Morning.Meeting", "label")
thursdataMM <- data.frame(thursday$Morning.Meeting, thursday$label)
names(thursdataMM) <- c("Morning.Meeting", "label")

histDataMM <- rbind(mondataMM, tuesdataMM, weddataMM, thursdataMM) #creates data frame with histogram data
histDataMM <- na.omit(histDataMM)
ggplot(histDataMM, aes(Morning.Meeting, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Lunch##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00440011/M00440011_Lunch.png",width=1000, height=600) #Opens a PNG file#

mondataL <- data.frame(monday$Lunch, monday$label)
names(mondataL) <- c("Lunch", "label")
tuesdataL <- data.frame(tuesday$Lunch, tuesday$label)
names(tuesdataL) <- c("Lunch", "label")
weddataL <- data.frame(wednesday$Lunch, wednesday$label)
names(weddataL) <- c("Lunch", "label")
thursdataL <- data.frame(thursday$Lunch, thursday$label)
names(thursdataL) <- c("Lunch", "label")

histDataL <- rbind(mondataL, tuesdataL, weddataL, thursdataL) #creates data frame with histogram data
histDataL <- na.omit(histDataL)
ggplot(histDataL, aes(Lunch, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Outside Play##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00440011/M00440011_OutsidePlay.png",width=1000, height=600) #Opens a PNG file#

mondataOP <- data.frame(monday$Outside.Play, monday$label)
names(mondataOP) <- c("Outside.Play", "label")
tuesdataOP <- data.frame(tuesday$Outside.Play, tuesday$label)
names(tuesdataOP) <- c("Outside.Play", "label")
weddataOP <- data.frame(wednesday$Outside.Play, wednesday$label)
names(weddataOP) <- c("Outside.Play", "label")
thursdataOP <- data.frame(thursday$Outside.Play, thursday$label)
names(thursdataOP) <- c("Outside.Play", "label")

histDataOP <- rbind(mondataOP, tuesdataOP, weddataOP, thursdataOP) #creates data frame with histogram data
histDataOP <- na.omit(histDataOP)
ggplot(histDataOP, aes(Outside.Play, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Prize Store##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00440011/M00440011_PrizeStore.png",width=1000, height=600) #Opens a PNG file#

mondataPS <- data.frame(monday$Prize.Store, monday$label)
names(mondataPS) <- c("Prize.Store", "label")
tuesdataPS <- data.frame(tuesday$Prize.Store, tuesday$label)
names(tuesdataPS) <- c("Prize.Store", "label")
weddataPS <- data.frame(wednesday$Prize.Store, wednesday$label)
names(weddataPS) <- c("Prize.Store", "label")
thursdataPS <- data.frame(thursday$Prize.Store, thursday$label)
names(thursdataPS) <- c("Prize.Store", "label")

histDataPS <- rbind(mondataPS, tuesdataPS, weddataPS, thursdataPS) #creates data frame with histogram data
histDataPS <- na.omit(histDataPS)
ggplot(histDataPS, aes(Prize.Store, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()
rm(list=ls())

##M00441664

monday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00441664/Distributions/M00441664_monday.csv")

tuesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00441664/Distributions/M00441664_tuesday.csv")

wednesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00441664/Distributions/M00441664_wednesday.csv")

thursday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00441664/Distributions/M00441664_thursday.csv")

#creates a label column
monday$label <- 'mon'
tuesday$label <- 'tues'
wednesday$label <- 'wed'
thursday$label <- 'thurs'

###Full Day Plot###
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00441664/M00441664_Full.png",width=1000, height=600) #Opens a PNG file#

#create new data frames with just full day values and labels
mondata <- data.frame(monday$Full.Day, monday$label)
names(mondata) <- c("Full.Day", "label")
tuesdata <- data.frame(tuesday$Full.Day, tuesday$label)
names(tuesdata) <- c("Full.Day", "label")
weddata <- data.frame(wednesday$Full.Day, wednesday$label)
names(weddata) <- c("Full.Day", "label")
thursdata <- data.frame(thursday$Full.Day, thursday$label)
names(thursdata) <- c("Full.Day", "label")

histData <- rbind(mondata, tuesdata, weddata, thursdata) #creates data frame with histogram data

ggplot(histData, aes(Full.Day, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Warm Up##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00441664/M00441664_WarmUp.png",width=1000, height=600) #Opens a PNG file#

mondataWU <- data.frame(monday$Warm.Up, monday$label)
names(mondataWU) <- c("Warm.Up", "label")
tuesdataWU <- data.frame(tuesday$Warm.Up, tuesday$label)
names(tuesdataWU) <- c("Warm.Up", "label")
weddataWU <- data.frame(wednesday$Warm.Up, wednesday$label)
names(weddataWU) <- c("Warm.Up", "label")
thursdataWU <- data.frame(thursday$Warm.Up, thursday$label)
names(thursdataWU) <- c("Warm.Up", "label")

histDataWU <- rbind(mondataWU, tuesdataWU, weddataWU, thursdataWU) #creates data frame with histogram data
histDataWU <- na.omit(histDataWU)
ggplot(histDataWU, aes(Warm.Up, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Morning Meeting##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00441664/M00441664_MM.png",width=1000, height=600) #Opens a PNG file#

mondataMM <- data.frame(monday$Morning.Meeting, monday$label)
names(mondataMM) <- c("Morning.Meeting", "label")
tuesdataMM <- data.frame(tuesday$Morning.Meeting, tuesday$label)
names(tuesdataMM) <- c("Morning.Meeting", "label")
weddataMM <- data.frame(wednesday$Morning.Meeting, wednesday$label)
names(weddataMM) <- c("Morning.Meeting", "label")
thursdataMM <- data.frame(thursday$Morning.Meeting, thursday$label)
names(thursdataMM) <- c("Morning.Meeting", "label")

histDataMM <- rbind(mondataMM, tuesdataMM, weddataMM, thursdataMM) #creates data frame with histogram data
histDataMM <- na.omit(histDataMM)
ggplot(histDataMM, aes(Morning.Meeting, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Lunch##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00441664/M00441664_Lunch.png",width=1000, height=600) #Opens a PNG file#

mondataL <- data.frame(monday$Lunch, monday$label)
names(mondataL) <- c("Lunch", "label")
tuesdataL <- data.frame(tuesday$Lunch, tuesday$label)
names(tuesdataL) <- c("Lunch", "label")
weddataL <- data.frame(wednesday$Lunch, wednesday$label)
names(weddataL) <- c("Lunch", "label")
thursdataL <- data.frame(thursday$Lunch, thursday$label)
names(thursdataL) <- c("Lunch", "label")

histDataL <- rbind(mondataL, tuesdataL, weddataL, thursdataL) #creates data frame with histogram data
histDataL <- na.omit(histDataL)
ggplot(histDataL, aes(Lunch, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Outside Play##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00441664/M00441664_OutsidePlay.png",width=1000, height=600) #Opens a PNG file#

mondataOP <- data.frame(monday$Outside.Play, monday$label)
names(mondataOP) <- c("Outside.Play", "label")
tuesdataOP <- data.frame(tuesday$Outside.Play, tuesday$label)
names(tuesdataOP) <- c("Outside.Play", "label")
weddataOP <- data.frame(wednesday$Outside.Play, wednesday$label)
names(weddataOP) <- c("Outside.Play", "label")
thursdataOP <- data.frame(thursday$Outside.Play, thursday$label)
names(thursdataOP) <- c("Outside.Play", "label")

histDataOP <- rbind(mondataOP, tuesdataOP, weddataOP, thursdataOP) #creates data frame with histogram data
histDataOP <- na.omit(histDataOP)
ggplot(histDataOP, aes(Outside.Play, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Prize Store##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00441664/M00441664_PrizeStore.png",width=1000, height=600) #Opens a PNG file#

mondataPS <- data.frame(monday$Prize.Store, monday$label)
names(mondataPS) <- c("Prize.Store", "label")
tuesdataPS <- data.frame(tuesday$Prize.Store, tuesday$label)
names(tuesdataPS) <- c("Prize.Store", "label")
weddataPS <- data.frame(wednesday$Prize.Store, wednesday$label)
names(weddataPS) <- c("Prize.Store", "label")
thursdataPS <- data.frame(thursday$Prize.Store, thursday$label)
names(thursdataPS) <- c("Prize.Store", "label")

histDataPS <- rbind(mondataPS, tuesdataPS, weddataPS, thursdataPS) #creates data frame with histogram data
histDataPS <- na.omit(histDataPS)
ggplot(histDataPS, aes(Prize.Store, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()
rm(list=ls())


##M00445929

monday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00445929/Distributions/M00445929_monday.csv")

tuesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00445929/Distributions/M00445929_tuesday.csv")

wednesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00445929/Distributions/M00445929_wednesday.csv")

thursday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00445929/Distributions/M00445929_thursday.csv")

#creates a label column
monday$label <- 'mon'
tuesday$label <- 'tues'
wednesday$label <- 'wed'
thursday$label <- 'thurs'

###Full Day Plot###
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00445929/M00445929_Full.png",width=1000, height=600) #Opens a PNG file#

#create new data frames with just full day values and labels
mondata <- data.frame(monday$Full.Day, monday$label)
names(mondata) <- c("Full.Day", "label")
tuesdata <- data.frame(tuesday$Full.Day, tuesday$label)
names(tuesdata) <- c("Full.Day", "label")
weddata <- data.frame(wednesday$Full.Day, wednesday$label)
names(weddata) <- c("Full.Day", "label")
thursdata <- data.frame(thursday$Full.Day, thursday$label)
names(thursdata) <- c("Full.Day", "label")

histData <- rbind(mondata, tuesdata, weddata, thursdata) #creates data frame with histogram data

ggplot(histData, aes(Full.Day, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Warm Up##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00445929/M00445929_WarmUp.png",width=1000, height=600) #Opens a PNG file#

mondataWU <- data.frame(monday$Warm.Up, monday$label)
names(mondataWU) <- c("Warm.Up", "label")
tuesdataWU <- data.frame(tuesday$Warm.Up, tuesday$label)
names(tuesdataWU) <- c("Warm.Up", "label")
weddataWU <- data.frame(wednesday$Warm.Up, wednesday$label)
names(weddataWU) <- c("Warm.Up", "label")
thursdataWU <- data.frame(thursday$Warm.Up, thursday$label)
names(thursdataWU) <- c("Warm.Up", "label")

histDataWU <- rbind(mondataWU, tuesdataWU, weddataWU, thursdataWU) #creates data frame with histogram data
histDataWU <- na.omit(histDataWU)
ggplot(histDataWU, aes(Warm.Up, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Morning Meeting##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00445929/M00445929_MM.png",width=1000, height=600) #Opens a PNG file#

mondataMM <- data.frame(monday$Morning.Meeting, monday$label)
names(mondataMM) <- c("Morning.Meeting", "label")
tuesdataMM <- data.frame(tuesday$Morning.Meeting, tuesday$label)
names(tuesdataMM) <- c("Morning.Meeting", "label")
weddataMM <- data.frame(wednesday$Morning.Meeting, wednesday$label)
names(weddataMM) <- c("Morning.Meeting", "label")
thursdataMM <- data.frame(thursday$Morning.Meeting, thursday$label)
names(thursdataMM) <- c("Morning.Meeting", "label")

histDataMM <- rbind(mondataMM, tuesdataMM, weddataMM, thursdataMM) #creates data frame with histogram data
histDataMM <- na.omit(histDataMM)
ggplot(histDataMM, aes(Morning.Meeting, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Lunch##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00445929/M00445929_Lunch.png",width=1000, height=600) #Opens a PNG file#

mondataL <- data.frame(monday$Lunch, monday$label)
names(mondataL) <- c("Lunch", "label")
tuesdataL <- data.frame(tuesday$Lunch, tuesday$label)
names(tuesdataL) <- c("Lunch", "label")
weddataL <- data.frame(wednesday$Lunch, wednesday$label)
names(weddataL) <- c("Lunch", "label")
thursdataL <- data.frame(thursday$Lunch, thursday$label)
names(thursdataL) <- c("Lunch", "label")

histDataL <- rbind(mondataL, tuesdataL, weddataL, thursdataL) #creates data frame with histogram data
histDataL <- na.omit(histDataL)
ggplot(histDataL, aes(Lunch, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Outside Play##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00445929/M00445929_OutsidePlay.png",width=1000, height=600) #Opens a PNG file#

mondataOP <- data.frame(monday$Outside.Play, monday$label)
names(mondataOP) <- c("Outside.Play", "label")
tuesdataOP <- data.frame(tuesday$Outside.Play, tuesday$label)
names(tuesdataOP) <- c("Outside.Play", "label")
weddataOP <- data.frame(wednesday$Outside.Play, wednesday$label)
names(weddataOP) <- c("Outside.Play", "label")
thursdataOP <- data.frame(thursday$Outside.Play, thursday$label)
names(thursdataOP) <- c("Outside.Play", "label")

histDataOP <- rbind(mondataOP, tuesdataOP, weddataOP, thursdataOP) #creates data frame with histogram data
histDataOP <- na.omit(histDataOP)
ggplot(histDataOP, aes(Outside.Play, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Prize Store##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00445929/M00445929_PrizeStore.png",width=1000, height=600) #Opens a PNG file#

mondataPS <- data.frame(monday$Prize.Store, monday$label)
names(mondataPS) <- c("Prize.Store", "label")
tuesdataPS <- data.frame(tuesday$Prize.Store, tuesday$label)
names(tuesdataPS) <- c("Prize.Store", "label")
weddataPS <- data.frame(wednesday$Prize.Store, wednesday$label)
names(weddataPS) <- c("Prize.Store", "label")
thursdataPS <- data.frame(thursday$Prize.Store, thursday$label)
names(thursdataPS) <- c("Prize.Store", "label")

histDataPS <- rbind(mondataPS, tuesdataPS, weddataPS, thursdataPS) #creates data frame with histogram data
histDataPS <- na.omit(histDataPS)
ggplot(histDataPS, aes(Prize.Store, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()
rm(list=ls())

##M00447059

monday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00447059/Distributions/M00447059_monday.csv")

tuesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00447059/Distributions/M00447059_tuesday.csv")

wednesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00447059/Distributions/M00447059_wednesday.csv")

thursday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00447059/Distributions/M00447059_thursday.csv")

#creates a label column
monday$label <- 'mon'
tuesday$label <- 'tues'
wednesday$label <- 'wed'
thursday$label <- 'thurs'

###Full Day Plot###
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00447059/M00447059_Full.png",width=1000, height=600) #Opens a PNG file#

#create new data frames with just full day values and labels
mondata <- data.frame(monday$Full.Day, monday$label)
names(mondata) <- c("Full.Day", "label")
tuesdata <- data.frame(tuesday$Full.Day, tuesday$label)
names(tuesdata) <- c("Full.Day", "label")
weddata <- data.frame(wednesday$Full.Day, wednesday$label)
names(weddata) <- c("Full.Day", "label")
thursdata <- data.frame(thursday$Full.Day, thursday$label)
names(thursdata) <- c("Full.Day", "label")

histData <- rbind(mondata, tuesdata, weddata, thursdata) #creates data frame with histogram data

ggplot(histData, aes(Full.Day, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Warm Up##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00447059/M00447059_WarmUp.png",width=1000, height=600) #Opens a PNG file#

mondataWU <- data.frame(monday$Warm.Up, monday$label)
names(mondataWU) <- c("Warm.Up", "label")
tuesdataWU <- data.frame(tuesday$Warm.Up, tuesday$label)
names(tuesdataWU) <- c("Warm.Up", "label")
weddataWU <- data.frame(wednesday$Warm.Up, wednesday$label)
names(weddataWU) <- c("Warm.Up", "label")
thursdataWU <- data.frame(thursday$Warm.Up, thursday$label)
names(thursdataWU) <- c("Warm.Up", "label")

histDataWU <- rbind(mondataWU, tuesdataWU, weddataWU, thursdataWU) #creates data frame with histogram data
histDataWU <- na.omit(histDataWU)
ggplot(histDataWU, aes(Warm.Up, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Morning Meeting##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00447059/M00447059_MM.png",width=1000, height=600) #Opens a PNG file#

mondataMM <- data.frame(monday$Morning.Meeting, monday$label)
names(mondataMM) <- c("Morning.Meeting", "label")
tuesdataMM <- data.frame(tuesday$Morning.Meeting, tuesday$label)
names(tuesdataMM) <- c("Morning.Meeting", "label")
weddataMM <- data.frame(wednesday$Morning.Meeting, wednesday$label)
names(weddataMM) <- c("Morning.Meeting", "label")
thursdataMM <- data.frame(thursday$Morning.Meeting, thursday$label)
names(thursdataMM) <- c("Morning.Meeting", "label")

histDataMM <- rbind(mondataMM, tuesdataMM, weddataMM, thursdataMM) #creates data frame with histogram data
histDataMM <- na.omit(histDataMM)
ggplot(histDataMM, aes(Morning.Meeting, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Lunch##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00447059/M00447059_Lunch.png",width=1000, height=600) #Opens a PNG file#

mondataL <- data.frame(monday$Lunch, monday$label)
names(mondataL) <- c("Lunch", "label")
tuesdataL <- data.frame(tuesday$Lunch, tuesday$label)
names(tuesdataL) <- c("Lunch", "label")
weddataL <- data.frame(wednesday$Lunch, wednesday$label)
names(weddataL) <- c("Lunch", "label")
thursdataL <- data.frame(thursday$Lunch, thursday$label)
names(thursdataL) <- c("Lunch", "label")

histDataL <- rbind(mondataL, tuesdataL, weddataL, thursdataL) #creates data frame with histogram data
histDataL <- na.omit(histDataL)
ggplot(histDataL, aes(Lunch, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Outside Play##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00447059/M00447059_OutsidePlay.png",width=1000, height=600) #Opens a PNG file#

mondataOP <- data.frame(monday$Outside.Play, monday$label)
names(mondataOP) <- c("Outside.Play", "label")
tuesdataOP <- data.frame(tuesday$Outside.Play, tuesday$label)
names(tuesdataOP) <- c("Outside.Play", "label")
weddataOP <- data.frame(wednesday$Outside.Play, wednesday$label)
names(weddataOP) <- c("Outside.Play", "label")
thursdataOP <- data.frame(thursday$Outside.Play, thursday$label)
names(thursdataOP) <- c("Outside.Play", "label")

histDataOP <- rbind(mondataOP, tuesdataOP, weddataOP, thursdataOP) #creates data frame with histogram data
histDataOP <- na.omit(histDataOP)
ggplot(histDataOP, aes(Outside.Play, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Prize Store##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00447059/M00447059_PrizeStore.png",width=1000, height=600) #Opens a PNG file#

mondataPS <- data.frame(monday$Prize.Store, monday$label)
names(mondataPS) <- c("Prize.Store", "label")
tuesdataPS <- data.frame(tuesday$Prize.Store, tuesday$label)
names(tuesdataPS) <- c("Prize.Store", "label")
weddataPS <- data.frame(wednesday$Prize.Store, wednesday$label)
names(weddataPS) <- c("Prize.Store", "label")
thursdataPS <- data.frame(thursday$Prize.Store, thursday$label)
names(thursdataPS) <- c("Prize.Store", "label")

histDataPS <- rbind(mondataPS, tuesdataPS, weddataPS, thursdataPS) #creates data frame with histogram data
histDataPS <- na.omit(histDataPS)
ggplot(histDataPS, aes(Prize.Store, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()
rm(list=ls())

##M00472399

monday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00472399/Distributions/M00472399_monday.csv")

tuesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00472399/Distributions/M00472399_tuesday.csv")

wednesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00472399/Distributions/M00472399_wednesday.csv")

thursday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00472399/Distributions/M00472399_thursday.csv")

#creates a label column
monday$label <- 'mon'
tuesday$label <- 'tues'
wednesday$label <- 'wed'
thursday$label <- 'thurs'

###Full Day Plot###
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00472399/M00472399_Full.png",width=1000, height=600) #Opens a PNG file#

#create new data frames with just full day values and labels
mondata <- data.frame(monday$Full.Day, monday$label)
names(mondata) <- c("Full.Day", "label")
tuesdata <- data.frame(tuesday$Full.Day, tuesday$label)
names(tuesdata) <- c("Full.Day", "label")
weddata <- data.frame(wednesday$Full.Day, wednesday$label)
names(weddata) <- c("Full.Day", "label")
thursdata <- data.frame(thursday$Full.Day, thursday$label)
names(thursdata) <- c("Full.Day", "label")

histData <- rbind(mondata, tuesdata, weddata, thursdata) #creates data frame with histogram data

ggplot(histData, aes(Full.Day, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Warm Up##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00472399/M00472399_WarmUp.png",width=1000, height=600) #Opens a PNG file#

mondataWU <- data.frame(monday$Warm.Up, monday$label)
names(mondataWU) <- c("Warm.Up", "label")
tuesdataWU <- data.frame(tuesday$Warm.Up, tuesday$label)
names(tuesdataWU) <- c("Warm.Up", "label")
weddataWU <- data.frame(wednesday$Warm.Up, wednesday$label)
names(weddataWU) <- c("Warm.Up", "label")
thursdataWU <- data.frame(thursday$Warm.Up, thursday$label)
names(thursdataWU) <- c("Warm.Up", "label")

histDataWU <- rbind(mondataWU, tuesdataWU, weddataWU, thursdataWU) #creates data frame with histogram data
histDataWU <- na.omit(histDataWU)
ggplot(histDataWU, aes(Warm.Up, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Morning Meeting##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00472399/M00472399_MM.png",width=1000, height=600) #Opens a PNG file#

mondataMM <- data.frame(monday$Morning.Meeting, monday$label)
names(mondataMM) <- c("Morning.Meeting", "label")
tuesdataMM <- data.frame(tuesday$Morning.Meeting, tuesday$label)
names(tuesdataMM) <- c("Morning.Meeting", "label")
weddataMM <- data.frame(wednesday$Morning.Meeting, wednesday$label)
names(weddataMM) <- c("Morning.Meeting", "label")
thursdataMM <- data.frame(thursday$Morning.Meeting, thursday$label)
names(thursdataMM) <- c("Morning.Meeting", "label")

histDataMM <- rbind(mondataMM, tuesdataMM, weddataMM, thursdataMM) #creates data frame with histogram data
histDataMM <- na.omit(histDataMM)
ggplot(histDataMM, aes(Morning.Meeting, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Lunch##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00472399/M00472399_Lunch.png",width=1000, height=600) #Opens a PNG file#

mondataL <- data.frame(monday$Lunch, monday$label)
names(mondataL) <- c("Lunch", "label")
tuesdataL <- data.frame(tuesday$Lunch, tuesday$label)
names(tuesdataL) <- c("Lunch", "label")
weddataL <- data.frame(wednesday$Lunch, wednesday$label)
names(weddataL) <- c("Lunch", "label")
thursdataL <- data.frame(thursday$Lunch, thursday$label)
names(thursdataL) <- c("Lunch", "label")

histDataL <- rbind(mondataL, tuesdataL, weddataL, thursdataL) #creates data frame with histogram data
histDataL <- na.omit(histDataL)
ggplot(histDataL, aes(Lunch, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Outside Play##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00472399/M00472399_OutsidePlay.png",width=1000, height=600) #Opens a PNG file#

mondataOP <- data.frame(monday$Outside.Play, monday$label)
names(mondataOP) <- c("Outside.Play", "label")
tuesdataOP <- data.frame(tuesday$Outside.Play, tuesday$label)
names(tuesdataOP) <- c("Outside.Play", "label")
weddataOP <- data.frame(wednesday$Outside.Play, wednesday$label)
names(weddataOP) <- c("Outside.Play", "label")
thursdataOP <- data.frame(thursday$Outside.Play, thursday$label)
names(thursdataOP) <- c("Outside.Play", "label")

histDataOP <- rbind(mondataOP, tuesdataOP, weddataOP, thursdataOP) #creates data frame with histogram data
histDataOP <- na.omit(histDataOP)
ggplot(histDataOP, aes(Outside.Play, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Prize Store##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00472399/M00472399_PrizeStore.png",width=1000, height=600) #Opens a PNG file#

mondataPS <- data.frame(monday$Prize.Store, monday$label)
names(mondataPS) <- c("Prize.Store", "label")
tuesdataPS <- data.frame(tuesday$Prize.Store, tuesday$label)
names(tuesdataPS) <- c("Prize.Store", "label")
weddataPS <- data.frame(wednesday$Prize.Store, wednesday$label)
names(weddataPS) <- c("Prize.Store", "label")
thursdataPS <- data.frame(thursday$Prize.Store, thursday$label)
names(thursdataPS) <- c("Prize.Store", "label")

histDataPS <- rbind(mondataPS, tuesdataPS, weddataPS, thursdataPS) #creates data frame with histogram data
histDataPS <- na.omit(histDataPS)
ggplot(histDataPS, aes(Prize.Store, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()
rm(list=ls())


##M00475465

monday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00475465/Distributions/M00475465_monday.csv")

tuesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00475465/Distributions/M00475465_tuesday.csv")

wednesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00475465/Distributions/M00475465_wednesday.csv")

thursday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00475465/Distributions/M00475465_thursday.csv")

#creates a label column
monday$label <- 'mon'
tuesday$label <- 'tues'
wednesday$label <- 'wed'
thursday$label <- 'thurs'

###Full Day Plot###
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00475465/M00475465_Full.png",width=1000, height=600) #Opens a PNG file#

#create new data frames with just full day values and labels
mondata <- data.frame(monday$Full.Day, monday$label)
names(mondata) <- c("Full.Day", "label")
tuesdata <- data.frame(tuesday$Full.Day, tuesday$label)
names(tuesdata) <- c("Full.Day", "label")
weddata <- data.frame(wednesday$Full.Day, wednesday$label)
names(weddata) <- c("Full.Day", "label")
thursdata <- data.frame(thursday$Full.Day, thursday$label)
names(thursdata) <- c("Full.Day", "label")

histData <- rbind(mondata, tuesdata, weddata, thursdata) #creates data frame with histogram data

ggplot(histData, aes(Full.Day, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Warm Up##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00475465/M00475465_WarmUp.png",width=1000, height=600) #Opens a PNG file#

mondataWU <- data.frame(monday$Warm.Up, monday$label)
names(mondataWU) <- c("Warm.Up", "label")
tuesdataWU <- data.frame(tuesday$Warm.Up, tuesday$label)
names(tuesdataWU) <- c("Warm.Up", "label")
weddataWU <- data.frame(wednesday$Warm.Up, wednesday$label)
names(weddataWU) <- c("Warm.Up", "label")
thursdataWU <- data.frame(thursday$Warm.Up, thursday$label)
names(thursdataWU) <- c("Warm.Up", "label")

histDataWU <- rbind(mondataWU, tuesdataWU, weddataWU, thursdataWU) #creates data frame with histogram data
histDataWU <- na.omit(histDataWU)
ggplot(histDataWU, aes(Warm.Up, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Morning Meeting##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00475465/M00475465_MM.png",width=1000, height=600) #Opens a PNG file#

mondataMM <- data.frame(monday$Morning.Meeting, monday$label)
names(mondataMM) <- c("Morning.Meeting", "label")
tuesdataMM <- data.frame(tuesday$Morning.Meeting, tuesday$label)
names(tuesdataMM) <- c("Morning.Meeting", "label")
weddataMM <- data.frame(wednesday$Morning.Meeting, wednesday$label)
names(weddataMM) <- c("Morning.Meeting", "label")
thursdataMM <- data.frame(thursday$Morning.Meeting, thursday$label)
names(thursdataMM) <- c("Morning.Meeting", "label")

histDataMM <- rbind(mondataMM, tuesdataMM, weddataMM, thursdataMM) #creates data frame with histogram data
histDataMM <- na.omit(histDataMM)
ggplot(histDataMM, aes(Morning.Meeting, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Lunch##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00475465/M00475465_Lunch.png",width=1000, height=600) #Opens a PNG file#

mondataL <- data.frame(monday$Lunch, monday$label)
names(mondataL) <- c("Lunch", "label")
tuesdataL <- data.frame(tuesday$Lunch, tuesday$label)
names(tuesdataL) <- c("Lunch", "label")
weddataL <- data.frame(wednesday$Lunch, wednesday$label)
names(weddataL) <- c("Lunch", "label")
thursdataL <- data.frame(thursday$Lunch, thursday$label)
names(thursdataL) <- c("Lunch", "label")

histDataL <- rbind(mondataL, tuesdataL, weddataL, thursdataL) #creates data frame with histogram data
histDataL <- na.omit(histDataL)
ggplot(histDataL, aes(Lunch, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Outside Play##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00475465/M00475465_OutsidePlay.png",width=1000, height=600) #Opens a PNG file#

mondataOP <- data.frame(monday$Outside.Play, monday$label)
names(mondataOP) <- c("Outside.Play", "label")
tuesdataOP <- data.frame(tuesday$Outside.Play, tuesday$label)
names(tuesdataOP) <- c("Outside.Play", "label")
weddataOP <- data.frame(wednesday$Outside.Play, wednesday$label)
names(weddataOP) <- c("Outside.Play", "label")
thursdataOP <- data.frame(thursday$Outside.Play, thursday$label)
names(thursdataOP) <- c("Outside.Play", "label")

histDataOP <- rbind(mondataOP, tuesdataOP, weddataOP, thursdataOP) #creates data frame with histogram data
histDataOP <- na.omit(histDataOP)
ggplot(histDataOP, aes(Outside.Play, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Prize Store##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00475465/M00475465_PrizeStore.png",width=1000, height=600) #Opens a PNG file#

mondataPS <- data.frame(monday$Prize.Store, monday$label)
names(mondataPS) <- c("Prize.Store", "label")
tuesdataPS <- data.frame(tuesday$Prize.Store, tuesday$label)
names(tuesdataPS) <- c("Prize.Store", "label")
weddataPS <- data.frame(wednesday$Prize.Store, wednesday$label)
names(weddataPS) <- c("Prize.Store", "label")
thursdataPS <- data.frame(thursday$Prize.Store, thursday$label)
names(thursdataPS) <- c("Prize.Store", "label")

histDataPS <- rbind(mondataPS, tuesdataPS, weddataPS, thursdataPS) #creates data frame with histogram data
histDataPS <- na.omit(histDataPS)
ggplot(histDataPS, aes(Prize.Store, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()
rm(list=ls())

##M00490907

monday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00490907/Distributions/M00490907_monday.csv")

tuesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00490907/Distributions/M00490907_tuesday.csv")

wednesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00490907/Distributions/M00490907_wednesday.csv")

thursday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00490907/Distributions/M00490907_thursday.csv")

#creates a label column
monday$label <- 'mon'
tuesday$label <- 'tues'
wednesday$label <- 'wed'
thursday$label <- 'thurs'

###Full Day Plot###
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00490907/M00490907_Full.png",width=1000, height=600) #Opens a PNG file#

#create new data frames with just full day values and labels
mondata <- data.frame(monday$Full.Day, monday$label)
names(mondata) <- c("Full.Day", "label")
tuesdata <- data.frame(tuesday$Full.Day, tuesday$label)
names(tuesdata) <- c("Full.Day", "label")
weddata <- data.frame(wednesday$Full.Day, wednesday$label)
names(weddata) <- c("Full.Day", "label")
thursdata <- data.frame(thursday$Full.Day, thursday$label)
names(thursdata) <- c("Full.Day", "label")

histData <- rbind(mondata, tuesdata, weddata, thursdata) #creates data frame with histogram data

ggplot(histData, aes(Full.Day, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Warm Up##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00490907/M00490907_WarmUp.png",width=1000, height=600) #Opens a PNG file#

mondataWU <- data.frame(monday$Warm.Up, monday$label)
names(mondataWU) <- c("Warm.Up", "label")
tuesdataWU <- data.frame(tuesday$Warm.Up, tuesday$label)
names(tuesdataWU) <- c("Warm.Up", "label")
weddataWU <- data.frame(wednesday$Warm.Up, wednesday$label)
names(weddataWU) <- c("Warm.Up", "label")
thursdataWU <- data.frame(thursday$Warm.Up, thursday$label)
names(thursdataWU) <- c("Warm.Up", "label")

histDataWU <- rbind(mondataWU, tuesdataWU, weddataWU, thursdataWU) #creates data frame with histogram data
histDataWU <- na.omit(histDataWU)
ggplot(histDataWU, aes(Warm.Up, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Morning Meeting##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00490907/M00490907_MM.png",width=1000, height=600) #Opens a PNG file#

mondataMM <- data.frame(monday$Morning.Meeting, monday$label)
names(mondataMM) <- c("Morning.Meeting", "label")
tuesdataMM <- data.frame(tuesday$Morning.Meeting, tuesday$label)
names(tuesdataMM) <- c("Morning.Meeting", "label")
weddataMM <- data.frame(wednesday$Morning.Meeting, wednesday$label)
names(weddataMM) <- c("Morning.Meeting", "label")
thursdataMM <- data.frame(thursday$Morning.Meeting, thursday$label)
names(thursdataMM) <- c("Morning.Meeting", "label")

histDataMM <- rbind(mondataMM, tuesdataMM, weddataMM, thursdataMM) #creates data frame with histogram data
histDataMM <- na.omit(histDataMM)
ggplot(histDataMM, aes(Morning.Meeting, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Lunch##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00490907/M00490907_Lunch.png",width=1000, height=600) #Opens a PNG file#

mondataL <- data.frame(monday$Lunch, monday$label)
names(mondataL) <- c("Lunch", "label")
tuesdataL <- data.frame(tuesday$Lunch, tuesday$label)
names(tuesdataL) <- c("Lunch", "label")
weddataL <- data.frame(wednesday$Lunch, wednesday$label)
names(weddataL) <- c("Lunch", "label")
thursdataL <- data.frame(thursday$Lunch, thursday$label)
names(thursdataL) <- c("Lunch", "label")

histDataL <- rbind(mondataL, tuesdataL, weddataL, thursdataL) #creates data frame with histogram data
histDataL <- na.omit(histDataL)
ggplot(histDataL, aes(Lunch, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Outside Play##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00490907/M00490907_OutsidePlay.png",width=1000, height=600) #Opens a PNG file#

mondataOP <- data.frame(monday$Outside.Play, monday$label)
names(mondataOP) <- c("Outside.Play", "label")
tuesdataOP <- data.frame(tuesday$Outside.Play, tuesday$label)
names(tuesdataOP) <- c("Outside.Play", "label")
weddataOP <- data.frame(wednesday$Outside.Play, wednesday$label)
names(weddataOP) <- c("Outside.Play", "label")
thursdataOP <- data.frame(thursday$Outside.Play, thursday$label)
names(thursdataOP) <- c("Outside.Play", "label")

histDataOP <- rbind(mondataOP, tuesdataOP, weddataOP, thursdataOP) #creates data frame with histogram data
histDataOP <- na.omit(histDataOP)
ggplot(histDataOP, aes(Outside.Play, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Prize Store##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00490907/M00490907_PrizeStore.png",width=1000, height=600) #Opens a PNG file#

mondataPS <- data.frame(monday$Prize.Store, monday$label)
names(mondataPS) <- c("Prize.Store", "label")
tuesdataPS <- data.frame(tuesday$Prize.Store, tuesday$label)
names(tuesdataPS) <- c("Prize.Store", "label")
weddataPS <- data.frame(wednesday$Prize.Store, wednesday$label)
names(weddataPS) <- c("Prize.Store", "label")
thursdataPS <- data.frame(thursday$Prize.Store, thursday$label)
names(thursdataPS) <- c("Prize.Store", "label")

histDataPS <- rbind(mondataPS, tuesdataPS, weddataPS, thursdataPS) #creates data frame with histogram data
histDataPS <- na.omit(histDataPS)
ggplot(histDataPS, aes(Prize.Store, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()
rm(list=ls())

##M00495999

monday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00495999/Distributions/M00495999_monday.csv")

tuesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00495999/Distributions/M00495999_tuesday.csv")

wednesday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00495999/Distributions/M00495999_wednesday.csv")

thursday <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/M00495999/Distributions/M00495999_thursday.csv")

#creates a label column
monday$label <- 'mon'
tuesday$label <- 'tues'
wednesday$label <- 'wed'
thursday$label <- 'thurs'

###Full Day Plot###
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00495999/M00495999_Full.png",width=1000, height=600) #Opens a PNG file#

#create new data frames with just full day values and labels
mondata <- data.frame(monday$Full.Day, monday$label)
names(mondata) <- c("Full.Day", "label")
tuesdata <- data.frame(tuesday$Full.Day, tuesday$label)
names(tuesdata) <- c("Full.Day", "label")
weddata <- data.frame(wednesday$Full.Day, wednesday$label)
names(weddata) <- c("Full.Day", "label")
thursdata <- data.frame(thursday$Full.Day, thursday$label)
names(thursdata) <- c("Full.Day", "label")

histData <- rbind(mondata, tuesdata, weddata, thursdata) #creates data frame with histogram data

ggplot(histData, aes(Full.Day, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Warm Up##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00495999/M00495999_WarmUp.png",width=1000, height=600) #Opens a PNG file#

mondataWU <- data.frame(monday$Warm.Up, monday$label)
names(mondataWU) <- c("Warm.Up", "label")
tuesdataWU <- data.frame(tuesday$Warm.Up, tuesday$label)
names(tuesdataWU) <- c("Warm.Up", "label")
weddataWU <- data.frame(wednesday$Warm.Up, wednesday$label)
names(weddataWU) <- c("Warm.Up", "label")
thursdataWU <- data.frame(thursday$Warm.Up, thursday$label)
names(thursdataWU) <- c("Warm.Up", "label")

histDataWU <- rbind(mondataWU, tuesdataWU, weddataWU, thursdataWU) #creates data frame with histogram data
histDataWU <- na.omit(histDataWU)
ggplot(histDataWU, aes(Warm.Up, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Morning Meeting##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00495999/M00495999_MM.png",width=1000, height=600) #Opens a PNG file#

mondataMM <- data.frame(monday$Morning.Meeting, monday$label)
names(mondataMM) <- c("Morning.Meeting", "label")
tuesdataMM <- data.frame(tuesday$Morning.Meeting, tuesday$label)
names(tuesdataMM) <- c("Morning.Meeting", "label")
weddataMM <- data.frame(wednesday$Morning.Meeting, wednesday$label)
names(weddataMM) <- c("Morning.Meeting", "label")
thursdataMM <- data.frame(thursday$Morning.Meeting, thursday$label)
names(thursdataMM) <- c("Morning.Meeting", "label")

histDataMM <- rbind(mondataMM, tuesdataMM, weddataMM, thursdataMM) #creates data frame with histogram data
histDataMM <- na.omit(histDataMM)
ggplot(histDataMM, aes(Morning.Meeting, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Lunch##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00495999/M00495999_Lunch.png",width=1000, height=600) #Opens a PNG file#

mondataL <- data.frame(monday$Lunch, monday$label)
names(mondataL) <- c("Lunch", "label")
tuesdataL <- data.frame(tuesday$Lunch, tuesday$label)
names(tuesdataL) <- c("Lunch", "label")
weddataL <- data.frame(wednesday$Lunch, wednesday$label)
names(weddataL) <- c("Lunch", "label")
thursdataL <- data.frame(thursday$Lunch, thursday$label)
names(thursdataL) <- c("Lunch", "label")

histDataL <- rbind(mondataL, tuesdataL, weddataL, thursdataL) #creates data frame with histogram data
histDataL <- na.omit(histDataL)
ggplot(histDataL, aes(Lunch, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Outside Play##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00495999/M00495999_OutsidePlay.png",width=1000, height=600) #Opens a PNG file#

mondataOP <- data.frame(monday$Outside.Play, monday$label)
names(mondataOP) <- c("Outside.Play", "label")
tuesdataOP <- data.frame(tuesday$Outside.Play, tuesday$label)
names(tuesdataOP) <- c("Outside.Play", "label")
weddataOP <- data.frame(wednesday$Outside.Play, wednesday$label)
names(weddataOP) <- c("Outside.Play", "label")
thursdataOP <- data.frame(thursday$Outside.Play, thursday$label)
names(thursdataOP) <- c("Outside.Play", "label")

histDataOP <- rbind(mondataOP, tuesdataOP, weddataOP, thursdataOP) #creates data frame with histogram data
histDataOP <- na.omit(histDataOP)
ggplot(histDataOP, aes(Outside.Play, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()

##Prize Store##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/M00495999/M00495999_PrizeStore.png",width=1000, height=600) #Opens a PNG file#

mondataPS <- data.frame(monday$Prize.Store, monday$label)
names(mondataPS) <- c("Prize.Store", "label")
tuesdataPS <- data.frame(tuesday$Prize.Store, tuesday$label)
names(tuesdataPS) <- c("Prize.Store", "label")
weddataPS <- data.frame(wednesday$Prize.Store, wednesday$label)
names(weddataPS) <- c("Prize.Store", "label")
thursdataPS <- data.frame(thursday$Prize.Store, thursday$label)
names(thursdataPS) <- c("Prize.Store", "label")

histDataPS <- rbind(mondataPS, tuesdataPS, weddataPS, thursdataPS) #creates data frame with histogram data
histDataPS <- na.omit(histDataPS)
ggplot(histDataPS, aes(Prize.Store, fill = label))+ geom_density(alpha = 0.2) #plot histogram
dev.off()
rm(list=ls())