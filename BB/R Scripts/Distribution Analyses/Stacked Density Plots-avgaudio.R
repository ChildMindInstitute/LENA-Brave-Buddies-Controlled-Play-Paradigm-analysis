##Brave Buddies Stacked Density Plots-Average Audio Levels

library("ggplot2")
library("reshape")
library("plyr")
library("car")
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/Distributions")

##Monday
#load in cleaned monday files
filenames <- list.files(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/Distributions", pattern = "*monday", full.names = FALSE)
numfiles <- length(filenames)
for(i in c(1:numfiles)){
  filenames[i] <- paste("",filenames[i],sep="")  
  assign(gsub("[.]csv$","",filenames[i]),read.csv(filenames[i], header=TRUE, stringsAsFactors = FALSE))
}

#adds row index
M00402344_monday$index <- seq.int(nrow(M00402344_monday))
M00412434_monday$index <- seq.int(nrow(M00412434_monday))
M00413464_monday$index <- seq.int(nrow(M00413464_monday))
M00440011_monday$index <- seq.int(nrow(M00440011_monday))
M00441664_monday$index <- seq.int(nrow(M00441664_monday))
M00445929_monday$index <- seq.int(nrow(M00445929_monday))
M00447059_monday$index <- seq.int(nrow(M00447059_monday))
M00472399_monday$index <- seq.int(nrow(M00472399_monday))
M00475465_monday$index <- seq.int(nrow(M00475465_monday))
M00490907_monday$index <- seq.int(nrow(M00490907_monday))
M00494954_monday$index <- seq.int(nrow(M00494954_monday))
M00495999_monday$index <- seq.int(nrow(M00495999_monday))

#adds ursi column
M00402344_monday$ursi <- "M00402344"
M00412434_monday$ursi <- "M00412434"
M00413464_monday$ursi <- "M00413464"
M00440011_monday$ursi <- "M00440011"
M00441664_monday$ursi <- "M00441664"
M00445929_monday$ursi <- "M00445929"
M00447059_monday$ursi <- "M00447059"
M00472399_monday$ursi <- "M00472399"
M00475465_monday$ursi <- "M00475465"
M00490907_monday$ursi <- "M00490907"
M00494954_monday$ursi <- "M00494954"
M00495999_monday$ursi <- "M00495999"

#creates a list of all files
monday_list = list(M00494954_monday, M00441664_monday, M00445929_monday, M00440011_monday, M00490907_monday, M00447059_monday, M00495999_monday,
                   M00402344_monday, M00413464_monday, M00412434_monday, M00475465_monday, M00472399_monday)
#binds together all listed files into a single, ordered data frame
monday_totals <- do.call(rbind.data.frame, monday_list)

#plot density plots
#Full Day
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/monday full day composite.png",width=1000, height=600) #Opens a PNG file#

mon <- ggplot(monday_totals, aes(Full.Day, fill = ursi)) + geom_density(alpha = 0.2)
mon + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("dB SPL Density on Monday")

dev.off()

#Warm Up
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/monday Average warm up composite.png",width=1000, height=600) #Opens a PNG file#
monWU <- ggplot(monday_totals, aes(Warm.Up, fill = ursi)) + geom_density(alpha = 0.2)
monWU + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on Monday-Warm Up")
dev.off()
#Morning Meeting
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/monday Average morning meeting composite.png",width=1000, height=600) #Opens a PNG file#

monMM <- ggplot(monday_totals, aes(Morning.Meeting, fill = ursi)) + geom_density(alpha = 0.2)
monMM + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on Monday-Morning Meeting")
dev.off()
#Lunch
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/monday Average lunch composite.png",width=1000, height=600) #Opens a PNG file#

monL <- ggplot(monday_totals, aes(Lunch, fill = ursi)) + geom_density(alpha = 0.2)
monL + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on Monday-Lunch")
dev.off()
#Outside Play
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/monday Average outside play composite.png",width=1000, height=600) #Opens a PNG file#

monOP <- ggplot(monday_totals, aes(Outside.Play, fill = ursi)) + geom_density(alpha = 0.2)
monOP + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on Monday-Outside Play")
dev.off()

#Prize Store
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/monday Average prize store composite.png",width=1000, height=600) #Opens a PNG file#
monPS <- ggplot(monday_totals, aes(Prize.Store, fill = ursi)) + geom_density(alpha = 0.2)
monPS + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on Monday-Prize Store")
dev.off()

##Tuesday
tuesnames <- list.files(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/Distributions", pattern = "*tuesday", full.names = FALSE)
tuesfiles <- length(tuesnames)
for(i in c(1:tuesfiles)){
  tuesnames[i] <- paste("",tuesnames[i],sep="")  
  assign(gsub("[.]csv$","",tuesnames[i]),read.csv(tuesnames[i], header=TRUE, stringsAsFactors = FALSE))
}

M00402344_tuesday$index <- seq.int(nrow(M00402344_tuesday))
M00412434_tuesday$index <- seq.int(nrow(M00412434_tuesday))
M00413464_tuesday$index <- seq.int(nrow(M00413464_tuesday))
M00440011_tuesday$index <- seq.int(nrow(M00440011_tuesday))
M00441664_tuesday$index <- seq.int(nrow(M00441664_tuesday))
M00445929_tuesday$index <- seq.int(nrow(M00445929_tuesday))
M00447059_tuesday$index <- seq.int(nrow(M00447059_tuesday))
M00472399_tuesday$index <- seq.int(nrow(M00472399_tuesday))
M00475465_tuesday$index <- seq.int(nrow(M00475465_tuesday))
M00490907_tuesday$index <- seq.int(nrow(M00490907_tuesday))
M00494954_tuesday$index <- seq.int(nrow(M00494954_tuesday))
M00495999_tuesday$index <- seq.int(nrow(M00495999_tuesday))

M00402344_tuesday$ursi <- "M00402344"
M00412434_tuesday$ursi <- "M00412434"
M00413464_tuesday$ursi <- "M00413464"
M00440011_tuesday$ursi <- "M00440011"
M00441664_tuesday$ursi <- "M00441664"
M00445929_tuesday$ursi <- "M00445929"
M00447059_tuesday$ursi <- "M00447059"
M00472399_tuesday$ursi <- "M00472399"
M00475465_tuesday$ursi <- "M00475465"
M00490907_tuesday$ursi <- "M00490907"
M00494954_tuesday$ursi <- "M00494954"
M00495999_tuesday$ursi <- "M00495999"

tuesday_list = list(M00494954_tuesday, M00441664_tuesday, M00445929_tuesday, M00440011_tuesday, M00490907_tuesday, M00447059_tuesday, M00495999_tuesday,
                   M00402344_tuesday, M00413464_tuesday, M00412434_tuesday, M00475465_tuesday, M00472399_tuesday)

tuesday_totals <- do.call(rbind.data.frame, tuesday_list)

#Full Day
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/tuesday full day composite.png",width=1000, height=600) #Opens a PNG file#

tues <- ggplot(tuesday_totals, aes(Full.Day, fill = ursi)) + geom_density(alpha = 0.2)
tues + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("dB SPL Density on Tuesday")
dev.off()

#Warm Up
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/tuesday Average warm up composite.png",width=1000, height=600) #Opens a PNG file#
tuesdayWU <- ggplot(tuesday_totals, aes(Warm.Up, fill = ursi)) + geom_density(alpha = 0.2)
tuesdayWU + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on tuesday-Warm Up")
dev.off()
#Morning Meeting
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/tuesday Average morning meeting composite.png",width=1000, height=600) #Opens a PNG file#

tuesMM <- ggplot(tuesday_totals, aes(Morning.Meeting, fill = ursi)) + geom_density(alpha = 0.2)
tuesMM + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on tuesday-Morning Meeting")
dev.off()
#Lunch
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/tuesday Average lunch composite.png",width=1000, height=600) #Opens a PNG file#

tuesL <- ggplot(tuesday_totals, aes(Lunch, fill = ursi)) + geom_density(alpha = 0.2)
tuesL + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on tuesday-Lunch")
dev.off()
#Outside Play
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/tuesday Average outside play composite.png",width=1000, height=600) #Opens a PNG file#

tuesOP <- ggplot(tuesday_totals, aes(Outside.Play, fill = ursi)) + geom_density(alpha = 0.2)
tuesOP + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on tuesday-Outside Play")
dev.off()

#Prize Store
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/tuesday Average prize store composite.png",width=1000, height=600) #Opens a PNG file#
tuesPS <- ggplot(tuesday_totals, aes(Prize.Store, fill = ursi)) + geom_density(alpha = 0.2)
tuesPS + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on tuesday-Prize Store")
dev.off()

##Wednesday
wedsnames <- list.files(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/Distributions", pattern = "*wednesday", full.names = FALSE)
wedfiles <- length(wedsnames)
for(i in c(1:wedfiles)){
  wedsnames[i] <- paste("",wedsnames[i],sep="")  
  assign(gsub("[.]csv$","",wedsnames[i]),read.csv(wedsnames[i], header=TRUE, stringsAsFactors = FALSE))
}

M00402344_wednesday$index <- seq.int(nrow(M00402344_wednesday))
M00412434_wednesday$index <- seq.int(nrow(M00412434_wednesday))
M00413464_wednesday$index <- seq.int(nrow(M00413464_wednesday))
M00440011_wednesday$index <- seq.int(nrow(M00440011_wednesday))
M00441664_wednesday$index <- seq.int(nrow(M00441664_wednesday))
M00445929_wednesday$index <- seq.int(nrow(M00445929_wednesday))
M00447059_wednesday$index <- seq.int(nrow(M00447059_wednesday))
M00472399_wednesday$index <- seq.int(nrow(M00472399_wednesday))
M00475465_wednesday$index <- seq.int(nrow(M00475465_wednesday))
M00490907_wednesday$index <- seq.int(nrow(M00490907_wednesday))
M00494954_wednesday$index <- seq.int(nrow(M00494954_wednesday))
M00495999_wednesday$index <- seq.int(nrow(M00495999_wednesday))

M00402344_wednesday$ursi <- "M00402344"
M00412434_wednesday$ursi <- "M00412434"
M00413464_wednesday$ursi <- "M00413464"
M00440011_wednesday$ursi <- "M00440011"
M00441664_wednesday$ursi <- "M00441664"
M00445929_wednesday$ursi <- "M00445929"
M00447059_wednesday$ursi <- "M00447059"
M00472399_wednesday$ursi <- "M00472399"
M00475465_wednesday$ursi <- "M00475465"
M00490907_wednesday$ursi <- "M00490907"
M00494954_wednesday$ursi <- "M00494954"
M00495999_wednesday$ursi <- "M00495999"

wednesday_list = list(M00494954_wednesday, M00441664_wednesday, M00445929_wednesday, M00440011_wednesday, M00490907_wednesday, M00447059_wednesday, M00495999_wednesday,
                    M00402344_wednesday, M00413464_wednesday, M00412434_wednesday, M00475465_wednesday, M00472399_wednesday)

wednesday_totals <- do.call(rbind.data.frame, wednesday_list)

#Full Day
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/wednesday full day composite.png",width=1000, height=600) #Opens a PNG file#

wed <- ggplot(wednesday_totals, aes(Full.Day, fill = ursi)) + geom_density(alpha = 0.2)
wed + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("dB SPL Density on Wednesday")

dev.off()

#Warm Up
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Wednesday Average warm up composite.png",width=1000, height=600) #Opens a PNG file#
WednesdayWU <- ggplot(wednesday_totals, aes(Warm.Up, fill = ursi)) + geom_density(alpha = 0.2)
WednesdayWU + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on Wednesday-Warm Up")
dev.off()

#Morning Meeting
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Wednesday Average morning meeting composite.png",width=1000, height=600) #Opens a PNG file#

WedMM <- ggplot(wednesday_totals, aes(Morning.Meeting, fill = ursi)) + geom_density(alpha = 0.2)
WedMM + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on Wednesday-Morning Meeting")
dev.off()
#Lunch
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Wednesday Average lunch composite.png",width=1000, height=600) #Opens a PNG file#

WedL <- ggplot(wednesday_totals, aes(Lunch, fill = ursi)) + geom_density(alpha = 0.2)
WedL + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on Wednesday-Lunch")
dev.off()
#Outside Play
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Wednesday Average outside play composite.png",width=1000, height=600) #Opens a PNG file#

WedOP <- ggplot(wednesday_totals, aes(Outside.Play, fill = ursi)) + geom_density(alpha = 0.2)
WedOP + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on Wednesday-Outside Play")
dev.off()

#Prize Store
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Wednesday Average prize store composite.png",width=1000, height=600) #Opens a PNG file#
WedPS <- ggplot(wednesday_totals, aes(Prize.Store, fill = ursi)) + geom_density(alpha = 0.2)
WedPS + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on Wednesday-Prize Store")
dev.off()


##Thursday

thursnames <- list.files(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/Distributions", pattern = "*thursday", full.names = FALSE)
thursfiles <- length(thursnames)
for(i in c(1:thursfiles)){
  thursnames[i] <- paste("",thursnames[i],sep="")  
  assign(gsub("[.]csv$","",thursnames[i]),read.csv(thursnames[i], header=TRUE, stringsAsFactors = FALSE))
}

M00402344_thursday$index <- seq.int(nrow(M00402344_thursday))
M00412434_thursday$index <- seq.int(nrow(M00412434_thursday))
M00413464_thursday$index <- seq.int(nrow(M00413464_thursday))
M00440011_thursday$index <- seq.int(nrow(M00440011_thursday))
M00441664_thursday$index <- seq.int(nrow(M00441664_thursday))
M00445929_thursday$index <- seq.int(nrow(M00445929_thursday))
M00447059_thursday$index <- seq.int(nrow(M00447059_thursday))
M00472399_thursday$index <- seq.int(nrow(M00472399_thursday))
M00475465_thursday$index <- seq.int(nrow(M00475465_thursday))
M00490907_thursday$index <- seq.int(nrow(M00490907_thursday))
M00494954_thursday$index <- seq.int(nrow(M00494954_thursday))
M00495999_thursday$index <- seq.int(nrow(M00495999_thursday))

M00402344_thursday$ursi <- "M00402344"
M00412434_thursday$ursi <- "M00412434"
M00413464_thursday$ursi <- "M00413464"
M00440011_thursday$ursi <- "M00440011"
M00441664_thursday$ursi <- "M00441664"
M00445929_thursday$ursi <- "M00445929"
M00447059_thursday$ursi <- "M00447059"
M00472399_thursday$ursi <- "M00472399"
M00475465_thursday$ursi <- "M00475465"
M00490907_thursday$ursi <- "M00490907"
M00494954_thursday$ursi <- "M00494954"
M00495999_thursday$ursi <- "M00495999"

thursday_list = list(M00494954_thursday, M00441664_thursday, M00445929_thursday, M00440011_thursday, M00490907_thursday, M00447059_thursday, M00495999_thursday,
                    M00402344_thursday, M00413464_thursday, M00412434_thursday, M00475465_thursday, M00472399_thursday)

thursday_totals <- do.call(rbind.data.frame, thursday_list)

#Full Day
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/thursday full day composite.png",width=1000, height=600) #Opens a PNG file#

thurs <- ggplot(thursday_totals, aes(Full.Day, fill = ursi)) + geom_density(alpha = 0.2)
thurs + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("dB SPL Density on Thursday")
dev.off()

#Warm Up
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/thursday Average warm up composite.png",width=1000, height=600) #Opens a PNG file#
thursdayWU <- ggplot(thursday_totals, aes(Warm.Up, fill = ursi)) + geom_density(alpha = 0.2)
thursdayWU + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on thursday-Warm Up")
dev.off()

#Morning Meeting
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/thursday Average morning meeting composite.png",width=1000, height=600) #Opens a PNG file#

thursMM <- ggplot(thursday_totals, aes(Morning.Meeting, fill = ursi)) + geom_density(alpha = 0.2)
thursMM + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on thursday-Morning Meeting")
dev.off()
#Lunch
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/thursday Average lunch composite.png",width=1000, height=600) #Opens a PNG file#

thursL <- ggplot(thursday_totals, aes(Lunch, fill = ursi)) + geom_density(alpha = 0.2)
thursL + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on thursday-Lunch")
dev.off()
#Outside Play
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/thursday Average outside play composite.png",width=1000, height=600) #Opens a PNG file#

thursOP <- ggplot(thursday_totals, aes(Outside.Play, fill = ursi)) + geom_density(alpha = 0.2)
thursOP + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on thursday-Outside Play")
dev.off()

#Prize Store
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/thursday Average prize store composite.png",width=1000, height=600) #Opens a PNG file#
thursPS <- ggplot(thursday_totals, aes(Prize.Store, fill = ursi)) + geom_density(alpha = 0.2)
thursPS + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Average dB SPL Density on thursday-Prize Store")
dev.off()

####Averages####
#Full Day
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Full day Average composite.png",width=1000, height=600) #Opens a PNG file#

composite <- ggplot() + geom_density(data = monday_totals, aes(x = Full.Day, color = "monday")) +
  geom_density(data = tuesday_totals, aes(x = Full.Day, color = "tuesday")) +
  geom_density(data = wednesday_totals, aes(x = Full.Day, color = "wednesday")) + 
  geom_density(data = thursday_totals, aes(x = Full.Day, color = "thursday"))

composite + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("dB SPL Density Throughout the Week")

dev.off()

#Warm Up
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Warm Up Average composite.png",width=1000, height=600) #Opens a PNG file#

compositeWU <- ggplot() + geom_density(data = monday_totals, aes(x = Warm.Up, color = "monday")) +
  geom_density(data = tuesday_totals, aes(x = Warm.Up, color = "tuesday")) +
  geom_density(data = wednesday_totals, aes(x = Warm.Up, color = "wednesday")) + 
  geom_density(data = thursday_totals, aes(x = Warm.Up, color = "thursday"))
compositeWU + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("dB SPL Density Throughout the Week-Warm Up")
dev.off()

#Morning Meeting
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Morning Meeting Average composite.png",width=1000, height=600) #Opens a PNG file#

compositeMM <- ggplot() + geom_density(data = monday_totals, aes(x = Morning.Meeting, color = "monday")) +
  geom_density(data = tuesday_totals, aes(x = Morning.Meeting, color = "tuesday")) +
  geom_density(data = wednesday_totals, aes(x = Morning.Meeting, color = "wednesday")) + 
  geom_density(data = thursday_totals, aes(x = Morning.Meeting, color = "thursday"))
compositeMM + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("dB SPL Density Throughout the Week-Morning Meeting")
dev.off()

#Lunch
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Lunch Average composite.png",width=1000, height=600) #Opens a PNG file#

compositeL <- ggplot() + geom_density(data = monday_totals, aes(x = Lunch, color = "monday")) +
  geom_density(data = tuesday_totals, aes(x = Lunch, color = "tuesday")) +
  geom_density(data = wednesday_totals, aes(x = Lunch, color = "wednesday")) + 
  geom_density(data = thursday_totals, aes(x = Lunch, color = "thursday"))
compositeL + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("dB SPL Density Throughout the Week-Lunch")
dev.off()

#Outside Play
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Outside Play Average composite.png",width=1000, height=600) #Opens a PNG file#

compositeOP <- ggplot() + geom_density(data = monday_totals, aes(x = Outside.Play, color = "monday")) +
  geom_density(data = tuesday_totals, aes(x = Outside.Play, color = "tuesday")) +
  geom_density(data = wednesday_totals, aes(x = Outside.Play, color = "wednesday")) + 
  geom_density(data = thursday_totals, aes(x = Outside.Play, color = "thursday"))
compositeOP + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("dB SPL Density Throughout the Week-Outside Play")
dev.off()

#Prize Store
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Prize Store Average composite.png",width=1000, height=600) #Opens a PNG file#

compositePS <- ggplot() + geom_density(data = monday_totals, aes(x = Prize.Store, color = "monday")) +
  geom_density(data = tuesday_totals, aes(x = Prize.Store, color = "tuesday")) +
  geom_density(data = wednesday_totals, aes(x = Prize.Store, color = "wednesday")) + 
  geom_density(data = thursday_totals, aes(x = Prize.Store, color = "thursday"))
compositePS + scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("dB SPL Density Throughout the Week-Prize Store")
dev.off()

##DENSITY PLOTS WITH CONFIDENCE INTERVALS
#monday average
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Graphs with confidence intervals and mean/monday confidence int.png",width=800, height=600) #Opens a PNG file#

m <- ggplot() + geom_density(data = monday_totals, aes(x = Full.Day))

mq5 <- quantile(monday_totals$Full.Day,.05)
mq95 <- quantile(monday_totals$Full.Day,.95)
mmeanx <- mean(monday_totals$Full.Day)
mx.dens <- density(monday_totals$Full.Day)
mdf.dens <- data.frame(x = mx.dens$x, y = mx.dens$y)
compM <- m + geom_area(data = subset(mdf.dens, x >= mq5 & x <= mq95), 
                      aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = mmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(70, 85), name = "Average Child Segment dB SPL") + ggtitle("Monday")
compM

dev.off()
#tuesday
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Graphs with confidence intervals and mean/tuesday confidence int.png",width=800, height=600) #Opens a PNG file#

t <- ggplot() + geom_density(data = tuesday_totals, aes(x = Full.Day))

tq5 <- quantile(tuesday_totals$Full.Day,.05)
tq95 <- quantile(tuesday_totals$Full.Day,.95)
tmeanx <- mean(tuesday_totals$Full.Day)
tx.dens <- density(tuesday_totals$Full.Day)
tdf.dens <- data.frame(x = tx.dens$x, y = tx.dens$y)
compT <- t + geom_area(data = subset(tdf.dens, x >= tq5 & x <= tq95), 
              aes(x=x,y=y), fill = 'blue', alpha = 0.1) +
  geom_vline(xintercept = tmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(70, 85), name = "Average Child Segment dB SPL") + ggtitle("Tuesday")
compT

dev.off()
#wednesday
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Graphs with confidence intervals and mean/wednesday confidence int.png",width=800, height=600) #Opens a PNG file#

w <- ggplot() + geom_density(data = wednesday_totals, aes(x = Full.Day))

wq5 <- quantile(wednesday_totals$Full.Day,.05)
wq95 <- quantile(wednesday_totals$Full.Day,.95)
wmeanx <- mean(wednesday_totals$Full.Day)
wx.dens <- density(wednesday_totals$Full.Day)
wdf.dens <- data.frame(x = wx.dens$x, y = wx.dens$y)
compW <- w + geom_area(data = subset(wdf.dens, x >= wq5 & x <= wq95), 
              aes(x=x,y=y), fill = 'purple', alpha = 0.1) +
  geom_vline(xintercept = wmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(70, 85), name = "Average Child Segment dB SPL") + ggtitle("Wednesday")

compW

dev.off()
#Thursday
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Graphs with confidence intervals and mean/thursday confidence int.png",width=800, height=600) #Opens a PNG file#

r <- ggplot() + geom_density(data = thursday_totals, aes(x = Full.Day))

rq5 <- quantile(thursday_totals$Full.Day,.05)
rq95 <- quantile(thursday_totals$Full.Day,.95)
rmeanx <- mean(thursday_totals$Full.Day)
rx.dens <- density(thursday_totals$Full.Day)
rdf.dens <- data.frame(x = rx.dens$x, y = rx.dens$y)
compR <- r + geom_area(data = subset(rdf.dens, x >= rq5 & x <= rq95), 
              aes(x=x,y=y), fill = 'green', alpha = 0.1) +
  geom_vline(xintercept = rmeanx, show.legend = TRUE) + 
  scale_x_continuous(limits = c(70, 85), name = "Average Child Segment dB SPL") + ggtitle("Thursday")
compR

dev.off()

#Week Long Stacked

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Graphs with confidence intervals and mean/avg week long stacked.png",width=800, height=600) #Opens a PNG file#

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(compM), ggplotGrob(compT), ggplotGrob(compW), ggplotGrob(compR), size = "last"))
dev.off()

######EVENT DENSITY PLOTS WITH CONFIDENCE INTERVALS#####
##Monday
#Warm Up

mWU <- ggplot() + geom_density(data = monday_totals, aes(x = Warm.Up))

mWUq5 <- quantile(monday_totals$Warm.Up,.05, na.rm = TRUE)
mWUq95 <- quantile(monday_totals$Warm.Up,.95, na.rm = TRUE)
mWUmeanx <- mean(monday_totals$Warm.Up, na.rm = TRUE)
mWUx.dens <- density(monday_totals$Warm.Up, na.rm = TRUE)
mWUdf.dens <- data.frame(x = mWUx.dens$x, y = mWUx.dens$y)
MeWU <- mWU + geom_area(data = subset(mWUdf.dens, x >= mWUq5 & x <= mWUq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = mWUmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Monday Warm Up")


#Morning Meeting

mMM <- ggplot() + geom_density(data = monday_totals, aes(x = Morning.Meeting))

mMMq5 <- quantile(monday_totals$Morning.Meeting,.05, na.rm = TRUE)
mMMq95 <- quantile(monday_totals$Morning.Meeting,.95, na.rm = TRUE)
mMMmeanx <- mean(monday_totals$Morning.Meeting, na.rm = TRUE)
mMMx.dens <- density(monday_totals$Morning.Meeting, na.rm = TRUE)
mMMdf.dens <- data.frame(x = mMMx.dens$x, y = mMMx.dens$y)
MeMM <- mMM + geom_area(data = subset(mMMdf.dens, x >= mMMq5 & x <= mMMq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = mMMmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Monday Morning Meeting")

#Lunch
mL <- ggplot() + geom_density(data = monday_totals, aes(x = Lunch))

mLq5 <- quantile(monday_totals$Lunch,.05, na.rm = TRUE)
mLq95 <- quantile(monday_totals$Lunch,.95, na.rm = TRUE)
mLmeanx <- mean(monday_totals$Lunch, na.rm = TRUE)
mLx.dens <- density(monday_totals$Lunch, na.rm = TRUE)
mLdf.dens <- data.frame(x = mLx.dens$x, y = mLx.dens$y)
MeL <- mL + geom_area(data = subset(mLdf.dens, x >= mLq5 & x <= mLq95), 
                      aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = mLmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Monday Lunch")

#Outside Play
mOP <- ggplot() + geom_density(data = monday_totals, aes(x = Outside.Play))

mOPq5 <- quantile(monday_totals$Outside.Play,.05, na.rm = TRUE)
mOPq95 <- quantile(monday_totals$Outside.Play,.95, na.rm = TRUE)
mOPmeanx <- mean(monday_totals$Outside.Play, na.rm = TRUE)
mOPx.dens <- density(monday_totals$Outside.Play, na.rm = TRUE)
mOPdf.dens <- data.frame(x = mOPx.dens$x, y = mOPx.dens$y)
MeOP <- mOP + geom_area(data = subset(mOPdf.dens, x >= mOPq5 & x <= mOPq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = mOPmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Monday Outside Play")

#Prize Store
mPS <- ggplot() + geom_density(data = monday_totals, aes(x = Prize.Store))

mPSq5 <- quantile(monday_totals$Prize.Store,.05, na.rm = TRUE)
mPSq95 <- quantile(monday_totals$Prize.Store,.95, na.rm = TRUE)
mPSmeanx <- mean(monday_totals$Prize.Store, na.rm = TRUE)
mPSx.dens <- density(monday_totals$Prize.Store, na.rm = TRUE)
mPSdf.dens <- data.frame(x = mPSx.dens$x, y = mPSx.dens$y)
MePS <- mPS + geom_area(data = subset(mPSdf.dens, x >= mPSq5 & x <= mPSq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = mPSmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Monday Prize Store")

#Stacked Graphs
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Graphs with confidence intervals and mean/Average/Monday Events stacked.png",width=800, height=600) #Opens a PNG file#

grid.newpage()
grid.draw(rbind(ggplotGrob(MeWU), ggplotGrob(MeMM), ggplotGrob(MeL), ggplotGrob(MeOP), ggplotGrob(MePS), size = "last"))
dev.off()

##TUESDAY
#Warm Up

tWU <- ggplot() + geom_density(data = tuesday_totals, aes(x = Warm.Up))

tWUq5 <- quantile(tuesday_totals$Warm.Up,.05, na.rm = TRUE)
tWUq95 <- quantile(tuesday_totals$Warm.Up,.95, na.rm = TRUE)
tWUmeanx <- mean(tuesday_totals$Warm.Up, na.rm = TRUE)
tWUx.dens <- density(tuesday_totals$Warm.Up, na.rm = TRUE)
tWUdf.dens <- data.frame(x = tWUx.dens$x, y = tWUx.dens$y)
TeWU <- tWU + geom_area(data = subset(tWUdf.dens, x >= tWUq5 & x <= tWUq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = tWUmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("tuesday Warm Up")


#Morning Meeting

tMM <- ggplot() + geom_density(data = tuesday_totals, aes(x = Morning.Meeting))

tMMq5 <- quantile(tuesday_totals$Morning.Meeting,.05, na.rm = TRUE)
tMMq95 <- quantile(tuesday_totals$Morning.Meeting,.95, na.rm = TRUE)
tMMmeanx <- mean(tuesday_totals$Morning.Meeting, na.rm = TRUE)
tMMx.dens <- density(tuesday_totals$Morning.Meeting, na.rm = TRUE)
tMMdf.dens <- data.frame(x = tMMx.dens$x, y = tMMx.dens$y)
TeMM <- tMM + geom_area(data = subset(tMMdf.dens, x >= tMMq5 & x <= tMMq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = tMMmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("tuesday Morning Meeting")

#Lunch
tL <- ggplot() + geom_density(data = tuesday_totals, aes(x = Lunch))

tLq5 <- quantile(tuesday_totals$Lunch,.05, na.rm = TRUE)
tLq95 <- quantile(tuesday_totals$Lunch,.95, na.rm = TRUE)
tLmeanx <- mean(tuesday_totals$Lunch, na.rm = TRUE)
tLx.dens <- density(tuesday_totals$Lunch, na.rm = TRUE)
tLdf.dens <- data.frame(x = tLx.dens$x, y = tLx.dens$y)
TeL <- tL + geom_area(data = subset(tLdf.dens, x >= tLq5 & x <= tLq95), 
                      aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = tLmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("tuesday Lunch")

#Outside Play
tOP <- ggplot() + geom_density(data = tuesday_totals, aes(x = Outside.Play))

tOPq5 <- quantile(tuesday_totals$Outside.Play,.05, na.rm = TRUE)
tOPq95 <- quantile(tuesday_totals$Outside.Play,.95, na.rm = TRUE)
tOPmeanx <- mean(tuesday_totals$Outside.Play, na.rm = TRUE)
tOPx.dens <- density(tuesday_totals$Outside.Play, na.rm = TRUE)
tOPdf.dens <- data.frame(x = tOPx.dens$x, y = tOPx.dens$y)
TeOP <- tOP + geom_area(data = subset(tOPdf.dens, x >= tOPq5 & x <= tOPq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = tOPmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("tuesday Outside Play")

#Prize Store
tPS <- ggplot() + geom_density(data = tuesday_totals, aes(x = Prize.Store))

tPSq5 <- quantile(tuesday_totals$Prize.Store,.05, na.rm = TRUE)
tPSq95 <- quantile(tuesday_totals$Prize.Store,.95, na.rm = TRUE)
tPSmeanx <- mean(tuesday_totals$Prize.Store, na.rm = TRUE)
tPSx.dens <- density(tuesday_totals$Prize.Store, na.rm = TRUE)
tPSdf.dens <- data.frame(x = tPSx.dens$x, y = tPSx.dens$y)
TePS <- tPS + geom_area(data = subset(tPSdf.dens, x >= tPSq5 & x <= tPSq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = tPSmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("tuesday Prize Store")

#Stacked Graphs
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Graphs with confidence intervals and mean/Average/tuesday Events stacked.png",width=800, height=600) #Opens a PNG file#

grid.newpage()
grid.draw(rbind(ggplotGrob(TeWU), ggplotGrob(TeMM), ggplotGrob(TeL), ggplotGrob(TeOP), ggplotGrob(TePS), size = "last"))
dev.off()

#Wednesday 
#Warm Up
wWU <- ggplot() + geom_density(data = wednesday_totals, aes(x = Warm.Up))

wWUq5 <- quantile(wednesday_totals$Warm.Up,.05, na.rm = TRUE)
wWUq95 <- quantile(wednesday_totals$Warm.Up,.95, na.rm = TRUE)
wWUmeanx <- mean(wednesday_totals$Warm.Up, na.rm = TRUE)
wWUx.dens <- density(wednesday_totals$Warm.Up, na.rm = TRUE)
wWUdf.dens <- data.frame(x = wWUx.dens$x, y = wWUx.dens$y)
WeWU <- wWU + geom_area(data = subset(wWUdf.dens, x >= wWUq5 & x <= wWUq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = wWUmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("Wednesday Warm Up")


#Morning Meeting

wMM <- ggplot() + geom_density(data = wednesday_totals, aes(x = Morning.Meeting))

wMMq5 <- quantile(wednesday_totals$Morning.Meeting,.05, na.rm = TRUE)
wMMq95 <- quantile(wednesday_totals$Morning.Meeting,.95, na.rm = TRUE)
wMMmeanx <- mean(wednesday_totals$Morning.Meeting, na.rm = TRUE)
wMMx.dens <- density(wednesday_totals$Morning.Meeting, na.rm = TRUE)
wMMdf.dens <- data.frame(x = wMMx.dens$x, y = wMMx.dens$y)
WeMM <- wMM + geom_area(data = subset(wMMdf.dens, x >= wMMq5 & x <= wMMq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = wMMmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("wednesday Morning Meeting")

#Lunch
wL <- ggplot() + geom_density(data = wednesday_totals, aes(x = Lunch))

wLq5 <- quantile(wednesday_totals$Lunch,.05, na.rm = TRUE)
wLq95 <- quantile(wednesday_totals$Lunch,.95, na.rm = TRUE)
wLmeanx <- mean(wednesday_totals$Lunch, na.rm = TRUE)
wLx.dens <- density(wednesday_totals$Lunch, na.rm = TRUE)
wLdf.dens <- data.frame(x = wLx.dens$x, y = wLx.dens$y)
WeL <- wL + geom_area(data = subset(wLdf.dens, x >= wLq5 & x <= wLq95), 
                      aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = wLmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("wednesday Lunch")

#Outside Play
wOP <- ggplot() + geom_density(data = wednesday_totals, aes(x = Outside.Play))

wOPq5 <- quantile(wednesday_totals$Outside.Play,.05, na.rm = TRUE)
wOPq95 <- quantile(wednesday_totals$Outside.Play,.95, na.rm = TRUE)
wOPmeanx <- mean(wednesday_totals$Outside.Play, na.rm = TRUE)
wOPx.dens <- density(wednesday_totals$Outside.Play, na.rm = TRUE)
wOPdf.dens <- data.frame(x = wOPx.dens$x, y = wOPx.dens$y)
WeOP <- wOP + geom_area(data = subset(wOPdf.dens, x >= wOPq5 & x <= wOPq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = wOPmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("wednesday Outside Play")

#Prize Store
wPS <- ggplot() + geom_density(data = wednesday_totals, aes(x = Prize.Store))

wPSq5 <- quantile(wednesday_totals$Prize.Store,.05, na.rm = TRUE)
wPSq95 <- quantile(wednesday_totals$Prize.Store,.95, na.rm = TRUE)
wPSmeanx <- mean(wednesday_totals$Prize.Store, na.rm = TRUE)
wPSx.dens <- density(wednesday_totals$Prize.Store, na.rm = TRUE)
wPSdf.dens <- data.frame(x = wPSx.dens$x, y = wPSx.dens$y)
WePS <- wPS + geom_area(data = subset(wPSdf.dens, x >= wPSq5 & x <= wPSq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = wPSmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("wednesday Prize Store")

#Stacked Graphs
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Graphs with confidence intervals and mean/Average/Wednesday Events stacked.png",width=800, height=600) #Opens a PNG file#

grid.newpage()
grid.draw(rbind(ggplotGrob(WeWU), ggplotGrob(WeMM), ggplotGrob(WeL), ggplotGrob(WeOP), ggplotGrob(WePS), size = "last"))
dev.off()

#Thursday
#Warm Up
rWU <- ggplot() + geom_density(data = thursday_totals, aes(x = Warm.Up))

rWUq5 <- quantile(thursday_totals$Warm.Up,.05, na.rm = TRUE)
rWUq95 <- quantile(thursday_totals$Warm.Up,.95, na.rm = TRUE)
rWUmeanx <- mean(thursday_totals$Warm.Up, na.rm = TRUE)
rWUx.dens <- density(thursday_totals$Warm.Up, na.rm = TRUE)
rWUdf.dens <- data.frame(x = rWUx.dens$x, y = rWUx.dens$y)
ReWU <- rWU + geom_area(data = subset(rWUdf.dens, x >= rWUq5 & x <= rWUq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = rWUmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("thursday Warm Up")


#Morning Meeting

rMM <- ggplot() + geom_density(data = thursday_totals, aes(x = Morning.Meeting))

rMMq5 <- quantile(thursday_totals$Morning.Meeting,.05, na.rm = TRUE)
rMMq95 <- quantile(thursday_totals$Morning.Meeting,.95, na.rm = TRUE)
rMMmeanx <- mean(thursday_totals$Morning.Meeting, na.rm = TRUE)
rMMx.dens <- density(thursday_totals$Morning.Meeting, na.rm = TRUE)
rMMdf.dens <- data.frame(x = rMMx.dens$x, y = rMMx.dens$y)
ReMM <- rMM + geom_area(data = subset(rMMdf.dens, x >= rMMq5 & x <= rMMq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = rMMmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("thursday Morning Meeting")

#Lunch
rL <- ggplot() + geom_density(data = thursday_totals, aes(x = Lunch))

rLq5 <- quantile(thursday_totals$Lunch,.05, na.rm = TRUE)
rLq95 <- quantile(thursday_totals$Lunch,.95, na.rm = TRUE)
rLmeanx <- mean(thursday_totals$Lunch, na.rm = TRUE)
rLx.dens <- density(thursday_totals$Lunch, na.rm = TRUE)
rLdf.dens <- data.frame(x = rLx.dens$x, y = rLx.dens$y)
ReL <- rL + geom_area(data = subset(rLdf.dens, x >= rLq5 & x <= rLq95), 
                      aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = rLmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("thursday Lunch")

#Outside Play
rOP <- ggplot() + geom_density(data = thursday_totals, aes(x = Outside.Play))

rOPq5 <- quantile(thursday_totals$Outside.Play,.05, na.rm = TRUE)
rOPq95 <- quantile(thursday_totals$Outside.Play,.95, na.rm = TRUE)
rOPmeanx <- mean(thursday_totals$Outside.Play, na.rm = TRUE)
rOPx.dens <- density(thursday_totals$Outside.Play, na.rm = TRUE)
rOPdf.dens <- data.frame(x = rOPx.dens$x, y = rOPx.dens$y)
ReOP <- rOP + geom_area(data = subset(rOPdf.dens, x >= rOPq5 & x <= rOPq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = rOPmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("thursday Outside Play")

#Prize Store
rPS <- ggplot() + geom_density(data = thursday_totals, aes(x = Prize.Store))

rPSq5 <- quantile(thursday_totals$Prize.Store,.05, na.rm = TRUE)
rPSq95 <- quantile(thursday_totals$Prize.Store,.95, na.rm = TRUE)
rPSmeanx <- mean(thursday_totals$Prize.Store, na.rm = TRUE)
rPSx.dens <- density(thursday_totals$Prize.Store, na.rm = TRUE)
rPSdf.dens <- data.frame(x = rPSx.dens$x, y = rPSx.dens$y)
RePS <- rPS + geom_area(data = subset(rPSdf.dens, x >= rPSq5 & x <= rPSq95), 
                        aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = rPSmeanx, show.legend = TRUE) +
  scale_x_continuous(limits = c(60, 85), name = "Average Child Segment dB SPL") + ggtitle("thursday Prize Store")

#Stacked Graphs
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Graphs with confidence intervals and mean/Average/thursday Events stacked.png",width=800, height=600) #Opens a PNG file#

grid.newpage()
grid.draw(rbind(ggplotGrob(ReWU), ggplotGrob(ReMM), ggplotGrob(ReL), ggplotGrob(ReOP), ggplotGrob(RePS), size = "last"))
dev.off()

##BOXPLOTS##
#Full Day
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Total avg audio boxplot.png",width=800, height=600) #Opens a PNG file#

anovadata <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/Distributions/Totals.csv")
boxplot(anovadata, main = "Vocalization Average dB SPL vs Day", xlab = "Day", ylab = "Vocalization Average dB SPL")
legend(x=1, y= 63, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 8, bty = "n", cex = 1)
dev.off()

#Warm Up
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Warm Up avg audio boxplot.png",width=800, height=600) #Opens a PNG file#
wuanovadata <- cbind(monday_totals$Warm.Up, tuesday_totals$Warm.Up, wednesday_totals$Warm.Up, thursday_totals$Warm.Up)
colnames(wuanovadata) <- c("Monday", "Tuesday", "Wednesday", "Thursday")
boxplot(wuanovadata, main = "Vocalization Average dB SPL vs Day-Warm Up", xlab = "Day", ylab = "Vocalization Average dB SPL")
legend(x=1, y= 70, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 8, bty = "n", cex = 1)
dev.off()

#Morning Meeting
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Morning Meeting avg audio boxplot.png",width=800, height=600) #Opens a PNG file#
mmanovadata <- cbind(monday_totals$Morning.Meeting, tuesday_totals$Morning.Meeting, wednesday_totals$Morning.Meeting, thursday_totals$Morning.Meeting)
colnames(mmanovadata) <- c("Monday", "Tuesday", "Wednesday", "Thursday")
boxplot(mmanovadata, main = "Vocalization Average dB SPL vs Day-Morning Meeting", xlab = "Day", ylab = "Vocalization Average dB SPL")
legend(x=1, y= 70, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 8, bty = "n", cex = 1)
dev.off()

#Lunch
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Lunch avg audio boxplot.png",width=800, height=600) #Opens a PNG file#

Lanovadata <- cbind(monday_totals$Lunch, tuesday_totals$Lunch, wednesday_totals$Lunch, thursday_totals$Lunch)
colnames(Lanovadata) <- c("Monday", "Tuesday", "Wednesday", "Thursday")
boxplot(Lanovadata, main = "Vocalization Average dB SPL vs Day-Lunch", xlab = "Day", ylab = "Vocalization Average dB SPL")
legend(x=1, y= 70, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 8, bty = "n", cex = 1)
dev.off()

#Outside Play
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Outside Play avg audio boxplot.png",width=800, height=600) #Opens a PNG file#

OPanovadata <- cbind(monday_totals$Outside.Play, tuesday_totals$Outside.Play, wednesday_totals$Outside.Play, thursday_totals$Outside.Play)
colnames(OPanovadata) <- c("Monday", "Tuesday", "Wednesday", "Thursday")
boxplot(OPanovadata, main = "Vocalization Average dB SPL vs Day-Outside Play", xlab = "Day", ylab = "Vocalization Average dB SPL")
legend(x=1, y= 70, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 8, bty = "n", cex = 1)
dev.off()

#Prize Store
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Prize Store avg audio boxplot.png",width=800, height=600) #Opens a PNG file#

psanovadata <- cbind(monday_totals$Prize.Store, tuesday_totals$Prize.Store, wednesday_totals$Prize.Store, thursday_totals$Prize.Store)
colnames(psanovadata) <- c("Monday", "Tuesday", "Wednesday", "Thursday")
boxplot(psanovadata, main = "Vocalization Average dB SPL vs Day-Prize Store", xlab = "Day", ylab = "Vocalization Average dB SPL")
legend(x=1, y= 70, c("Median", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 8, bty = "n", cex = 1)
dev.off()

##Boxplots Using Mean Values
monmean <- aggregate(monday_totals[, 6], list(monday_totals$ursi), mean)
tuesmean <- aggregate(tuesday_totals[, 6], list(tuesday_totals$ursi), mean)
wedsmean <- aggregate(wednesday_totals[, 6], list(wednesday_totals$ursi), mean)
thursmean <- aggregate(thursday_totals[, 6], list(thursday_totals$ursi), mean)

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Boxplots/Mean Avg Audio Level.png",width=1400, height=600) #Opens a PNG file#
par(mfrow=c(1,2)) #Splits png into 2 sections

boxdata <- cbind(monmean[, 2], tuesmean[, 2], wedsmean[, 2], thursmean[, 2])
colnames(boxdata) = c("Monday", "Tuesday", "Wednesday", "Thursday")
boxplot(boxdata, main = "Mean Average dB SPL by Day", xlab = "Day", ylab = "Average Child Segment dB SPL")
legend(x=0.5, y= 76, c("Median of the group", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)

difdata <- cbind(boxdata[,1]-boxdata[,2], boxdata[,1]- boxdata[,3], boxdata[,1]- boxdata[,4])
colnames(difdata)=c("Monday-Tuesday","Monday-Wednesday", "Monday-Thursday")
boxplot(difdata, main = "Difference in Mean Average dB SPL by Day", xlab = "Comparison", ylab = "Average Child Segment dB SPL")
legend(x=.5, y= 5, c("Median of the group", "first and third quartiles"), lty=c(1,1), lwd = c(3,1) , text.font = 10, bty = "n", cex = 1)

dev.off()


#####STATS#####
##Repeated Measures ANOVA
#All Days
myLevels= c(1,2,3,4)
groupFactors=as.factor(myLevels)
groupFrame <- data.frame(groupFactors)
boxdata <- as.matrix(boxdata)
myModel=lm(boxdata~1)
analysis <- Anova(myModel, idata = groupFrame, idesign = ~groupFactors)
summary(analysis)

#just mon and thurs
boxdata <- as.data.frame(boxdata)
boxdata$Tuesday <- NULL
boxdata$Wednesday <- NULL
myLevels= c(1,2)
groupFactors=as.factor(myLevels)
groupFrame <- data.frame(groupFactors)
boxdata <- as.matrix(boxdata)
myModel=lm(boxdata~1)
analysis2 <- Anova(myModel, idata = groupFrame, idesign = ~groupFactors)
summary(analysis2)

##Normality tests
#Shapiro-wilk

shapiro.test(monday_totals$Full.Day)
shapiro.test(tuesday_totals$Full.Day)
shapiro.test(wednesday_totals$Full.Day)
shapiro.test(thursday_totals$Full.Day)

#K-S
library("Matching")
#Full Day
ks.boot(monday_totals$Full.Day, thursday_totals$Full.Day)

#Warm Up
ks.boot(monday_totals$Warm.Up, thursday_totals$Warm.Up)

#Morning Meeting
ks.boot(monday_totals$Morning.Meeting, thursday_totals$Morning.Meeting)

#Lunch
ks.boot(monday_totals$Lunch, thursday_totals$Lunch)

#Outside Play
ks.boot(monday_totals$Outside.Play, thursday_totals$Outside.Play)

#Prize Store
ks.boot(monday_totals$Prize.Store, thursday_totals$Prize.Store)


##DENSITY PLOTS WITH Interquartile Range and Median
#monday average
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Interquartile Range and Median Graphs/monday interquartile.png",width=800, height=600) #Opens a PNG file#

m <- ggplot() + geom_density(data = monday_totals, aes(x = Full.Day))

mq25 <- quantile(monday_totals$Full.Day,.25)
mq75 <- quantile(monday_totals$Full.Day,.75)
mmedianx <- median(monday_totals$Full.Day)
mx.dens <- density(monday_totals$Full.Day)
mdf.dens <- data.frame(x = mx.dens$x, y = mx.dens$y)
IQM <- m + geom_area(data = subset(mdf.dens, x >= mq25 & x <= mq75), 
                       aes(x=x,y=y), fill = 'red', alpha = 0.1) +
  geom_vline(xintercept = mmedianx, show.legend = TRUE) +
  scale_x_continuous(limits = c(70, 85), name = "Average Child Segment dB SPL") + ggtitle("Monday")
IQM

dev.off()
#tuesday
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Interquartile Range and Median Graphs/tuesday interquartile.png",width=800, height=600) #Opens a PNG file#

t <- ggplot() + geom_density(data = tuesday_totals, aes(x = Full.Day))

tq25 <- quantile(tuesday_totals$Full.Day,.25)
tq75 <- quantile(tuesday_totals$Full.Day,.75)
tmedianx <- median(tuesday_totals$Full.Day)
tx.dens <- density(tuesday_totals$Full.Day)
tdf.dens <- data.frame(x = tx.dens$x, y = tx.dens$y)
IQT <- t + geom_area(data = subset(tdf.dens, x >= tq25 & x <= tq75), 
                       aes(x=x,y=y), fill = 'blue', alpha = 0.1) +
  geom_vline(xintercept = tmedianx, show.legend = TRUE) +
  scale_x_continuous(limits = c(70, 85), name = "Average Child Segment dB SPL") + ggtitle("Tuesday")
IQT

dev.off()
#wednesday
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Interquartile Range and Median Graphs/wednesday interquartile.png",width=800, height=600) #Opens a PNG file#

w <- ggplot() + geom_density(data = wednesday_totals, aes(x = Full.Day))

wq25 <- quantile(wednesday_totals$Full.Day,.25)
wq75 <- quantile(wednesday_totals$Full.Day,.75)
wmedianx <- median(wednesday_totals$Full.Day)
wx.dens <- density(wednesday_totals$Full.Day)
wdf.dens <- data.frame(x = wx.dens$x, y = wx.dens$y)
IQW <- w + geom_area(data = subset(wdf.dens, x >= wq25 & x <= wq75), 
                       aes(x=x,y=y), fill = 'purple', alpha = 0.1) +
  geom_vline(xintercept = wmedianx, show.legend = TRUE) +
  scale_x_continuous(limits = c(70, 85), name = "Average Child Segment dB SPL") + ggtitle("Wednesday")

IQW

dev.off()
#Thursday
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Interquartile Range and Median Graphs/thursday interquartile.png",width=800, height=600) #Opens a PNG file#

r <- ggplot() + geom_density(data = thursday_totals, aes(x = Full.Day))

rq25 <- quantile(thursday_totals$Full.Day,.25)
rq75 <- quantile(thursday_totals$Full.Day,.75)
rmedianx <- median(thursday_totals$Full.Day)
rx.dens <- density(thursday_totals$Full.Day)
rdf.dens <- data.frame(x = rx.dens$x, y = rx.dens$y)
IQR <- r + geom_area(data = subset(rdf.dens, x >= rq25 & x <= rq75), 
                       aes(x=x,y=y), fill = 'green', alpha = 0.1) +
  geom_vline(xintercept = rmedianx, show.legend = TRUE) + 
  scale_x_continuous(limits = c(70, 85), name = "Average Child Segment dB SPL") + ggtitle("Thursday")
IQR

dev.off()

#Week Long Stacked

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Density Plots/Interquartile Range and Median Graphs/avg week long stacked.png",width=800, height=600) #Opens a PNG file#

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(IQM), ggplotGrob(IQT), ggplotGrob(IQW), ggplotGrob(IQR), size = "last"))
dev.off()

##Graph with all subjects and standard error

chart = monday_totals %>%ggplot(aes())
