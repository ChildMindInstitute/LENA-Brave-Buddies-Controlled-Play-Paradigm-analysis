##ICC for LENA Controlled play paradigm data
##Jacob Stroud, 11/11/2016
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean")

library("ICC")
library("dplyr")

#Load in data files
filenames <- list.files(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Clean", pattern = "M00*", full.names = FALSE)
numfiles <- length(filenames)
for(i in c(1:numfiles)){
  filenames[i] <- paste("",filenames[i],sep="")  
  assign(gsub("[.]csv$","",filenames[i]),read.csv(filenames[i], header=TRUE, stringsAsFactors = FALSE))
}
#Create a list in order to act on all files at once
full_list = list(M00412434_controlledplay_clean, M00413464_controlledplay_clean, M00426908_controlledplay_clean, M00440011_controlledplay_clean, 
                 M00440728_controlledplay_clean, M00409047_controlledplay_clean, M00403142_controlledplay_clean,
                 M00441664_controlledplay_clean, M00445929_controlledplay_clean, M00475465_controlledplay_clean, M00494954_controlledplay_clean)

###VOCALIZATION COUNT
voxdf <- do.call(rbind, lapply(full_list, function(x) x[, c(1, 2, 8)])) #creates a df with just session(1), ursi(2), and vox count(8)

#Filter out B and C blocks
target <- c("A1", "A2", "A3")
voxdf <- filter(voxdf, Session %in% target)

#Run ICC considering 6 separate 5 minute A blocks
ICCbare(URSI, Child_Voc_Count, voxdf)

voxdf <- voxdf %>% group_by(URSI, Session) %>% mutate(avg = mean(Child_Voc_Count)) #creates column with average of each 10 minute A block

Nth.delete<-function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),] #function to delete every other row
avgvoxdf <- Nth.delete(voxdf, 2) #delete every other row so each 10 minute A block is considered one observation
avgvoxdf$Child_Voc_Count <- NULL #remove raw vox count column
ICCbare(URSI, avg, avgvoxdf) #run ICC comparing the average of each 10 minute A block


###VOCALIZATION DURATION
durdf <- do.call(rbind, lapply(full_list, function(x) x[, c(1, 2, 10)]))

target <- c("A1", "A2", "A3")
durdf <- filter(durdf, Session %in% target)

ICCbare(URSI, Child_Voc_Duration, durdf)

durdf <- durdf %>% group_by(URSI, Session) %>% mutate(avg = mean(Child_Voc_Duration)) #creates column with average of each 10 minute A block

avgdurdf <- Nth.delete(durdf, 2) #delete every other row so each 10 minute A block is considered one observation
avgdurdf$Child_Voc_Duration <- NULL #remove raw vox count column
ICCbare(URSI, avg, avgdurdf) #run ICC comparing the average of each 10 minute A block

###CONVERSATIONAL TURNS
turndf <- do.call(rbind, lapply(full_list, function(x) x[, c(1, 2, 7)]))

target <- c("A1", "A2", "A3")
turndf <- filter(turndf, Session %in% target)

ICCbare(URSI, Turn_Count, turndf)

turndf <- turndf %>% group_by(URSI, Session) %>% mutate(avg = mean(Turn_Count)) #creates column with average of each 10 minute A block

avgturndf <- Nth.delete(turndf, 2) #delete every other row so each 10 minute A block is considered one observation
avgturndf$Turn_Count <- NULL #remove raw vox count column
ICCbare(URSI, avg, avgturndf) #run ICC comparing the average of each 10 minute A block

###SEGMENT DURATION
segdf <- do.call(rbind, lapply(full_list, function(x) x[, c(1, 2, 9)]))

target <- c("A1", "A2", "A3")
segdf <- filter(segdf, Session %in% target)

ICCbare(URSI, CHN, segdf)

segdf <- segdf %>% group_by(URSI, Session) %>% mutate(avg = mean(CHN)) #creates column with average of each 10 minute A block

avgsegdf <- Nth.delete(segdf, 2) #delete every other row so each 10 minute A block is considered one observation
avgsegdf$CHN <- NULL #remove raw vox count column
ICCbare(URSI, avg, avgsegdf) #run ICC comparing the average of each 10 minute A block

###AVG dB SPL
#Load in data files
setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Segment Level Detail")
audfilenames <- list.files(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/Segment Level Detail", pattern = "M00*", full.names = FALSE)
audnumfiles <- length(audfilenames)
for(i in c(1:audnumfiles)){
  audfilenames[i] <- paste("",audfilenames[i],sep="")  
  assign(gsub("[.]csv$","",audfilenames[i]),read.csv(audfilenames[i], header=TRUE, stringsAsFactors = FALSE))
}
#Create a list in order to act on all files at once
aud_list = list(M00412434_controlledplay_detailed, M00413464_controlledplay_detailed, M00426908_controlledplay_detailed, M00440011_controlledplay_detailed, 
                 M00440728_controlledplay_detailed, M00409047_controlledplay_detailed, M00403142_controlledplay_detailed, M00402147_controlledplay_detailed,
                 M00441664_controlledplay_detailed, M00445929_controlledplay_detailed, M00475465_controlledplay_detailed, M00494954_controlledplay_detailed)

avgdf <- do.call(rbind, lapply(aud_list, function(x) x[, c(1, 2, 21)])) #creates a df with just session(1), ursi(2), and audio level(21)

#Filter out B and C blocks
target <- c("A1", "A2", "A3")
avgdf <- filter(avgdf, Session %in% target)

#ICC Comparing each vocalization volume
ICCbare(URSI, Average_SignalLevel, avgdf)

avgdf <- avgdf %>% group_by(URSI, Session) %>% mutate(avg = mean(Average_SignalLevel)) #creates column with average of each 10 minute A block
avgdf$Average_SignalLevel <- NULL
avgdf <- unique(avgdf) #deletes duplicate rows

#ICC Comparing average volume of each 10 minute block
ICCbare(URSI, avg, avgdf) #run ICC comparing the average of each 10 minute A block

##PEAK
peakdf <- do.call(rbind, lapply(aud_list, function(x) x[, c(1, 2, 22)])) #creates a df with just session(1), ursi(2), and audio level(21)

#Filter out B and C blocks
target <- c("A1", "A2", "A3")
peakdf <- filter(peakdf, Session %in% target)

#ICC Comparing each vocalization peak volume
ICCbare(URSI, Peak_SignalLevel, peakdf)

peakdf <- peakdf %>% group_by(URSI, Session) %>% mutate(avg = mean(Peak_SignalLevel)) #creates column with average of each 10 minute A block
peakdf$Peak_SignalLevel <- NULL
peakdf <- unique(peakdf) #deletes duplicate rows

#ICC Comparing average peak volume of each 10 minute block
ICCbare(URSI, avg, peakdf) #run ICC comparing the average of each 10 minute A block


