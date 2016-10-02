library(car)

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies")

#SMQ Scores
scores <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/R Scripts/SMQ Correlations/BB_SMQs_demos_Cleaned.csv")

##LENA data
data <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/AvgTotalsCompiled.csv")

##At School###########
  ##Vocalizations
plot(scores$SMQ.At.School.Section.Average.Score, data$Mean.Vocalizations, xlab = "At School SMQ Subsection Average Score", ylab = "Mean Vocalizations", main = "School Subsection")

abline(lm(data$Mean.Vocalizations ~ scores$SMQ.At.School.Section.Average.Score))

cor.test(scores$SMQ.At.School.Section.Average.Score, data$Mean.Vocalizations, use = 'everything', method = c("pearson"))

  ##Avg dB SPL##
plot(scores$SMQ.At.School.Section.Average.Score, data$Mean.Avg.SL)

abline(lm(data$Mean.Avg.SL ~ scores$SMQ.At.School.Section.Average.Score))

cor.test(scores$SMQ.At.School.Section.Average.Score, data$Mean.Avg.SL, use = 'everything', method = c("pearson"))


  ##Peak dB SPL
plot(scores$SMQ.At.School.Section.Average.Score, data$Mean.Peak.SL)

abline(lm(data$Mean.Peak.SL ~ scores$SMQ.At.School.Section.Average.Score))

cor.test(scores$SMQ.At.School.Section.Average.Score, data$Mean.Peak.SL, use = 'everything', method = c("pearson"))


##At Home##########
  ##Vocalizations
plot(scores$SMQ.Home.Family.Section.Average.Score, data$Mean.Vocalizations)

abline(lm(data$Mean.Vocalizations ~ scores$SMQ.Home.Family.Section.Average.Score))

cor.test(scores$SMQ.Home.Family.Section.Average.Score, data$Mean.Vocalizations, use = 'everything', method = c("pearson"))


  ##AVG dB SPL
plot(scores$SMQ.Home.Family.Section.Average.Score, data$Mean.Avg.SL)

abline(lm(data$Mean.Avg.SL ~ scores$SMQ.Home.Family.Section.Average.Score))

cor.test(scores$SMQ.Home.Family.Section.Average.Score, data$Mean.Avg.SL, use = 'everything', method = c("pearson"))


  ##Peak dB SPL
plot(scores$SMQ.Home.Family.Section.Average.Score, data$Mean.Peak.SL)

abline(lm(data$Mean.Peak.SL ~ scores$SMQ.Home.Family.Section.Average.Score))

cor.test(scores$SMQ.Home.Family.Section.Average.Score, data$Mean.Peak.SL, use = 'everything', method = c("pearson"))


##Social Situations########
  ##Vocalizations
plot(scores$SMQ.In.Social.Situations..Outside.of.School..Section.Average.Score, data$Mean.Vocalizations)

abline(lm(data$Mean.Vocalizations ~ scores$SMQ.In.Social.Situations..Outside.of.School..Section.Average.Score))

cor.test(scores$SMQ.In.Social.Situations..Outside.of.School..Section.Average.Score, data$Mean.Vocalizations, use = 'everything', method = c("pearson"))


  ##AVG dB SPL
plot(scores$SMQ.In.Social.Situations..Outside.of.School..Section.Average.Score, data$Mean.Avg.SL)

abline(lm(data$Mean.Avg.SL ~ scores$SMQ.In.Social.Situations..Outside.of.School..Section.Average.Score))

cor.test(scores$SMQ.In.Social.Situations..Outside.of.School..Section.Average.Score, data$Mean.Avg.SL, use = 'everything', method = c("pearson"))


  ##Peak dB SPL
plot(scores$SMQ.In.Social.Situations..Outside.of.School..Section.Average.Score, data$Mean.Peak.SL)

abline(lm(data$Mean.Peak.SL ~ scores$SMQ.In.Social.Situations..Outside.of.School..Section.Average.Score))

cor.test(scores$SMQ.In.Social.Situations..Outside.of.School..Section.Average.Score, data$Mean.Peak.SL, use = 'everything', method = c("pearson"))


##Interference total score########
  ##Vocalizations
plot(scores$SMQ.Interference.Distress.Raw.Total.Score, data$Mean.Vocalizations)

abline(lm(data$Mean.Vocalizations ~ scores$SMQ.Interference.Distress.Raw.Total.Score))

cor.test(scores$SMQ.Interference.Distress.Raw.Total.Score, data$Mean.Vocalizations, use = 'everything', method = c("pearson"))

  ##AVG dB SPL
plot(scores$SMQ.Interference.Distress.Raw.Total.Score, data$Mean.Avg.SL)

abline(lm(data$Mean.Avg.SL ~ scores$SMQ.Interference.Distress.Raw.Total.Score))

cor.test(scores$SMQ.Interference.Distress.Raw.Total.Score, data$Mean.Avg.SL, use = 'everything', method = c("pearson"))

  ##Peak dB SPL
plot(scores$SMQ.Interference.Distress.Raw.Total.Score, data$Mean.Peak.SL)

abline(lm(data$Mean.Peak.SL ~ scores$SMQ.Interference.Distress.Raw.Total.Score))

cor.test(scores$SMQ.Interference.Distress.Raw.Total.Score, data$Mean.Peak.SL, use = 'everything', method = c("pearson"))

####Before and After Comparison################

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Data for Analysis Activity")

#Create a list of all the subjects in the directory 
subjects = list.files(getwd())

#Create two empty data frames with the following columns:
#ursi, day (1-4), activity, conversational turns, # of vocalizations, chn?, vocalization duration, avg signal, and peak signal
#brave_buddies contains data for each activity, brave_buddies_all contains aggregate data for the whole day.
brave_buddies = data.frame(ursi=character(), day=numeric(), activity=character(), turn_count=numeric(), child_voc_count=numeric(),
                           chn=numeric(), child_voc_duration=numeric(), avg_signal=numeric(), peak_signal=numeric(),
                           stringsAsFactors=FALSE)
brave_buddies_all = data.frame(ursi=character(), day=numeric(), activity=character(), turn_count=numeric(), child_voc_count=numeric(),
                               chn=numeric(), child_voc_duration=numeric(), avg_signal=numeric(), peak_signal=numeric(),
                               stringsAsFactors=FALSE)

#Note: Data frames contains data of different types (characters and numerics), matrices contain data of the same type

#Run a for loop to fill the two data frames
for (i in 1:length(subjects))
{
  days = list.files(subjects[i])
  for (j in 1:length(days))
  {
    #Create a data frame from .csv for a subject's day
    day_data = read.csv(paste(subjects[i], days[j], sep="/"))
    day_data_full = subset(day_data, day_data$Activity != "", 
                           select=c("Turn_Count", "Child_Voc_Count", "CHN", "Child_Voc_Duration", "Average_SignalLevel",
                                    "Peak_SignalLevel", "Activity"))
    #Subset relevant columns from .csv data
    day_data = subset(day_data, day_data$Activity %in% c("Warm Up", "Morning Meeting", "Lunch", "Outside Play", "Prize Store") ,
                      select=c("Turn_Count", "Child_Voc_Count", "CHN", "Child_Voc_Duration", "Average_SignalLevel",
                               "Peak_SignalLevel", "Activity"))
    #For each activity, create a single-row data frame containing average value for each measure
    warmup = data.frame(ursi=subjects[i], day=j, activity="Warmup", turn_count=mean(day_data$Turn_Count[which(day_data$Activity == "Warm Up")]), 
                        child_voc_count=mean(day_data$Child_Voc_Count[which(day_data$Activity == "Warm Up")]),
                        chn=mean(day_data$CHN[which(day_data$Activity == "Warm Up")]),
                        child_voc_duration=mean(day_data$Child_Voc_Duration[which(day_data$Activity == "Warm Up")]),
                        avg_signal=mean(day_data$Average_SignalLevel[which(day_data$Activity == "Warm Up")]),
                        peak_signal=mean(day_data$Peak_SignalLevel[which(day_data$Activity == "Warm Up")]))
    morning = data.frame(ursi=subjects[i], day=j, activity="Morning Meeting", turn_count=mean(day_data$Turn_Count[which(day_data$Activity == "Morning Meeting")]), 
                         child_voc_count=mean(day_data$Child_Voc_Count[which(day_data$Activity == "Morning Meeting")]),
                         chn=mean(day_data$CHN[which(day_data$Activity == "Morning Meeting")]),
                         child_voc_duration=mean(day_data$Child_Voc_Duration[which(day_data$Activity == "Morning Meeting")]),
                         avg_signal=mean(day_data$Average_SignalLevel[which(day_data$Activity == "Morning Meeting")]),
                         peak_signal=mean(day_data$Peak_SignalLevel[which(day_data$Activity == "Morning Meeting")]))
    lunch = data.frame(ursi=subjects[i], day=j, activity="Lunch", turn_count=mean(day_data$Turn_Count[which(day_data$Activity == "Lunch")]), 
                       child_voc_count=mean(day_data$Child_Voc_Count[which(day_data$Activity == "Lunch")]),
                       chn=mean(day_data$CHN[which(day_data$Activity == "Lunch")]),
                       child_voc_duration=mean(day_data$Child_Voc_Duration[which(day_data$Activity == "Lunch")]),
                       avg_signal=mean(day_data$Average_SignalLevel[which(day_data$Activity == "Lunch")]),
                       peak_signal=mean(day_data$Peak_SignalLevel[which(day_data$Activity == "Lunch")]))
    outside = data.frame(ursi=subjects[i], day=j, activity="Outside Play", turn_count=mean(day_data$Turn_Count[which(day_data$Activity == "Outside Play")]), 
                         child_voc_count=mean(day_data$Child_Voc_Count[which(day_data$Activity == "Outside Play")]),
                         chn=mean(day_data$CHN[which(day_data$Activity == "Outside Play")]),
                         child_voc_duration=mean(day_data$Child_Voc_Duration[which(day_data$Activity == "Outside Play")]),
                         avg_signal=mean(day_data$Average_SignalLevel[which(day_data$Activity == "Outside Play")]),
                         peak_signal=mean(day_data$Peak_SignalLevel[which(day_data$Activity == "Outside Play")]))
    prize = data.frame(ursi=subjects[i], day=j, activity="Prize Store", turn_count=mean(day_data$Turn_Count[which(day_data$Activity == "Prize Store")]), 
                       child_voc_count=mean(day_data$Child_Voc_Count[which(day_data$Activity == "Prize Store")]),
                       chn=mean(day_data$CHN[which(day_data$Activity == "Prize Store")]),
                       child_voc_duration=mean(day_data$Child_Voc_Duration[which(day_data$Activity == "Prize Store")]),
                       avg_signal=mean(day_data$Average_SignalLevel[which(day_data$Activity == "Prize Store")]),
                       peak_signal=mean(day_data$Peak_SignalLevel[which(day_data$Activity == "Prize Store")]))
    all = data.frame(ursi=subjects[i], day=j, activity="Whole Day", turn_count=mean(day_data_full$Turn_Count), 
                     child_voc_count=mean(day_data_full$Child_Voc_Count),
                     chn=mean(day_data_full$CHN),
                     child_voc_duration=mean(day_data_full$Child_Voc_Duration),
                     avg_signal=mean(day_data_full$Average_SignalLevel),
                     peak_signal=mean(day_data$Peak_SignalLevel))
    #Add the single-row data frame for each activity to brave_buddies and brave_buddies_all
    brave_buddies = rbind(brave_buddies, warmup)
    brave_buddies = rbind(brave_buddies, morning)
    brave_buddies = rbind(brave_buddies, lunch)
    brave_buddies = rbind(brave_buddies, outside)
    brave_buddies = rbind(brave_buddies, prize)
    brave_buddies_all = rbind(brave_buddies_all, all)
  }
}

#create a data frame containing only data during lunchtime
brave_buddies_lunch = subset(brave_buddies, brave_buddies$activity=="Lunch")

#add a new column to each data frame for groups (1-3). Right now, they are empty, but will be assigned values in the next step
brave_buddies$group = 0
brave_buddies_lunch$group = 0
brave_buddies_all$group = 0

#Assign group numbers to each data frame
for (l in 1:nrow(brave_buddies))
{
  if (brave_buddies$ursi[l] == "M00445929") {
    brave_buddies$group[l] = 1
  } else if (brave_buddies$ursi[l] == "M00412434" || brave_buddies$ursi[l] == "M00441664" || brave_buddies$ursi[l] =="M00402344" || brave_buddies$ursi[l] == "M00440011" || brave_buddies$ursi[l] == "M00494954"){
    brave_buddies$group[l] = 2
  } else{
    brave_buddies$group[l] = 3
  }
}

for (l in 1:nrow(brave_buddies_all))
{
  if (brave_buddies_all$ursi[l] == "M00445929") {
    brave_buddies_all$group[l] = 1
  } else if (brave_buddies_all$ursi[l] == "M00412434" || brave_buddies_all$ursi[l] == "M00441664" || brave_buddies_all$ursi[l] =="M00402344" || brave_buddies_all$ursi[l] == "M00440011" || brave_buddies_all$ursi[l] == "M00494954"){
    brave_buddies_all$group[l] = 2
  } else{
    brave_buddies_all$group[l] = 3
  }
}

for (l in 1:nrow(brave_buddies_lunch))
{
  if (brave_buddies_lunch$ursi[l] == "M00445929") {
    brave_buddies_lunch$group[l] = 1
  } else if (brave_buddies_lunch$ursi[l] == "M00412434" || brave_buddies_lunch$ursi[l] == "M00441664" || brave_buddies_lunch$ursi[l] =="M00402344" || brave_buddies_lunch$ursi[l] == "M00440011" || brave_buddies_lunch$ursi[l] == "M00494954"){
    brave_buddies_lunch$group[l] = 2
  } else{
    brave_buddies_lunch$group[l] = 3
  }
}


#Plot vocalizations for each participant for each day
bb_group_all = factor(brave_buddies_all$group) #Create a factor variable from group number

brave_buddies_lunch_diff = data.frame(ursi=character(), day=numeric(), activity=character(), diff_turn_count=numeric(), 
                                      diff_child_voc_count=numeric(), diff_chn=numeric(), diff_child_voc_duration=numeric(), 
                                      diff_avg_signal=numeric(), diff_peak_signal=numeric(), stringsAsFactors=FALSE)

for (l in 1:nrow(brave_buddies_lunch))
{
  if (brave_buddies_lunch[l, 2] > 1){
    #This loop runs though each row of the brave_buddies_lunch dataframe
    #If the row contains data for day 2, 3, or 4, we calculate the difference for each measure
    # between that day and day 1
    #We create temporary variables to store in a single-row data frame and then add to a 
    # new data frame called brave_buddies_lunch_diff
    temp_ursi=brave_buddies_lunch$ursi[l]
    temp_diff_turn_count=brave_buddies_lunch$turn_count[l] - brave_buddies_lunch$turn_count[which(brave_buddies_lunch$ursi== temp_ursi & brave_buddies_lunch$day==1)]
    temp_diff_child_voc_count=brave_buddies_lunch$child_voc_count[l] - brave_buddies_lunch$child_voc_count[which(brave_buddies_lunch$ursi== temp_ursi & brave_buddies_lunch$day==1)]
    temp_diff_chn=brave_buddies_lunch$chn[l] - brave_buddies_lunch$chn[which(brave_buddies_lunch$ursi== temp_ursi & brave_buddies_lunch$day==1)]
    temp_diff_child_voc_duration=brave_buddies_lunch$child_voc_duration[l] - brave_buddies_lunch$child_voc_duration[which(brave_buddies_lunch$ursi== temp_ursi & brave_buddies_lunch$day==1)]
    temp_diff_avg_signal=brave_buddies_lunch$avg_signal[l] - brave_buddies_lunch$avg_signal[which(brave_buddies_lunch$ursi==temp_ursi & brave_buddies_lunch$day==1)]
    temp_diff_peak_signal=brave_buddies_lunch$peak_signal[l] - brave_buddies_lunch$peak_signal[which(brave_buddies_lunch$ursi==temp_ursi & brave_buddies_lunch$day==1)]
    diff = data.frame(ursi=temp_ursi, day=brave_buddies_lunch$day[l] - 1, activity="Lunch", 
                      diff_turn_count= temp_diff_turn_count,
                      diff_child_voc_count=temp_diff_child_voc_count,
                      diff_chn=temp_diff_chn,
                      diff_child_voc_duration=temp_diff_child_voc_duration,
                      diff_avg_signal=temp_diff_avg_signal,
                      diff_peak_signal=temp_diff_peak_signal,
                      group = brave_buddies_lunch$group[l])
    brave_buddies_lunch_diff = rbind(brave_buddies_lunch_diff, diff)
  }
}

#Plot differences in vocalizations for each participant for lunchtime for each day
bb_group_lunch_diff = factor(brave_buddies_lunch_diff$group, levels = c(1,2,3)) #Create a factor variable from group number

#Read in SMQ scores
SMQdata <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/SMQ Follow up/BB_SMQs_longform.csv")

#Remove subjects without follow up data
SMQdata <- SMQdata[-c(3,8,15,20), ]
SMQdata$date_pre <- NULL

#Create an empty dataframe for differential data
SMQ_diff = data.frame(URSI=character(), diff_school=numeric(), 
                                      diff_home=numeric(), diff_social=numeric(), diff_intdis=numeric(), 
                                      stringsAsFactors=FALSE)

#loop to do the subtractions and place them into temporary data frames, then aggregate them all into a single SMQ_diff dataframe
for (k in 1:nrow(SMQdata))
{
  if (SMQdata[k, 6] > 1){
    temp_SMQ_ursi=SMQdata$URSI[k]
    temp_diff_school=SMQdata$SMQ_School[k] - SMQdata$SMQ_School[which(SMQdata$URSI== temp_SMQ_ursi & SMQdata$Timepoint==1)]
    temp_diff_home=SMQdata$SMQ_Home[k] - SMQdata$SMQ_Home[which(SMQdata$URSI== temp_SMQ_ursi & SMQdata$Timepoint==1)]
    temp_diff_social=SMQdata$SMQ_Social[k] - SMQdata$SMQ_Social[which(SMQdata$URSI== temp_SMQ_ursi & SMQdata$Timepoint==1)]
    temp_diff_intdis=SMQdata$SMQ_IntDis[k] - SMQdata$SMQ_IntDis[which(SMQdata$URSI== temp_SMQ_ursi & SMQdata$Timepoint==1)]
    SMQdiff = data.frame(URSI=temp_SMQ_ursi, 
                      diff_school= temp_diff_school,
                      diff_home=temp_diff_home,
                      diff_social=temp_diff_social,
                      diff_intdis=temp_diff_intdis
                      )
    SMQ_diff = rbind(SMQ_diff, SMQdiff)
  }
}

#Create a df with thurs-mon data from brave_buddies_lunch_diff
thursdiff <- data.frame(ursi=character(), diff_turn_count=numeric(), 
                                   diff_child_voc_count=numeric(), diff_chn=numeric(), diff_child_voc_duration=numeric(), 
                                   diff_avg_signal=numeric(), diff_peak_signal=numeric(), stringsAsFactors=FALSE)

for (m in 1:nrow(brave_buddies_lunch_diff))
{
  if (brave_buddies_lunch_diff[m, 2] == 3){
    temp_thurs_ursi=brave_buddies_lunch_diff$ursi[m]
    temp_thurs_turns=brave_buddies_lunch_diff$diff_turn_count[m]
    temp_thurs_count=brave_buddies_lunch_diff$diff_child_voc_count[m]
    temp_thurs_dur=brave_buddies_lunch_diff$diff_child_voc_duration[m]
    temp_thurs_chn=brave_buddies_lunch_diff$diff_chn[m]
    temp_thurs_avgsignal=brave_buddies_lunch_diff$diff_avg_signal[m]
    temp_thurs_peaksignal=brave_buddies_lunch_diff$diff_peak_signal[m]
    thurs_diff = data.frame(ursi = temp_thurs_ursi,
                            diff_turn_count = temp_thurs_turns,
                            diff_child_voc_count = temp_thurs_count,
                            diff_chn = temp_thurs_chn,
                            diff_child_voc_duration = temp_thurs_dur,
                            diff_avg_signal = temp_thurs_avgsignal,
                            diff_peak_signal = temp_thurs_peaksignal
                            )
    thursdiff = rbind(thursdiff, thurs_diff)
  }
}

#Remove subjects from thursdiff that were removed from smq_diff
thursdiff <- thursdiff[-c(3,8), ]

###########Correlations#########
###School SMQ Scores###
#vs Child Voc Count
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/SchoolvsVocalCount.png", width = 900, height = 700)
plot(SMQ_diff$diff_school, thursdiff$diff_child_voc_count)
abline(lm(thursdiff$diff_child_voc_count ~ SMQ_diff$diff_school))
dev.off()
cor.test(SMQ_diff$diff_school, thursdiff$diff_child_voc_count, method = c("pearson"))

#vs turn count
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/SchoolvsTurnCount.png", width = 900, height = 700)
plot(SMQ_diff$diff_school, thursdiff$diff_turn_count)
abline(lm(thursdiff$diff_turn_count ~ SMQ_diff$diff_school))
dev.off()

cor.test(SMQ_diff$diff_school, thursdiff$diff_turn_count, method = c("pearson"))

#vs chn
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/SchoolvsCHN.png", width = 900, height = 700)
plot(SMQ_diff$diff_school, thursdiff$diff_chn)
abline(lm(thursdiff$diff_chn ~ SMQ_diff$diff_school))
dev.off()
cor.test(SMQ_diff$diff_school, thursdiff$diff_chn, method = c("pearson"))

#vs voc duration 
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/SchoolvsDuration.png", width = 900, height = 700)
plot(SMQ_diff$diff_school, thursdiff$diff_child_voc_duration)
abline(lm(thursdiff$diff_child_voc_duration ~ SMQ_diff$diff_school))
dev.off()
cor.test(SMQ_diff$diff_school, thursdiff$diff_child_voc_duration, method = c("pearson"))

#vs avg volume
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/SchoolvsAvgVol.png", width = 900, height = 700)
plot(SMQ_diff$diff_school, thursdiff$diff_avg_signal)
abline(lm(thursdiff$diff_avg_signal ~ SMQ_diff$diff_school))
dev.off()
cor.test(SMQ_diff$diff_school, thursdiff$diff_avg_signal, method = c("pearson"))

#vs peak volume 
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/SchoolvsPeakVol.png", width = 900, height = 700)
plot(SMQ_diff$diff_school, thursdiff$diff_peak_signal)
abline(lm(thursdiff$diff_peak_signal ~ SMQ_diff$diff_school))
dev.off()
cor.test(SMQ_diff$diff_school, thursdiff$diff_peak_signal, method = c("pearson"))

##At home

#vs Child Voc Count
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/HomevsVocalizations.png", width = 900, height = 700)
plot(SMQ_diff$diff_home, thursdiff$diff_child_voc_count)
abline(lm(thursdiff$diff_child_voc_count ~ SMQ_diff$diff_home))
dev.off()
cor.test(SMQ_diff$diff_home, thursdiff$diff_child_voc_count, method = c("pearson"))

#vs turn count
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/HomevsTurns.png", width = 900, height = 700)
plot(SMQ_diff$diff_home, thursdiff$diff_turn_count)
abline(lm(thursdiff$diff_turn_count ~ SMQ_diff$diff_home))
dev.off()
cor.test(SMQ_diff$diff_home, thursdiff$diff_turn_count, method = c("pearson"))

#vs chn
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/HomevsCHN.png", width = 900, height = 700)
plot(SMQ_diff$diff_home, thursdiff$diff_chn)
abline(lm(thursdiff$diff_chn ~ SMQ_diff$diff_home))
dev.off()
cor.test(SMQ_diff$diff_home, thursdiff$diff_chn, method = c("pearson"))

#vs voc duration 
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/HomevsVocDuration.png", width = 900, height = 700)
plot(SMQ_diff$diff_home, thursdiff$diff_child_voc_duration)
abline(lm(thursdiff$diff_child_voc_duration ~ SMQ_diff$diff_home))
dev.off()
cor.test(SMQ_diff$diff_home, thursdiff$diff_child_voc_duration, method = c("pearson"))

#vs avg volume
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/HomevsAvgVol.png", width = 900, height = 700)
plot(SMQ_diff$diff_home, thursdiff$diff_avg_signal)
abline(lm(thursdiff$diff_avg_signal ~ SMQ_diff$diff_home))
dev.off()
cor.test(SMQ_diff$diff_home, thursdiff$diff_avg_signal, method = c("pearson"))

#vs peak volume 
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/HomevsPeakVol.png", width = 900, height = 700)
plot(SMQ_diff$diff_home, thursdiff$diff_peak_signal)
abline(lm(thursdiff$diff_peak_signal ~ SMQ_diff$diff_home))
dev.off()
cor.test(SMQ_diff$diff_home, thursdiff$diff_peak_signal, method = c("pearson"))

##Social Situations

#vs Child Voc Count
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/SocialvsVox.png", width = 900, height = 700)
plot(SMQ_diff$diff_social, thursdiff$diff_child_voc_count)
abline(lm(thursdiff$diff_child_voc_count ~ SMQ_diff$diff_social))
dev.off()
cor.test(SMQ_diff$diff_social, thursdiff$diff_child_voc_count, method = c("pearson"))

#vs turn count
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/SocialvsTurns.png", width = 900, height = 700)
plot(SMQ_diff$diff_social, thursdiff$diff_turn_count)
abline(lm(thursdiff$diff_turn_count ~ SMQ_diff$diff_social))
dev.off()
cor.test(SMQ_diff$diff_social, thursdiff$diff_turn_count, method = c("pearson"))

#vs chn
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/SocialvsCHN.png", width = 900, height = 700)
plot(SMQ_diff$diff_social, thursdiff$diff_chn)
abline(lm(thursdiff$diff_chn ~ SMQ_diff$diff_social))
dev.off()
cor.test(SMQ_diff$diff_social, thursdiff$diff_chn, method = c("pearson"))

#vs voc duration 
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/SocialvsVocDur.png", width = 900, height = 700)
plot(SMQ_diff$diff_social, thursdiff$diff_child_voc_duration)
abline(lm(thursdiff$diff_child_voc_duration ~ SMQ_diff$diff_social))
dev.off()
cor.test(SMQ_diff$diff_social, thursdiff$diff_child_voc_duration, method = c("pearson"))

#vs avg volume
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/SocialvsAvgVol.png", width = 900, height = 700)
plot(SMQ_diff$diff_social, thursdiff$diff_avg_signal)
abline(lm(thursdiff$diff_avg_signal ~ SMQ_diff$diff_social))
dev.off()
cor.test(SMQ_diff$diff_social, thursdiff$diff_avg_signal, method = c("pearson"))

#vs peak volume 
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/SocialvsPeakVol.png", width = 900, height = 700)
plot(SMQ_diff$diff_social, thursdiff$diff_peak_signal)
abline(lm(thursdiff$diff_peak_signal ~ SMQ_diff$diff_social))
dev.off()
cor.test(SMQ_diff$diff_social, thursdiff$diff_peak_signal, method = c("pearson"))

##Total Interference

#vs Child Voc Count
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/InterferencevsVox.png", width = 900, height = 700)
plot(SMQ_diff$diff_intdis, thursdiff$diff_child_voc_count)
abline(lm(thursdiff$diff_child_voc_count ~ SMQ_diff$diff_intdis))
dev.off()
cor.test(SMQ_diff$diff_intdis, thursdiff$diff_child_voc_count, method = c("pearson"))

#vs turn count
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/InterferencevsTurns.png", width = 900, height = 700)
plot(SMQ_diff$diff_intdis, thursdiff$diff_turn_count)
abline(lm(thursdiff$diff_turn_count ~ SMQ_diff$diff_intdis))
dev.off()
cor.test(SMQ_diff$diff_intdis, thursdiff$diff_turn_count, method = c("pearson"))

#vs chn
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/InterferencevsCHN.png", width = 900, height = 700)
plot(SMQ_diff$diff_intdis, thursdiff$diff_chn)
abline(lm(thursdiff$diff_chn ~ SMQ_diff$diff_intdis))
dev.off()
cor.test(SMQ_diff$diff_intdis, thursdiff$diff_chn, method = c("pearson"))

#vs voc duration 
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/InterferencevsVocDur.png", width = 900, height = 700)
plot(SMQ_diff$diff_intdis, thursdiff$diff_child_voc_duration)
abline(lm(thursdiff$diff_child_voc_duration ~ SMQ_diff$diff_intdis))
dev.off()
cor.test(SMQ_diff$diff_intdis, thursdiff$diff_child_voc_duration, method = c("pearson"))

#vs avg volume
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/InterferencevsAvgVol.png", width = 900, height = 700)
plot(SMQ_diff$diff_intdis, thursdiff$diff_avg_signal)
abline(lm(thursdiff$diff_avg_signal ~ SMQ_diff$diff_intdis))
dev.off()
cor.test(SMQ_diff$diff_intdis, thursdiff$diff_avg_signal, method = c("pearson"))

#vs peak volume 
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/SMQ Correlations/InterferencevsPeakVol.png", width = 900, height = 700)
plot(SMQ_diff$diff_intdis, thursdiff$diff_peak_signal)
abline(lm(thursdiff$diff_peak_signal ~ SMQ_diff$diff_intdis))
dev.off()
cor.test(SMQ_diff$diff_intdis, thursdiff$diff_peak_signal, method = c("pearson"))