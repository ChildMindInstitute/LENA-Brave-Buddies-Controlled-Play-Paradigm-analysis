#call in 24 csvs and relevant variables and create large dataframe with relevant data
library("plyr")
setwd("/Users/helen.xu/")
filelist <- list.files("Desktop/CPP/Control")
rm(CPP_data)
for (file in filelist){
  
  if (!exists("CPP_data")){
    CPP_data <- read.csv(file.path("Desktop/CPP/Control", file), header=TRUE, sep=",")
  } else if (exists("CPP_data")){
    temp_dataset <-read.csv(file.path("Desktop/CPP/Control", file), header=TRUE, sep=",")
    CPP_data<-rbind.fill(CPP_data, temp_dataset)
  }
  
}
CPP_data <- cbind(CPP_data, SM_dx=0)
#make SM dataframe
rm(CPP_data2)
filelist2 <- list.files("Desktop/CPP/SM")
for (file in filelist2){
  
  if (!exists("CPP_data2")){
    CPP_data2 <- read.csv(file.path("Desktop/CPP/SM", file), header=TRUE, sep=",")
  } else if (exists("CPP_data2")){
    temp_dataset <-read.csv(file.path("Desktop/CPP/SM", file), header=TRUE, sep=",")
    CPP_data2<-rbind.fill(CPP_data2, temp_dataset)
  }
  
}
CPP_data2 <- cbind(CPP_data2, SM_dx=1)
#combine dataframes
CPP_data <- rbind.fill(CPP_data, CPP_data2)
write.csv(CPP_data, file="Desktop/CPP/CPP_data_all.csv")
CPP_data <- CPP_data[, c(1,2,7,8,10,11,25,26,30)] #only relevant columns
#remove accidental A1,2,3
CPP_data$Session[CPP_data$Session=="A1"] <- "A"
CPP_data$Session[CPP_data$Session=="A2"] <- "A"
CPP_data$Session[CPP_data$Session=="A3"] <- "A"
CPP_data$Session <- factor(CPP_data$Session) #refactor to 3 levels
write.csv(CPP_data, file="Desktop/CPP/CPP_data.csv")
#change dataframe to average sessions and one URSI per row
tc <- cast(CPP_data, URSI + SM_dx ~ Session, mean, value=c("Turn_Count"))
cvc <- cast(CPP_data, URSI + SM_dx ~ Session, mean, value=c("Child_Voc_Count"))
cvd <- cast(CPP_data, URSI + SM_dx ~ Session, mean, value=c("Child_Voc_Duration"))
cnvd <- cast(CPP_data, URSI + SM_dx ~ Session, mean, value=c("Child_NonVoc_Duration"))
asl <- cast(CPP_data, URSI + SM_dx ~ Session, mean, value=c("Average_SignalLevel"))
psl <- cast(CPP_data, URSI + SM_dx ~ Session, mean, value=c("Peak_SignalLevel"))

write.csv(tc, file="Turn_Count.csv")
write.csv(cvc, file="Child_Voc_Count.csv")
write.csv(cvd, file="Child_Voc_Duration.csv")
write.csv(cnvd, file="Child_NonVoc_Duration.csv")
write.csv(asl, file="Average_SignalLevel.csv")
write.csv(psl, file="Peak_SignalLevel.csv")
#add in B-A and C-A variables
Turn_Count <- cbind(Turn_Count, A_B=(Turn_Count$A-Turn_Count$B))
Turn_Count <- cbind(Turn_Count, A_C=(Turn_Count$A-Turn_Count$C))
Child_Voc_Count <- cbind(Child_Voc_Count, A_B=(Child_Voc_Count$A-Child_Voc_Count$B))
Child_Voc_Count <- cbind(Child_Voc_Count, A_C=(Child_Voc_Count$A-Child_Voc_Count$C))
Child_Voc_Duration <- cbind(Child_Voc_Duration, A_B=(Child_Voc_Duration$A-Child_Voc_Duration$B))
Child_Voc_Duration <- cbind(Child_Voc_Duration, A_C=(Child_Voc_Duration$A-Child_Voc_Duration$C))
Child_NonVoc_Duration <- cbind(Child_NonVoc_Duration, A_B=(Child_NonVoc_Duration$A-Child_NonVoc_Duration$B))
Child_NonVoc_Duration <- cbind(Child_NonVoc_Duration, A_C=(Child_NonVoc_Duration$A-Child_NonVoc_Duration$C))
Average_SignalLevel <- cbind(Average_SignalLevel, A_B=(Average_SignalLevel$A-Average_SignalLevel$B))
Average_SignalLevel <- cbind(Average_SignalLevel, A_C=(Average_SignalLevel$A-Average_SignalLevel$C))
Peak_SignalLevel <- cbind(Peak_SignalLevel, A_B=(Peak_SignalLevel$A-Peak_SignalLevel$B))
Peak_SignalLevel <- cbind(Peak_SignalLevel, A_C=(Peak_SignalLevel$A-Peak_SignalLevel$C))

#Random Forest
install.packages("randomForest")
library(randomForest)
#6IVs
fit_rf <- randomForest(SM_dx ~ Turn_Count + Child_Voc_Count + Child_Voc_Duration + Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, data=CPP_data,
                                        importance=TRUE, ntree=2000)
fit_rf
varImpPlot(fit_rf)
#4IVs
fit_rf_4 <- randomForest(SM_dx ~ Turn_Count + Child_Voc_Count + Child_Voc_Duration + Average_SignalLevel, data=CPP_data,
                       importance=TRUE, ntree=2000)
fit_rf_4
varImpPlot(fit_rf_4)
#2IVs
fit_rf_2 <- randomForest(SM_dx ~Child_Voc_Count + Child_Voc_Duration, data=CPP_data,
                         importance=TRUE, ntree=2000)
fit_rf_2
varImpPlot(fit_rf_2)

#Bayesian
install.packages("e1071")
library(e1071)
fit_nb <- naiveBayes(SM_dx ~ Turn_Count + Child_Voc_Count + Child_Voc_Duration + Child_NonVoc_Duration + Average_SignalLevel + Peak_SignalLevel, data=CPP_data)
fit_nb
table(predict())




##WEDNESDAY ANOVAS##
#Turncount#
Turncount_raw <- sqldf ('select URSI, SM_dx, A, B, C
                    from Turn_Count')
Turncount_long <- gather(Turncount_raw, session, Turncount, A:C)
fit_Turncount <- aov(Turncount~(session*SM_dx)+Error(URSI/session)+(SM_dx),
                  data=Turncount_long)
summary(fit_Turncount) #SM_dx 0.0046 ** #session:SM_dx 0.0283 *
#Vocal#
Childvoc_raw <- sqldf ('select URSI, SM_dx, A, B, C
                    from Child_Voc_Count')
Childvoc_long <- gather(Childvoc_raw, session, Voc, A:C)
fit_Childvoc <- aov(Voc~(session*SM_dx)+Error(URSI/session)+(SM_dx),
                     data=Childvoc_long)
summary(fit_Childvoc) #SM_dx 0.00492 **
#VocDur#
VocDur_raw <- sqldf ('select URSI, SM_dx, A, B, C
                    from Child_Voc_Duration')
VocDur_long <- gather(VocDur_raw, session, VocDur, A:C)
fit_VocDur <- aov(VocDur~(session*SM_dx)+Error(URSI/session)+(SM_dx),
                     data=VocDur_long)
summary(fit_VocDur) #SM_dx 0.0145 * #session 0.0124 *
#NonVocDur#
NonVocDur_raw <- sqldf ('select URSI, SM_dx, A, B, C
                    from Turn_Count')
NonVocDur_long <- gather(NonVocDur_raw, session, NonVocDur, A:C)
fit_NonVocDur <- aov(NonVocDur~(session*SM_dx)+Error(URSI/session)+(SM_dx),
                     data=NonVocDur_long)
summary(fit_NonVocDur) #SM_dx 0.0046 ** #session:SM_dx 0.0283 *
#PeakSignal#
PeakSignal_raw <- sqldf ('select URSI, SM_dx, A, B, C
                    from Turn_Count')
PeakSignal_long <- gather(PeakSignal_raw, session, PeakSignal, A:C)
fit_PeakSignal <- aov(PeakSignal~(session*SM_dx)+Error(URSI/session)+(SM_dx),
                     data=PeakSignal_long)
summary(fit_PeakSignal) #SM_dx 0.0046 ** #session:SM_dx 0.0283 *
#AvgSignal#
AvgSignal_raw <- sqldf ('select URSI, SM_dx, A, B, C
                    from Turn_Count')
AvgSignal_long <- gather(AvgSignal_raw, session, AvgSignal, A:C)
fit_AvgSignal <- aov(AvgSignal~(session*SM_dx)+Error(URSI/session)+(SM_dx),
                     data=AvgSignal_long)
summary(fit_AvgSignal) #SM_dx 0.0046 ** #session:SM_dx 0.0283 *


##PLOTS
#Boxplots of Avg across all conditions, SM vs. Non-SM
Turn_Count <- cbind(Turn_Count, ABCAvg=(Turn_Count$A+Turn_Count$B+Turn_Count$C)/3)
boxplot(ABCAvg~SM_dx,data=Turn_Count, 
        names=c("Control","SM"),
        col=(c("cadetblue2","darkolivegreen2")),
        main="Conversational Turns", xlab="SM Diagnosis", ylab="Avg CT Across all Conditions")
Child_Voc_Count <- cbind(Child_Voc_Count, ABCAvg=(Child_Voc_Count$A+Child_Voc_Count$B+Child_Voc_Count$C)/3)
boxplot(ABCAvg~SM_dx,data=Child_Voc_Count, 
        names=c("Control","SM"),
        col=(c("cadetblue2","darkolivegreen2")),
        main="Child Vocalizations", xlab="SM Diagnosis", ylab="Avg Voc Across all Conditions") 
Child_Voc_Duration <- cbind(Child_Voc_Duration, ABCAvg=(Child_Voc_Duration$A+Child_Voc_Duration$B+Child_Voc_Duration$C)/3)
boxplot(ABCAvg~SM_dx,data=Child_Voc_Duration, 
        names=c("Control","SM"),
        col=(c("cadetblue2","darkolivegreen2")),
        main="Child Vocalization Duration", xlab="SM Diagnosis", ylab="Avg VD Across all Conditions") 
Child_NonVoc_Duration <- cbind(Child_NonVoc_Duration, ABCAvg=(Child_NonVoc_Duration$A+Child_NonVoc_Duration$B+Child_NonVoc_Duration$C)/3)
boxplot(ABCAvg~SM_dx,data=Child_NonVoc_Duration, 
        names=c("Control","SM"),
        col=(c("cadetblue2","darkolivegreen2")),
        main="Child Non-Vocalization Duration", xlab="SM Diagnosis", ylab="Avg NVD Across all Conditions") 
Peak_SignalLevel <- cbind(Peak_SignalLevel, ABCAvg=(Peak_SignalLevel$A+Peak_SignalLevel$B+Peak_SignalLevel$C)/3)
boxplot(ABCAvg~SM_dx,data=Peak_SignalLevel, 
        names=c("Control","SM"),
        col=(c("cadetblue2","darkolivegreen2")),
        main="Peak Volume", xlab="SM Diagnosis", ylab="Avg Peak Vol (db) Across all Conditions") 
Average_SignalLevel <- cbind(Average_SignalLevel, ABCAvg=(Average_SignalLevel$A+Average_SignalLevel$B+Average_SignalLevel$C)/3)
boxplot(ABCAvg~SM_dx,data=Average_SignalLevel, 
        names=c("Control","SM"),
        col=(c("cadetblue2","darkolivegreen2")),
        main="Average Volume", xlab="SM Diagnosis", ylab="Avg Vol (db) Across all Conditions") 


##Extra plot differences in measures over sessions
Turn_Count$SM_dx <- factor(Turn_Count$SM_dx, c(0,1)) #turn into factor

Turncount_diff<- sqldf ('select URSI, SM_dx, A_B, A_C
                    from Turn_Count')
Turncount_diff_long <- gather(Turncount_diff, sessiondiff, Turncountdiff, A_B:A_C)

p = ggplot(data = Turncount_diff_long, aes(x = sessiondiff, y = Turncountdiff, group = URSI, color=SM_dx)) +
  ggtitle("Controlled Play Paradigm - Conversational Turn Differences Across Blocks") + labs(x="Block Difference", y="Difference in Conversational Turns") + 
  scale_colour_discrete(name="Group")
p + geom_point() + 
  geom_line(linetype=2) + 
  stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 4) + 
  stat_summary(aes(group = 1), geom = "line", fun.y = mean, linetype = 1, size = 2) +
  stat_summary(aes(group = 1), geom = "errorbar", fun.data=mean_se, width = 0.25) +
  scale_x_discrete(limits=c(1,2), labels=c("Tuesday - Monday", "Wednesday - Monday", "Thursday-Monday")) +
  coord_cartesian(xlim = c(1, 3), ylim = c(-6, 6))
dev.off()
