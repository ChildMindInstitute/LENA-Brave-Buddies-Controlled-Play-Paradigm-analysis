library("sqldf")
library("tidyr")

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs")

ancovadata <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/LENA Outputs/ANCOVAdata.csv")

###Total Interference Raw Score###

vINT <- aov(Vocalizations~(Condition*Interference)+Error(URSI/(Condition))+(Interference),
                   data=ancovadata)
summary(vINT)

###At School##

vAS <- aov(Vocalizations~(Condition*School)+Error(URSI/(Condition))+(School),
            data=ancovadata)
summary(vAS)

##At Home##

vAH <- aov(Vocalizations~(Condition*Home)+Error(URSI/(Condition))+(Home),
            data=ancovadata)
summary(vAH)

##Social Situations##

vSOC <- aov(Vocalizations~(Condition*Social)+Error(URSI/(Condition))+(Social),
            data=ancovadata)
summary(vSOC)
