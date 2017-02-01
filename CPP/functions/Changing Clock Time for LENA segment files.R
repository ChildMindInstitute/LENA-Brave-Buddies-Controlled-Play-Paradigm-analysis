##Changing Clock time for LENA Segment level output
#Jacob Stroud

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Segment Level Detail")

M00466567 <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Segment Level Detail/M00466567_controlledplay_detailed.csv")

M00466567$Clock_Time_TZAdj <- strftime(M00466567$Clock_Time_TZAdj, "%H:%M")
M00466567$Clock_Time_TZAdj <- strptime(M00466567$Clock_Time_TZAdj, "%H:%M")

M00466567$time <- (M00466567$Clock_Time_TZAdj-480)

write.csv(M00466567, "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Segment Level Detail/M00466567_controlledplay_detailed.csv")

M00418353 <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Segment Level Detail/M00418353_controlledplay_detailed.csv")

M00418353$Clock_Time_TZAdj <- strftime(M00418353$Clock_Time_TZAdj, "%H:%M")
M00418353$Clock_Time_TZAdj <- strptime(M00418353$Clock_Time_TZAdj, "%H:%M")

M00418353$time <- (M00418353$Clock_Time_TZAdj-480)

write.csv(M00418353, "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Segment Level Detail/M00418353_controlledplay_detailed.csv")

M00473061 <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Segment Level Detail/M00473061_controlledplay_detailed.csv", stringsAsFactors = TRUE)

M00473061$Clock_Time_TZAdj <- strptime(M00473061$Clock_Time_TZAdj, "%H:%M:%S")

M00473061$time <- (M00473061$Clock_Time_TZAdj-180)

write.csv(M00473061, "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Controlled Play Paradigm/Control Outputs/Segment Level Detail/M00473061_controlledplay_detailed.csv")
