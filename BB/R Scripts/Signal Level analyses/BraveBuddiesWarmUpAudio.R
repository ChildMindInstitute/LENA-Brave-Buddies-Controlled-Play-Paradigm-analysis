#LENA Data Analysis: Warm Up Audio Level Data
#Jacob Stroud

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs")
sub_folders=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE)
sub_folders_paths=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE, full.names = TRUE)

mycolors=rainbow(n= length(sub_folders))
groupColor= c()

days=c("Mon", "Tue", "Wed", "Thur")

#################Average Decibel Level###################


png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/WarmUp-avgdbl.png",width=1400, height=600) #Opens a PNG file#
par(mfrow=c(1,2)) #Splits png into 2 sections


#Creates data matrices with all zeroes
sum_counts=  c(rep(0, length(days))) 
day_diff_avg= matrix(c(rep(0, (length(days)+1)*length(sub_folders))),nrow= length(sub_folders), ncol= length(days)+1)
colorcheck=matrix(c(rep(0, (length(days)+1)*length(sub_folders))),nrow= length(sub_folders), ncol= length(days)+1)
avgAll= c()


#Creates a plot for the data
plot(c(0,length(days)+1), c(70, 90), type="n" , xlab ="Day", main = "Child Segment Average Decibel Level-Warm Up", cex.main=2, xaxt = "n",ylab = "dB SPL", cex.lab=1.3)
axis(side = 1,at= c(1:length(days)),labels =days,  las=1)


for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Audio Level View", sep=""))
  avgAll <- na.omit(avgAll)
  
  if (identical(sub_folders[i], "M00445929")) {
    groupColor="red"
  } else if (identical(sub_folders[i], "M00412434") || identical( sub_folders[i],"M00441664") || identical(sub_folders[i],"M00402344")|| identical(sub_folders[i],"M00440011") || identical( sub_folders[i],"M00494954" ) ){
    groupColor="green"
    
  } else if  (identical(sub_folders[i], "M00475465") || identical(sub_folders[i], "M00413464") || identical (sub_folders[i],"M00490907") || identical(sub_folders[i],"M00495999") || identical(sub_folders[i],"M00472399" )) {
    groupColor="blue"
  }
  
  
  dayfiles=dir(pattern=paste(sub_folders[i], "_WarmUpAudio.csv", sep = ""), full.names = FALSE)
  bravebuds_counts= read.csv(dayfiles)
  
  
  points(x= 1:length(days), y= bravebuds_counts$Average_SignalLevel[1:length(days)], pch=19, col= groupColor,type = "o",lty=3)
  avgAll=rbind(avgAll, bravebuds_counts$Average_SignalLevel[1:length(days)])
  for (j in 1:length(days)) {
    day_diff_avg[i,j] =bravebuds_counts$Average_SignalLevel[j]-bravebuds_counts$Average_SignalLevel[1]
    colorcheck[i,j]=bravebuds_counts$Average_SignalLevel[j]-bravebuds_counts$Average_SignalLevel[1]
  }
  colorcheck[i,length(days)+1]=dayfiles
  
}

#ave_counts= sum_counts/length(sub_folders)


lines(x= 1:length(days) , y=apply(avgAll,2,mean), type="o", pch=19, lwd=3)

arrows(x0=1:4, y0=apply(avgAll,2,mean), x1=1:4, y1=apply(avgAll,2,mean)+apply(avgAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
arrows(x0=1:4, y0=apply(avgAll,2,mean), x1=1:4, y1=apply(avgAll,2,mean)-apply(avgAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
legend(x=70, y= 90, c("group 1 ", "group 2", "group 3"), lty=0 ,col = c("red", "green", "blue") ,pch=20, text.font = 10, bty = "n", cex = 1)

#progress
plot(c(0,length(days)), c(-10,10), type="n" , main ="Average Segment dB Level during Warm Up Across Week", cex.main=2, xaxt = "n", xlab=NA, ylab = "Differences in Average Segment dB SPL Compared to Monday", cex.lab=1.3, cex.main=2)
axis(side = 1,at= 1:3,labels = c( "Tue-Mon", "Wed-Mon", "Thur-Mon"),  las=1)

for (q in 1:dim(day_diff_avg)[1]){
  day_diff_avg <- na.omit(day_diff_avg)
  
  if (identical(sub_folders[q], "M00445929")) {
    groupColor="red"
  } else if (identical(sub_folders[q], "M00412434") || identical( sub_folders[q],"M00441664") || identical(sub_folders[q],"M00402344")|| identical(sub_folders[q],"M00440011") || identical( sub_folders[q],"M00494954" ) ){
    groupColor="green"
    
  } else if  (identical(sub_folders[q], "M00475465") || identical(sub_folders[q], "M00413464") || identical (sub_folders[q],"M00490907") || identical(sub_folders[q],"M00495999") || identical(sub_folders[q],"M00472399" )) {
    groupColor="blue"
  }
  
  
  points(x=1:3, y= day_diff_avg[q,2:length(days)], pch=15 , col=groupColor, type = "o", lty=3  )
  lines(x= 1:3 , y=apply(day_diff_avg[,2:4],2,mean), type="o", pch=19, lwd=3)
  
  arrows(x0=1:3, y0= apply(day_diff_avg[,2:4],2,mean), x1=1:3, y1=apply(day_diff_avg[,2:4],2,mean)+apply(day_diff_avg[,2:4],2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
  arrows(x0=1:3, y0=apply(day_diff_avg[,2:4],2,mean), x1=1:3, y1=apply(day_diff_avg[,2:4],2,mean)-apply(day_diff_avg[,2:4],2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
  
}


dev.off()


#########Peak Decibel Level######


png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/WarmUp-peakdbl.png",width=1400, height=600) #Opens a PNG file#
par(mfrow=c(1,2)) #Splits png into 2 sections


#Creates data matrices with all zeroes
sum_counts=  c(rep(0, length(days))) 
day_diff_peak= matrix(c(rep(0, (length(days)+1)*length(sub_folders))),nrow= length(sub_folders), ncol= length(days)+1)
colorcheck=matrix(c(rep(0, (length(days)+1)*length(sub_folders))),nrow= length(sub_folders), ncol= length(days)+1)
peakAll= c()


#Creates a plot for the data
plot(c(0,length(days)+1), c(70, 90), type="n" , xlab ="Day", main = "Average Child Segment Peak dB SPL-Warm Up", cex.main=2, xaxt = "n",ylab = "dB SPL", cex.lab=1.3)
axis(side = 1,at= c(1:length(days)),labels =days,  las=1)


for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Audio Level View", sep=""))
  peakAll <- na.omit(peakAll)
  
  if (identical(sub_folders[i], "M00445929")) {
    groupColor="red"
  } else if (identical(sub_folders[i], "M00412434") || identical( sub_folders[i],"M00441664") || identical(sub_folders[i],"M00402344")|| identical(sub_folders[i],"M00440011") || identical( sub_folders[i],"M00494954" ) ){
    groupColor="green"
    
  } else if  (identical(sub_folders[i], "M00475465") || identical(sub_folders[i], "M00413464") || identical (sub_folders[i],"M00490907") || identical(sub_folders[i],"M00495999") || identical(sub_folders[i],"M00472399" )) {
    groupColor="blue"
  }
  
  
  dayfiles=dir(pattern=paste(sub_folders[i], "_WarmUpAudio.csv", sep = ""), full.names = FALSE)
  bravebuds_counts= read.csv(dayfiles)
  
  
  points(x= 1:length(days), y= bravebuds_counts$Peak_SignalLevel[1:length(days)], pch=19, col= groupColor,type = "o",lty=3)
  peakAll=rbind(peakAll, bravebuds_counts$Peak_SignalLevel[1:length(days)])
  for (j in 1:length(days)) {
    day_diff_peak[i,j] =bravebuds_counts$Peak_SignalLevel[j]-bravebuds_counts$Peak_SignalLevel[1]
    colorcheck[i,j]=bravebuds_counts$Peak_SignalLevel[j]-bravebuds_counts$Peak_SignalLevel[1]
  }
  colorcheck[i,length(days)+1]=dayfiles
  
}

#ave_counts= sum_counts/length(sub_folders)


lines(x= 1:length(days) , y=apply(peakAll,2,mean), type="o", pch=19, lwd=3)

arrows(x0=1:4, y0=apply(peakAll,2,mean), x1=1:4, y1=apply(peakAll,2,mean)+apply(peakAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
arrows(x0=1:4, y0=apply(peakAll,2,mean), x1=1:4, y1=apply(peakAll,2,mean)-apply(peakAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
legend(x=70, y= 90, c("group 1 ", "group 2", "group 3"), lty=0 ,col = c("red", "green", "blue") ,pch=20, text.font = 10, bty = "n", cex = 1)

#progress
plot(c(0,length(days)), c(-10,10), type="n" , main ="Average Segment Peak dB SPL Across Week-Warm Up", cex.main=2, xaxt = "n", xlab=NA, ylab = "Differences in Peak segment dB SPL Compared to Monday", cex.lab=1.3, cex.main=2)
axis(side = 1,at= 1:3,labels = c( "Tue-Mon", "Wed-Mon", "Thur-Mon"),  las=1)

for (q in 1:dim(day_diff_peak)[1]){
  day_diff_peak <- na.omit(day_diff_peak)
  
  if (identical(sub_folders[q], "M00445929")) {
    groupColor="red"
  } else if (identical(sub_folders[q], "M00412434") || identical( sub_folders[q],"M00441664") || identical(sub_folders[q],"M00402344")|| identical(sub_folders[q],"M00440011") || identical( sub_folders[q],"M00494954" ) ){
    groupColor="green"
    
  } else if  (identical(sub_folders[q], "M00475465") || identical(sub_folders[q], "M00413464") || identical (sub_folders[q],"M00490907") || identical(sub_folders[q],"M00495999") || identical(sub_folders[q],"M00472399" )) {
    groupColor="blue"
  }
  
  
  points(x=1:3, y= day_diff_peak[q,2:length(days)], pch=15 , col=groupColor, type = "o", lty=3  )
  lines(x= 1:3 , y=apply(day_diff_peak[,2:4],2,mean), type="o", pch=19, lwd=3)
  
  arrows(x0=1:3, y0= apply(day_diff_peak[,2:4],2,mean), x1=1:3, y1=apply(day_diff_peak[,2:4],2,mean)+apply(day_diff_peak[,2:4],2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
  arrows(x0=1:3, y0=apply(day_diff_peak[,2:4],2,mean), x1=1:3, y1=apply(day_diff_peak[,2:4],2,mean)-apply(day_diff_peak[,2:4],2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
  
}


dev.off()