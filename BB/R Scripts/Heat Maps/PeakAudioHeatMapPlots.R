#LENA Data Analysis: Peak Audio Level Heat Map
#Jacob Stroud

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs")
sub_folders=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE)
sub_folders_paths=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE, full.names = TRUE)

mycolors=rainbow(n= length(sub_folders))
groupColor= c()

activities=c("Warm Up", "Morning Meeting", "Lunch", "Outside Play", "Prize Store")

####### Monday #######

##1 Section PNG##
png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Monday-peak-HeatMap.png",width=600, height=600) #Opens a PNG file#


##Data Matrix##
sum_counts=  c(rep(0, length(activities))) 
day_diff_words= matrix(c(rep(0, (length(activities)+1)*length(sub_folders))),nrow= length(sub_folders), ncol= length(activities)+1)
colorcheck=matrix(c(rep(0, (length(activities)+1)*length(sub_folders))),nrow= length(sub_folders), ncol= length(activities)+1)
wordsAll= c()


##Plot##
plot(c(1,length(activities)), c(70, 90), type="n" , xlab ="Activity", main = "Average Peak dB SPL on Monday", cex.main=2, xaxt = "n",ylab = "Average Peak dB SPL per Child Segment", cex.lab=1.3)
axis(side = 1,at= c(1:length(activities)),labels =activities,  las=1)

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Heat Map View", sep=""))
  wordsAll <- na.omit(wordsAll)
  
  if (identical(sub_folders[i], "M00445929")) {
    groupColor="red"
  } else if (identical(sub_folders[i], "M00412434") || identical( sub_folders[i],"M00441664") || identical(sub_folders[i],"M00402344")|| identical(sub_folders[i],"M00440011") || identical( sub_folders[i],"M00494954" ) ){
    groupColor="green"
    
  } else if  (identical(sub_folders[i], "M00475465") || identical(sub_folders[i], "M00413464") || identical (sub_folders[i],"M00490907") || identical(sub_folders[i],"M00495999") || identical(sub_folders[i],"M00472399" )) {
    groupColor="blue"
  }
  
  
  dayfiles=dir(pattern=paste(sub_folders[i], "_peakaudio.csv", sep = ""), full.names = FALSE)
  bravebuds_counts= read.csv(dayfiles)
  
  
  points(x= 1:length(activities), y= bravebuds_counts$Monday[1:length(activities)], pch=19, col= groupColor,type = "o",lty=3)
  wordsAll=rbind(wordsAll, bravebuds_counts$Monday[1:length(activities)])
  for (j in 1:length(activities)) {
    day_diff_words[i,j] =bravebuds_counts$Monday[j]-bravebuds_counts$Monday[1]
    colorcheck[i,j]=bravebuds_counts$Monday[j]-bravebuds_counts$Monday[1]
  }
  colorcheck[i,length(activities)+1]=dayfiles
  
}

lines(x= 1:length(activities) , y=apply(wordsAll,2,mean), type="o", pch=19, lwd=3)

arrows(x0=1:5, y0=apply(wordsAll,2,mean), x1=1:5, y1=apply(wordsAll,2,mean)+apply(wordsAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
arrows(x0=1:5, y0=apply(wordsAll,2,mean), x1=1:5, y1=apply(wordsAll,2,mean)-apply(wordsAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
legend(x=0, y= 90, c("group 1 ", "group 2", "group 3"), lty=0 ,col = c("red", "green", "blue") ,pch=20, text.font = 10, bty = "n", cex = 1)

dev.off()

####### Tuesday #######

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Tuesday-peak-HeatMap.png",width=600, height=600)


durAll=c()
sum_durs=  c(rep(0, length(activities)))
day_diff_durs= matrix(c(rep(0, length(activities)*length(sub_folders))),nrow= length(sub_folders), ncol= length(activities))
colorcheck=matrix(c(rep(0, (length(activities)+1)*length(sub_folders))),nrow= length(sub_folders), ncol= length(activities)+1)

plot(c(1,length(activities)), c(70, 90), type="n" , xlab ="Day", main = "Average Peak dB SPL on Tuesday", cex.main=2, xaxt = "n",ylab = "Average Peak dB SPL per Child Segment", cex.lab=1.3)
axis(side = 1,at= c(1:length(activities)),labels =activities,  las=1)

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Heat Map View", sep = ""))
  dayfiles=dir(pattern=paste(sub_folders[i], "_peakaudio.csv", sep = ""), full.names = FALSE)
  bravebuds_counts= read.csv(dayfiles)
  durAll <- na.omit(durAll)
  if (identical(sub_folders[i], "M00445929")) {
    groupColor="red"
  } else if (identical(sub_folders[i], "M00412434") || identical( sub_folders[i],"M00441664") || identical(sub_folders[i],"M00402344")|| identical(sub_folders[i],"M00440011") || identical( sub_folders[i],"M00494954" ) ){
    groupColor="green"
    
  } else if  (identical(sub_folders[i], "M00475465") || identical(sub_folders[i], "M00413464") || identical (sub_folders[i],"M00490907") || identical(sub_folders[i],"M00495999") || identical(sub_folders[i],"M00472399" )) {
    groupColor="blue"
  }
  points(x= 1:length(activities), y= bravebuds_counts$Tuesday[1:length(activities)], type = "o", pch=19, col= groupColor,lty=3)
  #sum_durs= sum_durs+bravebuds_counts$Tuesday
  durAll=rbind(durAll, bravebuds_counts$Tuesday[1:length(activities)])
  
  for (j in 1:length(activities)) {
    day_diff_durs[i,j] =bravebuds_counts$Tuesday[j]-bravebuds_counts$Tuesday[1]
    colorcheck[i,j]=    bravebuds_counts$Tuesday[j]-bravebuds_counts$Tuesday[1]
  }
  colorcheck[i,length(activities)+1]=dayfiles
  
}

#ave_counts= sum_counts/length(sub_folders)
lines(x= 1:length(activities) , y=apply(durAll,2,mean), type="o", pch=19, lwd=3)
#legend(x=3, y= 600, c("Subject's word count per day during lunch", "Average word count across subjects per day during lunch"), lty=c(0,1),col = c(mycolors[1], "black") ,pch=20, text.font = 10, bty = "n", cex = .7, lwd = 3)
arrows(x0=1:5, y0=apply(durAll,2,mean), x1=1:5, y1=apply(durAll,2,mean)+apply(durAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
arrows(x0=1:5, y0=apply(durAll,2,mean), x1=1:5, y1=apply(durAll,2,mean)-apply(durAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)

legend(x=0, y= 90, c("group 1 ", "group 2", "group 3"), lty=0 ,col = c("red", "green", "blue") ,pch=20, text.font = 10, bty = "n", cex = 1)

dev.off()

## Wednesday ##

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Wednesday-peak-HeatMap.png",width=600, height=600)


sum_turns=  c(rep(0, length(activities)))
day_diff_turns= matrix(c(rep(0, 5*length(sub_folders))),nrow= length(sub_folders), ncol= 5)
turnsAll=c()
colorcheck=matrix(c(rep(0, (length(activities)+1)*length(sub_folders))),nrow= length(sub_folders), ncol= length(activities)+1)

plot(c(1,length(activities)), c(70, 90), type="n" , xlab ="Day", main = "Average Peak dB SPL on Wednesday", cex.main=2, xaxt = "n",ylab = "Average Peak dB SPL per Child Segment"  , cex.lab=1.3)
axis(side = 1,at= c(1:length(activities)),labels =activities,  las=1)

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Heat Map View", sep = ""))
  turnsAll <- na.omit(turnsAll)
  dailyFile=dir(pattern="_peakaudio.csv", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  if (identical(sub_folders[i], "M00445929")) {
    groupColor="red"
  } else if (identical(sub_folders[i], "M00412434") || identical( sub_folders[i],"M00441664") || identical(sub_folders[i],"M00402344")|| identical(sub_folders[i],"M00440011") || identical( sub_folders[i],"M00494954" ) ){
    groupColor="green"
    
  } else if  (identical(sub_folders[i], "M00475465") || identical(sub_folders[i], "M00413464") || identical (sub_folders[i],"M00490907") || identical(sub_folders[i],"M00495999") || identical(sub_folders[i],"M00472399" )) {
    groupColor="blue"
  }
  points(x= 1:length(activities), y= bravebuds_counts$Wednesday[1:length(activities)], type = "o",lty=3, pch=19, col= groupColor)
  turnsAll=rbind(turnsAll, bravebuds_counts$Wednesday[1:length(activities)])
  
  
  for (j in 1:length(activities)) {
    day_diff_turns[i,j] =bravebuds_counts$Wednesday[j]-bravebuds_counts$Wednesday[1]
    colorcheck[i,j]=    bravebuds_counts$Wednesday[j]-bravebuds_counts$Wednesday[1]
  }
  colorcheck[i,length(activities)+1]=dailyFile
  
}

lines(x= 1:length(activities) , y=apply(turnsAll, 2, mean), type="o", pch=19, lwd=3)

arrows(x0=1:5, y0=apply(turnsAll,2,mean), x1=1:5, y1=apply(turnsAll,2,mean)+apply(turnsAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
arrows(x0=1:5, y0=apply(turnsAll,2,mean), x1=1:5, y1=apply(turnsAll,2,mean)-apply(turnsAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
legend(x=0, y= 90, c("group 1 ", "group 2", "group 3"), lty=0 ,col = c("red", "green", "blue") ,pch=20, text.font = 10, bty = "n", cex = 1)

dev.off()

## Thursday ##

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/Thursday-peak-HeatMap.png",width=600, height=600)


sum_seg=  c(rep(0, length(activities)))
day_diff_seg= matrix(c(rep(0, length(activities)*length(sub_folders))),nrow= length(sub_folders), ncol= length(activities))
segAll=c()
colorcheck=matrix(c(rep(0, (length(activities)+1)*length(sub_folders))),nrow= length(sub_folders), ncol= length(activities)+1)

plot(c(1,length(activities)), c(70, 90), type="n" , xlab ="Day", main = "Average Peak dB SPL On Thursday", cex.main=2, xaxt = "n",ylab = "Average Peak dB SPL per Child Segment"  , cex.lab=1.3)
axis(side = 1,at= c(1:length(activities)),labels =activities,  las=1)

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Heat Map View", sep = ""))
  segAll <- na.omit(segAll)
  dailyFile=dir(pattern="_peakaudio.csv", full.names = FALSE)
  bravebuds_counts= read.csv(dailyFile)
  if (identical(sub_folders[i], "M00445929")) {
    groupColor="red"
  } else if (identical(sub_folders[i], "M00412434") || identical( sub_folders[i],"M00441664") || identical(sub_folders[i],"M00402344")|| identical(sub_folders[i],"M00440011") || identical( sub_folders[i],"M00494954" ) ){
    groupColor="green"
    
  } else if  (identical(sub_folders[i], "M00475465") || identical(sub_folders[i], "M00413464") || identical (sub_folders[i],"M00490907") || identical(sub_folders[i],"M00495999") || identical(sub_folders[i],"M00472399" )) {
    groupColor="blue"
  }
  points(x= 1:length(activities), y= bravebuds_counts$Thursday[1:length(activities)], type = "o", pch=19, col= groupColor, lty=3)
  #sum_seg= sum_seg+bravebuds_counts$Thursday
  segAll=rbind(segAll, bravebuds_counts$Thursday[1:length(activities)])
  
  for (j in 1:length(activities)) {
    day_diff_seg[i,j] =bravebuds_counts$Thursday[j]-bravebuds_counts$Thursday[1]
    
    colorcheck[i,j]=    bravebuds_counts$Thursday[j]-bravebuds_counts$Thursday[1]
  }
  colorcheck[i,length(activities)+1]=dailyFile
}
#sum_seg= sum_seg/length(sub_folders)
lines(x= 1:length(activities) , y=apply(segAll, 2, mean), type="o", pch=20, lwd=3)

arrows(x0=1:5, y0=apply(segAll,2,mean), x1=1:5, y1=apply(segAll,2,mean)+apply(segAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
arrows(x0=1:5, y0=apply(segAll,2,mean), x1=1:5, y1=apply(segAll,2,mean)-apply(segAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
legend(x=0, y= 90, c("group 1 ", "group 2", "group 3"), lty=0 ,col = c("red", "green", "blue") ,pch=20, text.font = 10, bty = "n", cex = 1)

dev.off()