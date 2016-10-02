#LENA Data Analysis: Lunch time data
#Jacob Stroud

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs")
sub_folders=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE)
sub_folders_paths=dir(path = "/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs", pattern = "M00*", ignore.case = TRUE, full.names = TRUE)

mycolors=rainbow(n= length(sub_folders))
groupColor= c()

days=c("Mon", "Tue", "Wed", "Thur")

################# child vocalization###################


png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/child-voc-groups_lunch.png",width=1400, height=600) #Opens a PNG file#
par(mfrow=c(1,2)) #Splits png into 2 sections


#Creates data matrices with all zeroes
sum_counts=  c(rep(0, length(days))) 
day_diff_words= matrix(c(rep(0, (length(days)+1)*length(sub_folders))),nrow= length(sub_folders), ncol= length(days)+1)
colorcheck=matrix(c(rep(0, (length(days)+1)*length(sub_folders))),nrow= length(sub_folders), ncol= length(days)+1)
wordsAll= c()

#Creates a plot for the data
plot(c(0,length(days)+1), c(0, 80), type="n" , xlab ="Day", main = "Child Vocalization During Lunchtime", cex.main=2, xaxt = "n",ylab = "Words", cex.lab=1.3)
axis(side = 1,at= c(1:length(days)),labels =days,  las=1)


for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Lunchtime View", sep=""))
  
  
   if (identical(sub_folders[i], "M00445929")) {
    groupColor="red"
  } else if (identical(sub_folders[i], "M00412434") || identical( sub_folders[i],"M00441664") || identical(sub_folders[i],"M00402344")|| identical(sub_folders[i],"M00440011") || identical( sub_folders[i],"M00494954" ) ){
    groupColor="green"
    
  } else if  (identical(sub_folders[i], "M00475465") || identical(sub_folders[i], "M00413464") || identical (sub_folders[i],"M00490907") || identical(sub_folders[i],"M00495999") || identical(sub_folders[i],"M00472399" )) {
    groupColor="blue"
  }
  
  
  dayfiles=dir(pattern=paste(sub_folders[i], "_Lunch_Counts.csv", sep = ""), full.names = FALSE)
  bravebuds_counts= read.csv(dayfiles)
  

  

  
  points(x= 1:length(days), y= bravebuds_counts$Child_Voc_Count[1:length(days)], pch=19, col= groupColor,type = "o",lty=3)
  wordsAll=rbind(wordsAll, bravebuds_counts$Child_Voc_Count[1:length(days)])
  for (j in 1:length(days)) {
    day_diff_words[i,j] =bravebuds_counts$Child_Voc_Count[j]-bravebuds_counts$Child_Voc_Count[1]
    colorcheck[i,j]=bravebuds_counts$Child_Voc_Count[j]-bravebuds_counts$Child_Voc_Count[1]
  }
  colorcheck[i,length(days)+1]=dayfiles
  
}

#ave_counts= sum_counts/length(sub_folders)
lines(x= 1:length(days) , y=apply(wordsAll,2,mean), type="o", pch=19, lwd=3)
#legend(x=3, y= 600, c("Subject's word count per day", "Average word count across subjects per day"), lty=c(0,1),col = c(mycolors[1], "black") ,pch=20, text.font = 10, bty = "n", cex = .7, lwd = 3)
arrows(x0=1:4, y0=apply(wordsAll,2,mean), x1=1:4, y1=apply(wordsAll,2,mean)+apply(wordsAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
arrows(x0=1:4, y0=apply(wordsAll,2,mean), x1=1:4, y1=apply(wordsAll,2,mean)-apply(wordsAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)

legend(x=0, y= 600, c("group 1 ", "group 2", "group 3"), lty=0 ,col = c("red", "green", "blue") ,pch=20, text.font = 10, bty = "n", cex = 1)

#progress
plot(c(0,length(days)), c(-38,42), type="n" , main ="Child's Vocalization Progress During Lunch time", cex.main=2, xaxt = "n", xlab=NA, ylab = "Modulations in child vocalizations compared to Monday", cex.lab=1.3, cex.main=2)
axis(side = 1,at= 1:3,labels = c( "Tue-Mon", "Wed-Mon", "Thur-Mon"),  las=1)

for (q in 1:dim(day_diff_words)[1]){
  
  if (identical(sub_folders[q], "M00445929")) {
    groupColor="red"
  } else if (identical(sub_folders[q], "M00412434") || identical( sub_folders[q],"M00441664") || identical(sub_folders[q],"M00402344")|| identical(sub_folders[q],"M00440011") || identical( sub_folders[q],"M00494954" ) ){
    groupColor="green"
    
  } else if  (identical(sub_folders[q], "M00475465") || identical(sub_folders[q], "M00413464") || identical (sub_folders[q],"M00490907") || identical(sub_folders[q],"M00495999") || identical(sub_folders[q],"M00472399" )) {
    groupColor="blue"
  }
  
  
  points(x=1:3, y= day_diff_words[q,2:length(days)], pch=15 , col=groupColor, type = "o", lty=3  )
  lines(x= 1:3 , y=apply(day_diff_words[,2:4],2,mean), type="o", pch=19, lwd=3)
  
  arrows(x0=1:3, y0= apply(day_diff_words[,2:4],2,mean), x1=1:3, y1=apply(day_diff_words[,2:4],2,mean)+apply(day_diff_words[,2:4],2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
  arrows(x0=1:3, y0=apply(day_diff_words[,2:4],2,mean), x1=1:3, y1=apply(day_diff_words[,2:4],2,mean)-apply(day_diff_words[,2:4],2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
  
}


dev.off()

######################## Vocalization Duration###############

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/child-voc-dur-group-Lunch.png",width=1400, height=600)
par(mfrow=c(1,2))

durAll=c()
sum_durs=  c(rep(0, length(days)))
day_diff_durs= matrix(c(rep(0, length(days)*length(sub_folders))),nrow= length(sub_folders), ncol= length(days))
colorcheck=matrix(c(rep(0, (length(days)+1)*length(sub_folders))),nrow= length(sub_folders), ncol= length(days)+1)

plot(c(0,length(days)+1), c(0, 80), type="n" , xlab ="Day", main = "Child Vocalization Duration", cex.main=2, xaxt = "n",ylab = "Time (s)", cex.lab=1.3)
axis(side = 1,at= c(1:length(days)),labels =days,  las=1)

for (i in 1:length(sub_folders_paths) ){
  setwd(paste(sub_folders_paths[i], "/Lunchtime View", sep = ""))
  dayfiles=dir(pattern=paste(sub_folders[i], "_Lunch_Counts.csv", sep = ""), full.names = FALSE)
  bravebuds_counts= read.csv(dayfiles)
  
  if (identical(sub_folders[i], "M00445929")) {
    groupColor="red"
  } else if (identical(sub_folders[i], "M00412434") || identical( sub_folders[i],"M00441664") || identical(sub_folders[i],"M00402344")|| identical(sub_folders[i],"M00440011") || identical( sub_folders[i],"M00494954" ) ){
    groupColor="green"
    
  } else if  (identical(sub_folders[i], "M00475465") || identical(sub_folders[i], "M00413464") || identical (sub_folders[i],"M00490907") || identical(sub_folders[i],"M00495999") || identical(sub_folders[i],"M00472399" )) {
    groupColor="blue"
  }
  points(x= 1:length(days), y= bravebuds_counts$Child_Voc_Duration[1:length(days)], type = "o", pch=19, col= groupColor,lty=3)
  #sum_durs= sum_durs+bravebuds_counts$Child_Voc_Duration
  durAll=rbind(durAll, bravebuds_counts$Child_Voc_Duration[1:length(days)])
  
  for (j in 1:length(days)) {
    day_diff_durs[i,j] =bravebuds_counts$Child_Voc_Duration[j]-bravebuds_counts$Child_Voc_Duration[1]
    colorcheck[i,j]=    bravebuds_counts$Child_Voc_Duration[j]-bravebuds_counts$Child_Voc_Duration[1]
  }
  colorcheck[i,length(days)+1]=dayfiles
  
}

#ave_counts= sum_counts/length(sub_folders)
lines(x= 1:length(days) , y=apply(wordsAll,2,mean), type="o", pch=19, lwd=3)
#legend(x=3, y= 600, c("Subject's word count per day during lunch", "Average word count across subjects per day during lunch"), lty=c(0,1),col = c(mycolors[1], "black") ,pch=20, text.font = 10, bty = "n", cex = .7, lwd = 3)
arrows(x0=1:4, y0=apply(wordsAll,2,mean), x1=1:4, y1=apply(wordsAll,2,mean)+apply(wordsAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
arrows(x0=1:4, y0=apply(wordsAll,2,mean), x1=1:4, y1=apply(wordsAll,2,mean)-apply(wordsAll,2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)

legend(x=0, y= 40, c("group 1 ", "group 2", "group 3"), lty=0 ,col = c("red", "green", "blue") ,pch=20, text.font = 10, bty = "n", cex = 1)

plot(c(0,length(days)), c(-40, 40), type="n" , main ="Child's Vocalization Duration Progress During Lunch", cex.main=2, xaxt = "n", xlab=NA,ylab = "Modulations in child's vocalization duration progress compared to monday lunch", cex.lab=1.3, cex.main=2)
axis(side = 1,at= 1:3,labels = c( "Tue-Mon", "Wed-Mon", "Thur-Mon"),  las=1)

for (q in 1:dim(day_diff_durs)[1]){
  if (identical(sub_folders[q], "M00445929")) {
    groupColor="red"
  } else if (identical(sub_folders[q], "M00412434") || identical( sub_folders[q],"M00441664") || identical(sub_folders[q],"M00402344")|| identical(sub_folders[q],"M00440011") || identical( sub_folders[q],"M00494954" ) ){
    groupColor="green"
    
  } else if  (identical(sub_folders[q], "M00475465") || identical(sub_folders[q], "M00413464") || identical (sub_folders[q],"M00490907") || identical(sub_folders[q],"M00495999") || identical(sub_folders[q],"M00472399" )) {
    groupColor="blue"
  }
  points(x=1:3, y= day_diff_durs[q,2:length(days)], pch=15 , col=groupColor, type = "o", lty=3  )
  lines(x= 1:3 , y=apply(day_diff_durs[,2:4],2,mean), type="o", pch=19, lwd=3)
  
  arrows(x0=1:3, y0= apply(day_diff_durs[,2:4],2,mean), x1=1:3, y1=apply(day_diff_durs[,2:4],2,mean)+apply(day_diff_durs[,2:4],2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
  arrows(x0=1:3, y0=apply(day_diff_durs[,2:4],2,mean), x1=1:3, y1=apply(day_diff_durs[,2:4],2,mean)-apply(day_diff_durs[,2:4],2,sd)/sqrt(length(sub_folders)),length=.05,angle = 90,lty=1,lwd=2)
  
}




dev.off()

