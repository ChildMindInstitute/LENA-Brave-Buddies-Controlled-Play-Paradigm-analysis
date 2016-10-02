wholepath = '/Volumes/Data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Brave Buddies Audio Level Exports/'
setwd(wholepath)

subject_name <- list.files(pattern= "M00")


# loop throught each subjects folder
for (i in subject_name){
  print(i)
  setwd(paste0(wholepath,"/",i))
  file_name <- list.files(pattern= ".csv")
  file_name = file_name[order(file_name)]
  data = read.csv(file_name[1])
  data_onlyCHN = subset(data,Speaker_ID=='CHN')
  data_sl = data_onlyCHN$Average_SignalLevel
  data_pk = data_onlyCHN$Peak_SignalLevel
}

data_time = data_onlyCHN$Clock_Time_TZAdj
