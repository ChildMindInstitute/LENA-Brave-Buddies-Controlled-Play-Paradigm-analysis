##BRAVE BUDDIES HEAT MAP VOCALIZATIONS##

library(viridis)
library(gplots)

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs")

data <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/AvgAudioHeatMapTotal.csv")

#Assigns labels in column 1 to "rnames"
rnames <- data[,1]

#transforms columns into matrix
mat_data <- data.matrix(data[, 3:ncol(data)])

#assign row names
rownames(mat_data) <- rnames

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/AVGAUD-HeatMap.png", width = 1600, height = 1400, pointsize = 18)

#plots heatmap
heatmap.2(mat_data, 
          density.info = "none", #turns off density plot in legend
          trace = "none", #do not show trace lines inside heat map
          dendrogram = "none", #turns off row dendrogram
          na.color = "red", #All "NA" values show as red
          Colv = "FALSE", #turn off column clustering
          Rowv = "FALSE", #turn off row reordering
          margins = c(11, 8),
          srtRow = 15,
          srtCol = 60,
          keysize = 0.75,
          key.xlab = "Mean Child Segment dB SPL",
          main = "Mean Child Segment dB SPL By Activity",
          xlab = "Activity",
          ylab = "Subject ID",
          col = "viridis") #set colors to viridis

dev.off()