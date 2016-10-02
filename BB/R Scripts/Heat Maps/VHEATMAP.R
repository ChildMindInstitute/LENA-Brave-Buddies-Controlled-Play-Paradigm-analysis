##BRAVE BUDDIES HEAT MAP VOCALIZATIONS##

library(viridis)
library(gplots)

setwd("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs")

data <- read.csv("/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/LENA Outputs/VocalizationHeatMapTotal.csv")

#Assigns labels in column 1 to "rnames"
rnames <- data[,1]

#transforms columns into matrix
mat_data <- data.matrix(data[, 3:ncol(data)])

#assign row names
rownames(mat_data) <- rnames

png(filename ="/Volumes/data/Research/CDB/Progress Monitoring:LENA/Brave Buddies/Plots and Results/VOC-HeatMap.png", width = 1600, height = 1400, pointsize = 25)

#plots heatmap
heatmap.2(mat_data, 
          density.info = "none", #turns off density plot in legend
          trace = "none", #do not show trace lines inside heat map
          dendrogram = "none", #turns off row dendrogram
          na.color = "red", #All "NA" values show as red
          Colv = "FALSE", #turn off column clustering
          Rowv = "FALSE", #turn off row reordering
          margins = c(11, 5),
          srtRow = 0,
          srtCol = 60,
          keysize = 0.8,
          key.xlab = "Number of Vocalizations per 5 minutes",
          key.title = NA,
          main = "Number of Vocalizations per 5 minutes By Activity",
          xlab = "Activity",
          ylab = "Subject ID",
          col = "viridis") #set colors to viridis

dev.off()