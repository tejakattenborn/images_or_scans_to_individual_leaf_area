# DIN A4 21.0 cm x 29.7 cm / Scan resolution: 2550 x 3501 pixels (200 dpi)
# total pixels: 7877250 / total cm²: 623.7

require(raster)
setwd("D:/googledrive/teaching/2021_forschungspraktikum/leaf_area/") # requires an "input" and "output" folder in there


#define factor to which scan resolution should be reduced to (increases speed / decreases memory usage)
scale_param = 1500 # height to which all images will be resampled
# minimum leaf size (segments smaller than this size will be removed)
min_size = 0.3


allscans = list.files("input/") # should contain files of tiff, png or bmp
result_mean = matrix(nrow=length(allscans), ncol=3)
colnames(result_mean) = c("ID", "mean_area", "no_leaves")
result_mean = as.data.frame(result_mean)


for(i in 1:length(allscans)){
  
  leafscan = raster(paste0("input/",allscans[[i]][[1]]))
    leafscan = aggregate(leafscan, fact=dim(leafscan)[1]/scale_param) # resample to coarser resolution
  
  if(summary(leafscan)[5]==255){
    leafscan[which(getValues(leafscan)<=0.9*255)] = 0
    leafscan[which(getValues(leafscan)>0.9*255)] = 255
    leafscan[which(getValues(leafscan)==255)] = 1
    
  }else{
    leafscan[which(getValues(leafscan)<=0.9)] = 0
    leafscan[which(getValues(leafscan)>0.9)] = 1
  }
  leafscan[which(getValues(leafscan)==1)] = 2
  leafscan[which(getValues(leafscan)==0)] = 1
  leafscan[which(getValues(leafscan)==2)] = NA
  
  #clean boarders and common scan artefacts
  leafscan[,1:5] = NA
  leafscan[,(dim(leafscan)[2]-5):(dim(leafscan)[2])] = NA
  leafscan[1:5,] = NA
  leafscan[(dim(leafscan)[1]-5):(dim(leafscan)[1]),] = NA
  
  #detect segments
  leafsegments  = clump(leafscan)
  #calculate area per polygon
  scan_pixels = dim(leafscan)[1]*dim(leafscan)[2]
  area_all = freq(leafsegments)[-which(is.na(freq(leafsegments)[,1])==TRUE),2] * 21.0 * 29.7 / scan_pixels
  
  #choose only segments greater than a minimum size
  leafselection = which(area_all>=min_size)
  leafstestID = na.omit(freq(leafsegments)[leafselection,1])
  leafstest = area_all[leafselection]
  
  #write pic (for checking)
  test = leafsegments
  test[which(is.na(match(getValues(leafsegments),leafstestID))==FALSE)] = 2
  test[which(is.na(match(getValues(leafsegments),leafstestID))==TRUE)] = NA
  png(file = paste0("output/plot_leaves_",allscans[i],".png"),width = 600,units="px", height = 800, res = 100,bg = "white") # areas in red indicate segments that have been removed
    plot(leafscan, col=c("white","red"))
    plot(test, add=TRUE, col=c("white", "black"))
  dev.off()
  
  #write results to table
  result_mean[i,1] = allscans[i]
  result_mean[i,2] = round(mean(leafstest), 4)
  result_mean[i,3] = length(leafstest)
  
}

result_mean
write.csv(result_mean, file="output/leaf_area_individual.csv", row.names = F, sep=";")
