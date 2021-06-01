####need to define "Attribute" ;"modulation_time";"filename"
#### FolderRoute <- "F:/TempMatching"
#### filename <- paste0(FolderRoute,"/02RawGCxGCMSData/blanc_1.cdf")
#### modulation_time <- 7 #### seconds
#### Function1 <- paste0(FolderRoute,"/01Functions/DataIndexDict.r")
#### source(Function1)
#### IndexTable <- DataIndexDict(modulation_time,filename)

DataIndexDict <- function(modulation_time,filename){

library(ncdf4)
data <- nc_open( filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )

############################################## load from data file ###############################################
scan_acquisition_time <- ncvar_get(data, varid = "scan_acquisition_time")
MS_start_time <- min(scan_acquisition_time)
MS_end_time <- max(scan_acquisition_time)

TIC <- ncvar_get(data, varid = "total_intensity")
total_resolution <- length(TIC)
DataIndex <- c(1:total_resolution)
scan_duration <- ncvar_get(data, varid = "scan_duration")
second_dimension_resolution <- modulation_time / scan_duration[1]
first_dimension_resolution <- total_resolution / second_dimension_resolution

############################################### creating the matirx ##############################################
DataIndexMatrix <- matrix(DataIndex,nrow = second_dimension_resolution, ncol = first_dimension_resolution)

###################################### prepare the x&y-axis for the DataIndexMatrix ##############################

DataIndexMatrixUpdown <- DataIndexMatrix[dim(DataIndexMatrix)[1]:1,]

#1st dimension axis 
first_dimension_axis <- matrix(nrow = 1, ncol = first_dimension_resolution)
for(i in 1:first_dimension_resolution) {
first_dimension_axis[1,i]= MS_start_time + (i-1)* modulation_time
}

#2nd dimension axis
second_dimension_axis <- matrix(nrow = second_dimension_resolution, ncol = 1)
for(j in 1:second_dimension_resolution) {
second_dimension_axis[j,1]= 0 + (j-1)* scan_duration[1]
}
second_dimension_axis <- as.matrix(second_dimension_axis[dim(second_dimension_axis)[1]:1,])

rownames(DataIndexMatrixUpdown) <- second_dimension_axis
colnames(DataIndexMatrixUpdown) <- first_dimension_axis

return(DataIndexMatrixUpdown)   ### DataIndexMatrixUpdown = DataIndexMatrix

}
