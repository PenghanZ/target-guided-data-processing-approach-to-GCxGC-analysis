#### FolderRoute <- "D:/TempMatchingSatur"
#### RawDataFolder <- paste0(FolderRoute,"/02RawGCxGCMSData")
#### FileList <- list.files(RawDataFolder)
#### CurrentFileName <- paste0(RawDataFolder,"/",FileList[1])
#### modulation_time <- 7 #### seconds
#### library(ncdf4)
#### data <- nc_open(CurrentFileName, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )
#### TICdata <- ncvar_get(data, varid = "total_intensity")
#### Function1 <- paste0(FolderRoute,"/01Functions/AttributeMatrix.r")
#### source(Function1)
#### TICMatrix <- AttributeMatrix(TICdata,modulation_time,CurrentFileName)

AttributeMatrix <- function(Attribute,modulation_time,filename){

library(ncdf4)
data <- nc_open( filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )

############################################## load from data file ###############################################
scan_acquisition_time <- ncvar_get(data, varid = "scan_acquisition_time")
MS_start_time <- min(scan_acquisition_time)
MS_end_time <- max(scan_acquisition_time)

total_resolution <- length(Attribute)
scan_duration <- ncvar_get(data, varid = "scan_duration")
second_dimension_resolution <- modulation_time / scan_duration[1]
first_dimension_resolution <- total_resolution / second_dimension_resolution

############################################### creating the matirx ##############################################
Attribute_matrix <- matrix(Attribute,nrow = second_dimension_resolution, ncol = first_dimension_resolution)

###################################### prepare the x&y-axis for the Attribute_matrix ##############################

Attribute_matrix_updown <- Attribute_matrix[dim(Attribute_matrix)[1]:1,]

#1st dimension axis 
first_dimension_axis <- matrix(nrow = 1, ncol = first_dimension_resolution)
for(i in 1:first_dimension_resolution) {
first_dimension_axis[1,i]= MS_start_time + (i-1)* modulation_time
}

#2nd dimension axis
second_dimension_axis <- matrix(nrow = second_dimension_resolution, ncol = 1)
for(j in 1:second_dimension_resolution) {
second_dimension_axis[j,1]= 0 + (j-1)*0.005
}
second_dimension_axis <- as.matrix(second_dimension_axis[dim(second_dimension_axis)[1]:1,])

rownames(Attribute_matrix_updown) <- second_dimension_axis
colnames(Attribute_matrix_updown) <- first_dimension_axis

return(Attribute_matrix_updown)   ### Attribute_matrix_updown = AttributeMatrix

}
