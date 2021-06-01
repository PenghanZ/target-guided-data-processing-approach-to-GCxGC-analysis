#### FolderRoute <- "F:/TempMatching"
#### filename <- paste0(FolderRoute,"/02RawGCxGCMSData/blanc_1 edited.cdf")
#### modulation_time <- 7 #### seconds
#### XSlice <- 10
#### XSignal <- 1.5
#### Function1 <- paste0(FolderRoute,"/01Functions/PkLabelTIC.r")
#### source(Function1)
#### PkPkLabeledTICTIC <- PkLabelTIC(filename,modulation_time)



PkLabelTIC2 <- function(filename,modulation_time, XSlice, XSignal){


library(ncdf4)
data <- nc_open(filename, write=TRUE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )
TICdata <- ncvar_get(data, varid = "total_intensity")

Function2 <- paste0(FolderRoute,"/01Functions/MatrixDeNoise4PkLabel.r")
source(Function2)
DeNioseTIC <- MatrixDeNoise4PkLabel(filename, TICdata, modulation_time, XSlice, XSignal)

#source("D:\\my functions\\AttributePlot.r")
#library(ggplot2)
#AttributePlot(TICdata,modulation_time,filename)
#AttributePlot(DeNioseTIC,modulation_time,filename)

################### Label the peak region ###########################
###convert the data into binary for the connect region labeling
DeNioseTICBinary <- DeNioseTIC
DeNioseTICBinary[DeNioseTIC>0] <- 1
#AttributePlot(DeNioseTICBinary,modulation_time,filename)

########################### DeNioseTICBinary matrix ###########################
Function3 <- paste0(FolderRoute,"/01Functions/BWConnLabel.r")
source(Function3)
PkLabeledTIC <- BWConnLabel(DeNioseTICBinary)

########################### adding the axis to the PkLabeledTIC matrix ###########################
scan_acquisition_time <- ncvar_get(data, varid = "scan_acquisition_time")
MS_start_time <- min(scan_acquisition_time)
MS_end_time <- max(scan_acquisition_time)

total_resolution <- length(TICdata)
scan_duration <- ncvar_get(data, varid = "scan_duration")[1]
second_dimension_resolution <- modulation_time / scan_duration
first_dimension_resolution <- total_resolution / second_dimension_resolution

###################################### prepare the x&y-axis for the DataIndexMatrix ##############################
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

rownames(PkLabeledTIC) <- second_dimension_axis
colnames(PkLabeledTIC) <- first_dimension_axis

return(PkLabeledTIC)

}
