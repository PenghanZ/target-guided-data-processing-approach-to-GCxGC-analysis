
#### FolderRoute <- "D:/TempMatching"

#### filename <- paste0(FolderRoute,"/02RawGCxGCMSData/Std_mix_con_1_1.cdf")
#### modulation_time <- 7 #### seconds
#### Function1 <- paste0(FolderRoute,"/01Functions/BaselineCorrect.r")
#### source(Function1)
#### DeNoisedData <- BaselineCorrect(filename,modulation_time)
#### SaveRoute <- paste0(FolderRoute,"/03BaselineCorrectData/DeNoisedData1.RData")
#### save(DeNoisedData, file = SaveRoute)

#### cehck the denoise correction
#AvgVector <- vector(mode="numeric", length = dim(DeNoisedData[,,1])[2])

#for(i in 1:dim(DeNoisedData[,,1])[2]) {
#AvgVector[i] <- mean(DeNoisedData[,i,1])

#}



BaselineCorrect <- function(filename, modulation_time){
library(ncdf4)
#filename1 <- filename
data <- nc_open(filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )

scan_acquisition_time <- ncvar_get(data, varid = "scan_acquisition_time")
MS_start_time <- min(scan_acquisition_time)
MS_end_time <- max(scan_acquisition_time)

TIC <- ncvar_get(data, varid = "total_intensity")
total_resolution <- length(TIC)
scan_duration <- ncvar_get(data, varid = "scan_duration")
second_dimension_resolution <- modulation_time / scan_duration[1]
first_dimension_resolution <- total_resolution / second_dimension_resolution

MzMin <- ncvar_get(data, varid = "mass_range_min")[1]
MzMax <- ncvar_get(data, varid = "mass_range_max")[1]
MzRange <- c(MzMin:MzMax)
MzRangeLength <- length(MzRange)
FullMassValue <- rep(MzRange, times = total_resolution)

IntensityLength <- total_resolution * MzRangeLength

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


DeNoisedData <- array(0,dim = c(second_dimension_resolution, first_dimension_resolution, MzRangeLength), 
dimnames = list(second_dimension_axis, first_dimension_axis, MzRange) )

#Function2 <- paste0(FolderRoute,"/01Functions/SingleMzExtract.r")
Function3 <- paste0(FolderRoute,"/01Functions/MatrixDeNoise.r")

#source(Function2)
source(Function3)

Function4 <- paste0(FolderRoute,"/01Functions/FillIntensityValue.r")
source(Function4)

FilledIntensityValue <- FillIntensityValue(filename,modulation_time)


for(MzValue in MzMin:MzMax){
#for(MzValue in MzMin:40) {
#print(MzValue)
MzData <- FilledIntensityValue[FullMassValue == MzValue]

DeNioseDataMatrix <- MatrixDeNoise(filename, MzData, modulation_time, 4)
DeNoisedData[, ,which(MzRange == MzValue)] <- DeNioseDataMatrix

}

############################################################################
nc_close(data)
rm(BaselineCorrect)
rm(FillIntensityValue)
rm(MatrixDeNoise)

return(DeNoisedData)
}
