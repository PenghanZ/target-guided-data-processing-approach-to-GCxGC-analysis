#### FolderRoute <- "F:/TempMatching"
#### FolderRoute <- "D:/TempMatching"
#### filename <- paste0(FolderRoute,"/02RawGCxGCMSData/All_Std_mix_1_8_1.cdf")
#### modulation_time <- 7 #### seconds
#### Function1 <- paste0(FolderRoute,"/01Functions/FillIntensityValue.r")
#### source(Function1)
#### FilledIntensityValue <- FillIntensityValue(filename,modulation_time)



FillIntensityValue <- function(filename, modulation_time){
library(ncdf4)
data <- nc_open( filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )

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

IntensityLength <- total_resolution * MzRangeLength


mass_values <- ncvar_get(data, varid = "mass_values")
intensity_values <- ncvar_get(data, varid = "intensity_values")
point_count <- ncvar_get(data, varid = "point_count")
scan_index <- ncvar_get(data, varid = "scan_index")


FilledIntensityValue <- vector(length = IntensityLength)

for (PointCountIndex in 1:length(point_count)){
# print(PointCountIndex)

if(point_count[PointCountIndex] < MzRangeLength){

MissingMass <- mass_values[(scan_index[PointCountIndex]+1) : (scan_index[PointCountIndex]+point_count[PointCountIndex]) ]
MissingIntensity <- intensity_values[(scan_index[PointCountIndex]+1) : (scan_index[PointCountIndex]+point_count[PointCountIndex]) ]
FilledMissingIntensity <- vector()

CrtMassValue <- MzRange

for (MassValueIndex in 1:length(MissingMass)){

MassValueDif <- MissingMass[MassValueIndex] - CrtMassValue[MassValueIndex]

if(MassValueDif == 0){
FilledMissingIntensity <- c(FilledMissingIntensity, MissingIntensity[MassValueIndex])
 }else{
FilledMissingIntensity <- c(FilledMissingIntensity, rep(0,time = MassValueDif), MissingIntensity[MassValueIndex])

CrtMassValue <- CrtMassValue[-c(MassValueIndex : (MassValueIndex + MassValueDif -1))]
}
}

FilledIntensityValue[((PointCountIndex-1) * MzRangeLength+1 ): (PointCountIndex * MzRangeLength)] <- FilledMissingIntensity

 }else{

NoMissingIntensity <- intensity_values[(scan_index[PointCountIndex]+1) : (scan_index[PointCountIndex]+point_count[PointCountIndex]) ]
FilledIntensityValue[((PointCountIndex-1) * MzRangeLength+1 ): (PointCountIndex * MzRangeLength)] <- 
NoMissingIntensity

}

}



############################################################################

return(FilledIntensityValue)
}
