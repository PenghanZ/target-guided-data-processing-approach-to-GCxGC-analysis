#### MzValue <- 40
#### FolderRoute <- "F:/TempMatching"
#### filename <- paste0(FolderRoute,"/02RawGCxGCMSData/blanc_1 edited.cdf")
#### Function1 <- paste0(FolderRoute,"/01Functions/SingleMzExtract.r")
#### source(Function1)
#### MzData <- SingleMzExtract(MzValue, filename)


SingleMzExtract <- function(MzValue,filename){

############################ open the data file ######################
library(ncdf4)
data <- nc_open( filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )

########################### extract interesting data ##################
#mass_values <- ncvar_get(data, varid = "mass_values")
#MzIndex <- mass_values == MzValue

MzIndex <- ncvar_get(data, varid = "mass_values") == MzValue

#rm(mass_values)

#intensity_values <- ncvar_get(data, varid = "intensity_values")
#MzIntensity <- intensity_values[MzIndex]

MzIntensity <- ncvar_get(data, varid = "intensity_values")[MzIndex]

return(MzIntensity)
}