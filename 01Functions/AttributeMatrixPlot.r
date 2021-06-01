#### FolderRoute <- "F:/TempMatching"
#### filename <- paste0(FolderRoute,"/02RawGCxGCMSData/blanc_1.cdf")
#### modulation_time <- 7 #### seconds
#### library(ncdf4)
#### data <- nc_open( filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )
#### TICdata <- ncvar_get(data, varid = "total_intensity") 
#### Function1 <- paste0(FolderRoute,"/01Functions/AttributeMatrix.r")
#### source(Function1)
#### TICMatrix <- AttributeMatrix(TICdata,modulation_time,filename)
#### Function2 <- paste0(FolderRoute,"/01Functions/AttributeMatrixPlot.r")
#### source(Function2)
#### AttributeMatrixPlot(TICMatrix,modulation_time, filename)
#### if error, check the rowname and the colname of the datamatrix


AttributeMatrixPlot <- function(AttributeMatrix, modulation_time, filename){

library(ncdf4)
data <- nc_open( filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )

total_resolution <- length(AttributeMatrix)
scan_duration <- ncvar_get(data, varid = "scan_duration")
second_dimension_resolution <- modulation_time / scan_duration[1]
first_dimension_resolution <- total_resolution / second_dimension_resolution

first_dimension_axis <- as.numeric(colnames(AttributeMatrix)) ### AttributeMatrix is Attribute_matrix_updown
second_dimension_axis <- as.numeric(rownames(AttributeMatrix)) ### AttributeMatrix is Attribute_matrix_updown

###convert data matrix into data frame###

GC1d <- vector(mode = "numeric", length = total_resolution)
GC2d <- vector(mode = "numeric", length = total_resolution)
Intensity <- vector(mode = "numeric", length = total_resolution)

for(i in 1:first_dimension_resolution) {
for(j in 1:second_dimension_resolution) {

#data points for color map
Intensity[(i-1)*second_dimension_resolution+j] = AttributeMatrix[j,i]

#1st dimention time index
GC1d[(i-1)*second_dimension_resolution+j] = first_dimension_axis[i]

#2nd dimention time index
GC2d[(i-1)*second_dimension_resolution+j] = second_dimension_axis[j]

}
}
RootedIntensity <- Intensity^(1/3) ###(1/n)
ColormapDataFrame <- data.frame(GC1d, GC2d, RootedIntensity)


###plot###
library(ggplot2)
ggplot(ColormapDataFrame, aes(GC1d, GC2d)) + geom_raster(aes(fill = RootedIntensity))

}