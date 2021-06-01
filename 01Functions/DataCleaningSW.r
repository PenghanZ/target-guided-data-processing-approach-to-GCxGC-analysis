#### WindowStart = 300 ###seconds
#### WindowEnd = 1800 ###seconds
#### filename <- "F:\\blanc_1.cdf" ####
#### "Attribute" ========----> 	data <- nc_open( filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )
#### 		 ========---->	total_intensity <- ncvar_get(data, varid = "total_intensity") ####





#### Data Cleaning for Selected Window
DataCleaningSW <- function(Attribute,WindowStart, WindowEnd, filename){

### load information from datafile
library(ncdf4)
data <- nc_open( filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )

scan_acquisition_time <- ncvar_get(data, varid = "scan_acquisition_time")
scan_duration <- ncvar_get(data, varid = "scan_duration")

### locate the cleaning border
if (WindowStart > min(scan_acquisition_time)){
WindowStartData = (WindowStart - min(scan_acquisition_time))/scan_duration[1]
} else {
WindowStartData = 1}

if (WindowEnd < max(scan_acquisition_time)){
WindowEndData = (WindowEnd - min(scan_acquisition_time))/scan_duration[1]
} else {
WindowEndData = length(Mz40)}

### remove the off window data
if (WindowStartData > 1){
Attribute[1:WindowStartData] = 0}

if (WindowEndData < length(Mz40)){
Attribute[WindowEndData:length(Mz40)] = 0}

return(Attribute)  ####cleaned data output
}