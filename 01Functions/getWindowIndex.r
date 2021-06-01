#### FolderRoute <- "F:/TempMatching"
#### CurrentFileName <- paste0(FolderRoute,"/02RawGCxGCMSData/blanc_1.cdf")
#### modulation_time <- 7 #### seconds
#### Window1d <- c(400,500)
#### Window2d <- c(2,3)
#### library(ncdf4)
#### data <- nc_open( CurrentFileName, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE)
#### TIC <- ncvar_get(data, varid = "total_intensity")
#### Function1 <- paste0(FolderRoute,"/01Functions/DataIndexDict.r")
#### source(Function1)
#### IndexTable <- DataIndexDict(modulation_time,CurrentFileName)
#### Function2 <- paste0(FolderRoute,"/01Functions/getWindowIndex.r")
#### source(Function2)
#### WindowIndex <- getWindowIndex(Window1d, Window2d,IndexTable, modulation_time,CurrentFileName)


getWindowIndex <- function(Window1d, Window2d,IndexTable, modulation_time,CurrentFileName){


############################################## load from data file ###############################################
library(ncdf4)
data <- nc_open( CurrentFileName, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )
scan_acquisition_time <- ncvar_get(data, varid = "scan_acquisition_time")

####################################### calculate the border for IndxeTable #####################################
WindowLeft <- which(colnames(IndexTable) == floor((Window1d[1]-min(scan_acquisition_time))/modulation_time)*modulation_time + min(scan_acquisition_time)) ###on x-axis column name
WindowRight <- which(colnames(IndexTable) == floor((Window1d[2]-min(scan_acquisition_time))/modulation_time)*modulation_time + min(scan_acquisition_time)) ###on x-axis column name
WindowBot <- which(rownames(IndexTable) == Window2d[1]) ###on y-axis row name
WindowTop <- which(rownames(IndexTable) == Window2d[2]) ###on y-axis row name
WindowIndex <- sort(as.vector(IndexTable[WindowTop:WindowBot,WindowLeft:WindowRight]))



return(WindowIndex)   

}