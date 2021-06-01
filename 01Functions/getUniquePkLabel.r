### FolderRoute <- "D:/TempMatching"
### CurrentFileName <- paste0(FolderRoute,"/02RawGCxGCMSData/blanc_1 edited.cdf")
### modulation_time <- 7 #### seconds
### Function7 <- paste0(FolderRoute,"/01Functions/PkLabelTIC.r")
### source(Function7)
### PkLabeledTIC <- PkLabelTIC(CurrentFileName,modulation_time)
### TemplateRT <- c(280, 2)
### DeltaRT1 <- 15 #in seconds
### DeltaRT2 <- 1 #in seconds
### Function9 <- paste0(FolderRoute,"/01Functions/getUniquePkLabel.r")
### source(Function9)
### UniquePkLabel <- getUniquePkLabel(CurrentFileName, PkLabeledTIC, TemplateRT, DeltaRT1, DeltaRT2)




getUniquePkLabel <- function(CurrentFileName, PkLabeledTIC, TemplateRT, DeltaRT1, DeltaRT2){

Window1d <- c(TemplateRT[1]-DeltaRT1,TemplateRT[1]+DeltaRT1)
Window2d <- c(TemplateRT[2]-DeltaRT2,TemplateRT[2]+DeltaRT2)



########################### check the window limits ##################


library(ncdf4)
data <- nc_open( CurrentFileName, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )
scan_acquisition_time <- ncvar_get(data, varid = "scan_acquisition_time")

MS_start_time <- min(scan_acquisition_time)
MS_end_time <- max(scan_acquisition_time)

###check Fist dimention
if (min(Window1d) < min(scan_acquisition_time)) {
Window1d[1] <- min(scan_acquisition_time) }

if (max(Window1d) > max(scan_acquisition_time)) {
Window1d[2] <- max(scan_acquisition_time) }

###check Second dimention
if (min(Window2d) < 0) {
Window2d[1] <- 0}

if (max(Window2d) > modulation_time) {
Window2d[2] <- modulation_time }

WindowLeft <- which(colnames(PkLabeledTIC) == as.character(floor((Window1d[1]-min(scan_acquisition_time))/modulation_time)*modulation_time + min(scan_acquisition_time))) ###on x-axis column name
WindowRight <- which(colnames(PkLabeledTIC) == as.character(floor((Window1d[2]-min(scan_acquisition_time))/modulation_time)*modulation_time + min(scan_acquisition_time))) ###on x-axis column name
WindowBot <- which(rownames(PkLabeledTIC) == as.character(signif(Window2d[1], digits = 3))) ###on y-axis row name
WindowTop <- which(rownames(PkLabeledTIC) == as.character(signif(Window2d[2], digits = 3))) ###on y-axis row name

WindowPkLabeledTIC <- PkLabeledTIC[WindowTop:WindowBot,WindowLeft:WindowRight]
UniquePkLabel <- unique(as.vector(WindowPkLabeledTIC))
UniquePkLabel <- UniquePkLabel[UniquePkLabel != 0]

return(UniquePkLabel)

}
