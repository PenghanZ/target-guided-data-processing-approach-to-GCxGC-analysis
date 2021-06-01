### Function9 <- paste0(FolderRoute,"/01Functions/getMS4LabeledPk.r")
### source(Function9)
### PkLabelNum <- 1
### MS4LabeledPk <- getMS4LabeledPk(PkLabeledTIC, UniquePkLabel, PkLabelNum)



getMS4LabeledPk <- function(PkLabeledTIC, UniquePkLabel, PkLabelNum){

library(ncdf4)
data <- nc_open( filename, write=TRUE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )
TICdata <- ncvar_get(data, varid = "total_intensity")

Function10 <- paste0(FolderRoute,"/01Functions/DataIndexDict.r")
source(Function10)

IndexTable <- DataIndexDict(modulation_time,filename)
SignalDataIndexPkRegion <- IndexTable[which(PkLabeledTIC == UniquePkLabel[PkLabelNum])] #to check the position[1.605, 355]

###for the each pixel in the peak region construct the MASS spectrum 
PkRegionLength <- length(SignalDataIndexPkRegion)
MzMin <- ncvar_get(data, varid = "mass_range_min")[1]
MzMax <- ncvar_get(data, varid = "mass_range_max")[1]
MzRange <- c(MzMin:MzMax)
MzLength <- length(MzRange)



MassValues <- ncvar_get(data, varid = "mass_values")
IntensityValues <- ncvar_get(data, varid = "intensity_values")

AllMSbyPixel <- matrix(nrow = PkRegionLength, ncol = MzLength) 


for (MzValueNum in 1 : MzLength){
print(MzValueNum)
PkRegionMZ <- vector(length = MzLength)

MzValue = MzRange[MzValueNum]
MzIndex <- which(MassValues == MzValue)[SignalDataIndexPkRegion]
MzIntensity <- IntensityValues[MzIndex]
PkRegionMZ <- MzIntensity
AllMSbyPixel[,MzValueNum] <- PkRegionMZ

}

rownames(AllMSbyPixel) <- SignalDataIndexPkRegion
colnames(AllMSbyPixel) <- MzRange

return(AllMSbyPixel)

}
