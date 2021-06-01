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
### UniquePkLabel <- getUniquePkLabel(PkLabeledTIC, TemplateRT, DeltaRT1, DeltaRT2)


### PkLabelNum <- 1
### DeNoisedDataName <- "DeNoisedData1.RData"

### Function9 <- paste0(FolderRoute,"/01Functions/getCrtMS4LabeledPk.r")
### source(Function9)
### MS4LabeledPk <- getCrtMS4LabeledPk(CurrentFileName, PkLabeledTIC, UniquePkLabel, DeNoisedDataName, PkLabelNum)



getCrtMS4LabeledPk <- function(CurrentFileName, PkLabeledTIC, UniquePkLabel, DeNoisedDataName, PkLabelNum){

#get the signal region index
library(ncdf4)
data <- nc_open( CurrentFileName, write=TRUE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )
TICdata <- ncvar_get(data, varid = "total_intensity")

Function10 <- paste0(FolderRoute,"/01Functions/DataIndexDict.r")
source(Function10)

IndexTable <- DataIndexDict(modulation_time,CurrentFileName)
PixelIndex <- IndexTable[which(PkLabeledTIC == UniquePkLabel[PkLabelNum])]

#load the baseline corrected signal
#DeNoisedDataFullName <- paste0(FolderRoute,"/03BaselineCorrectData/", DeNoisedDataName)
#load(DeNoisedDataFullName)


#prepare spread sheet for Mass spectrum
PkRegionLength <- length(PixelIndex)
MzMin <- ncvar_get(data, varid = "mass_range_min")[1]
MzMax <- ncvar_get(data, varid = "mass_range_max")[1]
MzRange <- c(MzMin:MzMax)
MzLength <- length(MzRange)


AllMSbyPixel <- matrix(nrow = PkRegionLength, ncol = MzLength) 
colnames(AllMSbyPixel) <- MzRange
rownames(AllMSbyPixel) <- PixelIndex

for (PixelNum in 1 : PkRegionLength){

PixelRCindex <- which(IndexTable == PixelIndex[PixelNum], arr.ind = TRUE)
PixelMass <- DeNoisedData[PixelRCindex[1],PixelRCindex[2],]
AllMSbyPixel[PixelNum,] <- PixelMass

}


return(AllMSbyPixel)

# plot the TIC
# rows <- dim(DeNoisedData)[1]
# cols <- dim(DeNoisedData)[2]

# TIC <- matrix(0, nrow=rows, ncol=cols)
# colnames(TIC) <- colnames(DeNoisedData[,,40])
# rownames(TIC) <- rownames(DeNoisedData[,,40])
# for (i in seq(rows)) {
#  for (j in seq(cols)) {
#    TIC[i,j] <- sum(DeNoisedData[i,j,])
#  }
# }

# Function2 <- paste0(FolderRoute,"/01Functions/AttributeMatrixPlot.r")
# source(Function2)
# AttributeMatrixPlot(TIC,modulation_time, CurrentFileName)

}
