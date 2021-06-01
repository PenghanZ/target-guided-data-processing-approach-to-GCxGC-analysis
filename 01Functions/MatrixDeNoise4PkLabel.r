#### FolderRoute <- "D:/TempMatching10xWMStd"
#### filename <- paste0(FolderRoute,"/02RawGCxGCMSData/10xMW+20mgStds1_1.cdf")
#### modulation_time <- 7 #### seconds
#### library(ncdf4)
#### data <- nc_open( filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )
#### Attribute <- ncvar_get(data, varid = "total_intensity")
#### XSlice <- 10

#### XSignal <- 3 #as the min qualitive detection requirement
#### Function1 <- paste0(FolderRoute,"/01Functions/MatrixDeNoise4PkLabel.r")
#### source(Function1)
#### DeNoiseMatrix <- MatrixDeNoise4PkLabel(filename, Attribute, modulation_time, XSlice, XSignal)
#### Function1 <- paste0(FolderRoute,"/01Functions/AttributeMatrixPlot.r")
#### source(Function1)
#### AttributeMatrixPlot(DeNoiseMatrix,modulation_time,filename)
#### View(DeNoiseMatrix[1:30,1:30])
#### View(SignalMatrix[1:30,1:30])

MatrixDeNoise4PkLabel <- function(filename, Attribute, modulation_time, XSlice, XSignal){
library(ncdf4)
data <- nc_open( filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )
NoiseMatrix <- matrix(dim(Attribute))

############################################## reform into matrix  ###############################################
function3 <- paste0(FolderRoute,"/01Functions/AttributeMatrix.r")

source(function3)
SignalMatrix <- AttributeMatrix(Attribute,modulation_time,filename)
DeNoiseSignalMatrix <- SignalMatrix

########################################### detect noise and denoise############################################
#Slice the SignalMatrix
Sclices <- seq(from=1, to=dim(SignalMatrix)[2], by=floor((dim(SignalMatrix)[2]-1)/(XSlice)))
Sclices[length(Sclices)] <- dim(SignalMatrix)[2]
NoiseSDVector <- matrix(nrow = 2, ncol = (length(Sclices)-1))

for (i in 1: (length(Sclices)-1)){
#i <- 2
Step <- SignalMatrix[,Sclices[i]:Sclices[i+1]]
MinFive <- sort(Step)[1:5]
AvgMinFive <- floor(mean(MinFive))
LocalSignalIndex <- c(which(Step %in% MinFive)-1, which(Step %in% MinFive), which(Step %in% MinFive)+1)
LocalSignalIndex2 <- LocalSignalIndex[which(LocalSignalIndex %in% c(1:length(Step)))]
StandDev <- floor(sd(Step[LocalSignalIndex2]))
BGNoiseTH <- (AvgMinFive + 2 * StandDev) * XSignal
NoiseSDVector[1,i] <- BGNoiseTH
NoiseSDVector[2,i] <- StandDev

DeNoiseSignalMatrix[,Sclices[i]:Sclices[i+1]] <- SignalMatrix[,Sclices[i]:Sclices[i+1]] - NoiseSDVector[1,i]

}

DeNoiseSignalMatrix[DeNoiseSignalMatrix < 0] <- 0
############################################################################

#return(NoiseVector)
return(DeNoiseSignalMatrix)
}
