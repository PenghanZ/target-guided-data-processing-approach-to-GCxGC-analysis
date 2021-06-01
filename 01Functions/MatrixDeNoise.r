#### FolderRoute <- "F:/TempMatching"
#### filename <- paste0(FolderRoute,"/02RawGCxGCMSData/blanc_1 edited.cdf")
#### modulation_time <- 7 #### seconds
#### library(ncdf4)
#### data <- nc_open( filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )
#### TICdata <- ncvar_get(data, varid = "total_intensity") 

#### Xsd <-4 
#### Function1 <- paste0(FolderRoute,"/01Functions/MatrixDeNoise.r")
#### source(Function1)
#### DeNoiseMatrix <- MatrixDeNoise(filename, TICdata, modulation_time, Xsd)
#### Function1 <- paste0(FolderRoute,"/01Functions/AttributeMatrixPlot.r")
#### source(Function1)
#### AttributeMatrixPlot(DeNoiseMatrix,modulation_time,filename)
#### View(DeNoiseMatrix[1:30,1:30])
#### View(SignalMatrix[1:30,1:30])

MatrixDeNoise <- function(filename, Attribute,modulation_time, Xsd){
library(ncdf4)
data <- nc_open(filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )


############################################## reform into matrix  ###############################################
function3 <- paste0(FolderRoute,"/01Functions/AttributeMatrix.r")

source(function3)
SignalMatrix <- AttributeMatrix(Attribute,modulation_time,filename)
NoiseSDVector <- matrix(nrow = 2, ncol = dim(SignalMatrix)[2])
DeNoiseSignalMatrix <- SignalMatrix

########################################### detect noise and denoise############################################

for (i in 1: dim(SignalMatrix)[2]){
Step <- SignalMatrix[,i]
Step <- Step[which(Step != 0)] 
MinFive <- sort(Step)[1:5]
AvgMinFive <- floor(mean(MinFive))
LocalSignalIndex <- c(which(Step %in% MinFive)-1, which(Step %in% MinFive), which(Step %in% MinFive)+1)
LocalSignalIndex <- LocalSignalIndex[which(LocalSignalIndex %in% c(1:dim(SignalMatrix)[1]))]
StandDev <- floor(sd(Step[LocalSignalIndex]))
MaxNoise <- AvgMinFive + Xsd * StandDev
NoiseSDVector[1,i] <- MaxNoise
NoiseSDVector[2,i] <- StandDev

DeNoiseSignalMatrix[,i] <- SignalMatrix[,i] - NoiseSDVector[1,i]

}

DeNoiseSignalMatrix[DeNoiseSignalMatrix < 0] <- 0
############################################################################

#return(NoiseVector)
return(DeNoiseSignalMatrix)
}
