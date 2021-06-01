#### "Attribute" ========----> 	data <- nc_open( filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE )
#### 		 ========---->	total_intensity <- ncvar_get(data, varid = "total_intensity") ####
#### BinNumber = 10 ####
#### SNR = 3 for LOD = 10 for LOQ

DataCleaningSAT <- function(Attribute,BinNumber, SNR){

############################ calculate the bin size ############################
IfNonZero <- !Attribute == 0
TotalPopulation <- length(Attribute[IfNonZero])
BinPopulation <- ceiling(TotalPopulation/BinNumber)
StartIndex <- min(which(IfNonZero == TRUE))

########################### thresholding for each bin ##########################

for( i in 1 : BinNumber){

BinDataIndexStart = StartIndex + (i-1)* BinPopulation
BinDataIndexEnd = StartIndex + i* BinPopulation -1

BinData <- Attribute[BinDataIndexStart:BinDataIndexEnd]
#MinNoise <- min(BinData[BinData>0])
#BinDataHist <- hist(BinData,breaks = 1000)
#MidNoise <- BinDataHist$mids[which.max(BinDataHist$counts)]
#NoiseThreshold <- (MidNoise-MinNoise)*2 * SNR
MedNoise <- median(BinData[BinData>0])
NoiseThreshold <- MedNoise * SNR

BinData[BinData < NoiseThreshold] <- 0
Attribute[BinDataIndexStart:BinDataIndexEnd] <- BinData

}

############################################################################

return(Attribute)
}
