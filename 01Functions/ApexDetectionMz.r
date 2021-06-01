
# FolderRoute <- "F:/TempMatching"
# FolderRoute <- "D:/TempMatching"
# Function1 <- paste0(FolderRoute,"/01Functions/sG2dSmoothing.r")
# source(Function1)
# MatrixData2D <- CroppedPeakRegionMS[,,40]
# sGPkMz <- sG2dSmoothing(MatrixData2D, WinColLength, WinRowLength)

# Function10 <- paste0(FolderRoute,"/01Functions/ApexDetectionMz.r")
# source(Function10)
# OperatorColLength <- 5
# OperatorRowLength <- 21 ## 0.1 second min peak width
# ModulationTime <- 7
# MSScanHz <- 0.005
# MzValue <- 40
# ApexResultMz <- ApexDetectionMz(sGPkMz, OperatorColLength, OperatorRowLength, ModulationTime, MSScanHz, MzValue)


ApexDetectionMz <- function(sGPkMz, OperatorColLength, OperatorRowLength, ModulationTime, MSScanHz,MzValue){

RowLength <- dim(sGPkMz)[1];
ColLength <- dim(sGPkMz)[2];
# zerofilling for operator sliding
FillingColNum <- (OperatorColLength-1)/2
FillingRowNum <- (OperatorRowLength-1)/2

# colnames and rownames for extented matrix
sGPkMzColNames <- colnames(sGPkMz)
sGPkMzRowNames <- rownames(sGPkMz)
sGPkMzPlusColNames <- seq(from=min(as.numeric(sGPkMzColNames))-FillingColNum*ModulationTime,to=max(as.numeric(sGPkMzColNames))+FillingColNum*ModulationTime,by=ModulationTime)
sGPkMzPlusRowNames <- rev(seq(from=min(as.numeric(sGPkMzRowNames))-FillingRowNum*MSScanHz,to=max(as.numeric(sGPkMzRowNames))+FillingRowNum*MSScanHz,by=MSScanHz))

# extent matrix
FillingCol <- matrix(0,RowLength, FillingColNum)
sGPkMzPlus <- cbind(FillingCol,sGPkMz,FillingCol)
FillingRow <- matrix(0,FillingRowNum,ColLength+FillingColNum*2)
sGPkMzPlus <- rbind(FillingRow,sGPkMzPlus,FillingRow)
colnames(sGPkMzPlus) <- sGPkMzPlusColNames
rownames(sGPkMzPlus) <- sGPkMzPlusRowNames

# Result table 
ApexResultMz <- matrix(0,dim(sGPkMz)[1], dim(sGPkMz)[2])
colnames(ApexResultMz) <- sGPkMzColNames
rownames(ApexResultMz) <- sGPkMzRowNames

# sliding the window
MidPosition <- (OperatorColLength-1)/2*OperatorRowLength + (OperatorRowLength+1)/2
LowSignalLimit <- quantile(sGPkMz[which(sGPkMz>0)],  probs = 0.25)
MaxSignalLimit <- max(sGPkMz)/10

for(ColNum in seq(dim(sGPkMz)[2])){
for(RowNum in seq(dim(sGPkMz)[1])){
# ColNum <- 1
# RowNum <- 49 
CurrentMzWindow <- sGPkMzPlus[RowNum:(RowNum+OperatorRowLength-1),ColNum:(ColNum+OperatorColLength-1)]
MaxPosition <- which(CurrentMzWindow == max(CurrentMzWindow))
 
Condition1 <- MaxPosition == MidPosition # max at the mid position
Condition2 <- CurrentMzWindow[MaxPosition] > MaxSignalLimit
Condition3 <- CurrentMzWindow[MaxPosition-1] > LowSignalLimit
Condition4 <- CurrentMzWindow[MaxPosition+1] > LowSignalLimit
ConditionSum <- all(Condition1,Condition2,Condition3,Condition4) # is all conditions are true
# ConditionSum <- all(Condition2,Condition3)

if(ConditionSum){
ApexResultMz[RowNum,ColNum] <- MzValue
}

}
}



return(ApexResultMz)
}