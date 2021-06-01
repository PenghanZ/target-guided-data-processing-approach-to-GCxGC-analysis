# FolderRoute <- "F:/TempMatching"
# FolderRoute <- "D:/TempMatching"
# library(readr)
# Pk1Mz40 <- read_csv("C:/Users/zhangp/Desktop/Pk1Mz40.csv")
# Pk1Mz40 <- as.matrix(Pk1Mz40)
# row.names(Pk1Mz40) <- Pk1Mz40[,1]
# MatrixData2D <- Pk1Mz40[,-1]
# WinColLength <- 3
# WinRowLength <- 7
# Function11 <- paste0(FolderRoute,"/01Functions/MA2dSmoothing.r")
# source(Function11)

# MA2dSmoothing(MatrixData2D, WinColLength, WinRowLength)


MA2dSmoothing <- function(MatrixData2D, WinColLength, WinRowLength){
extRowNum <- floor(WinRowLength/2)
extColNum <- floor(WinColLength/2)
extRow <- matrix(0,extRowNum,dim(MatrixData2D)[2])
extMatrixData2D <- rbind(extRow,MatrixData2D,extRow)
extCol <- matrix(0,dim(extMatrixData2D)[1],extColNum)
extMatrixData2D <- cbind(extCol,extMatrixData2D,extCol)

MASmoothMatrixData2D <- matrix(,dim(MatrixData2D)[1],dim(MatrixData2D)[2])
colnames(MASmoothMatrixData2D) <- colnames(MatrixData2D)
rownames(MASmoothMatrixData2D) <- rownames(MatrixData2D)

for(eachRow in seq(dim(MatrixData2D)[1])){
# eachRow <- seq(dim(MatrixData2D)[1])[1]
for(eachCol in seq(dim(MatrixData2D)[2])){
# eachCol <- seq(dim(MatrixData2D)[1])[1]
SlideWindow <- extMatrixData2D[eachRow:(eachRow+WinRowLength-1),eachCol:(eachCol+WinColLength-1)]
MA <- mean(SlideWindow) 
MASmoothMatrixData2D[eachRow, eachCol] <- MA
}
}


return(MASmoothMatrixData2D)
}