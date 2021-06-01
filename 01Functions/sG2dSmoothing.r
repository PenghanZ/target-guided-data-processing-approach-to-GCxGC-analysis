
#FolderRoute <- "F:/TempMatching"
#FolderRoute <- "D:/TempMatching"
#Function1 <- paste0(FolderRoute,"/01Functions/sG2dSmoothing.r")
#source(Function1)

#library(readr)
#Pk1Mz40 <- read_csv("C:/Users/zhangp/Desktop/Pk1Mz40.csv")
#Pk1Mz40 <- as.matrix(Pk1Mz40)
#row.names(Pk1Mz40) <- Pk1Mz40[,1]
#MatrixData2D <- Pk1Mz40[,-1]
#WinColLength <- 5
#WinRowLength <- 7

#sG2dSmoothing(MatrixData2D, WinColLength, WinRowLength)


sG2dSmoothing <- function(MatrixData2D, WinColLength, WinRowLength){
#对MatrixData2D做插值
#check the data size, does it have enough columes for the sliding window
#OriginalMatrixData2DColNum <- dim(MatrixData2D)[2]
#if(OriginalMatrixData2DColNum < WinColLength*2){
#InsertedMatrixData2D <- matrix(,dim(MatrixData2D)[1],2*OriginalMatrixData2DColNum-1)

#copy the original columes
#for (EachCol in seq(OriginalMatrixData2DColNum)){   
#InsertColIndex <- seq(OriginalMatrixData2DColNum)*2-1
#InsertedMatrixData2D[,InsertColIndex[EachCol]] <- MatrixData2D[,EachCol]
#}


#}



RowLength <- dim(MatrixData2D)[1];
ColLength <- dim(MatrixData2D)[2];
ord <- 3;  #fixed fitting order as 3 in this function, need to know the Jacobian matrix dimention in R

TotalPixelWin <- WinColLength*WinRowLength;

# moving window
WinCol <- seq(from = -(WinColLength-1)/2, to = (WinColLength-1)/2, by = 1)
WinRow <- seq(from = -(WinRowLength-1)/2, to = (WinRowLength-1)/2, by = 1)

WinIndex <- matrix(0, TotalPixelWin, 2)
count <- 1;
for(p in seq(WinColLength)){
for(q in seq(WinRowLength)){
WinIndex[count,1] <- WinCol[p];
WinIndex[count,2] <- WinRow[q];
count <- count +1;
}
}

A <- matrix(0, TotalPixelWin, 10) # The Jacobian has 10 columns for 3 order
for(nn in seq(TotalPixelWin)){
count <- 1;
for(nColLength in seq(from=0, to=ord, by=1)){
for(nRowLength in seq(from=0, to=ord, by=1)){
if(nColLength + nRowLength <=ord){
A[nn,count] <- WinIndex[nn,1]^nColLength*WinIndex[nn,2]^nRowLength;
count <- count+1;
}
}
}
}

AT <- t(A)
AT_A <- AT%*%A

F <- diag(TotalPixelWin);
c <- matrix(0,TotalPixelWin,TotalPixelWin);
for(nn in seq(TotalPixelWin)){ # excitation
CC <- solve(AT_A, AT%*%F[,nn])
for(ss in seq(TotalPixelWin)){ # location
c[ss,nn] <- 0;
count <- 1;
for(nColLength in seq(from=0, to=ord, by=1)){
for(nRowLength in seq(from=0, to=ord, by=1)){
if(nColLength + nRowLength <=ord){
c[ss,nn] <- c[ss,nn] + CC[count]*WinIndex[ss,1]^nColLength*WinIndex[ss,2]^nRowLength;
count <- count+1;
}
}
}
}
}

hWinColLength <- (WinColLength-1)/2;
hWinRowLength <- (WinRowLength-1)/2;
vOUT <- matrix(0, RowLength, ColLength);
colnames(vOUT) <- colnames(MatrixData2D)
rownames(vOUT) <- rownames(MatrixData2D)
# cal four corners (left-top, left-bottom, right-top, right-bottom)
# left-top
MatrixData2D_blk <- MatrixData2D[1:WinRowLength,1:WinColLength];
count <- 1;
for(q in seq(WinColLength)){
for(p in seq(WinRowLength)){
map <- matrix(c[count,],WinRowLength,WinColLength);
weightedM <- map*MatrixData2D_blk;
vOUT[p,q] <- sum(sum(weightedM));
count <- count +1;
}
}

# left-bottom
MatrixData2D_blk <- MatrixData2D[(RowLength-WinRowLength+1):RowLength,1:WinColLength];
count <- 1;
for(q in seq(WinColLength)){
for(p in seq(from=(RowLength-WinRowLength+1), to=RowLength, by=1)){
map <- matrix(c[count,],WinRowLength,WinColLength);
weightedM <- map*MatrixData2D_blk;
vOUT[p,q] <- sum(sum(weightedM));
count <- count +1;
}
}

# right-top
MatrixData2D_blk <- MatrixData2D[1:WinRowLength,(ColLength-WinColLength+1):ColLength];
count <- 1;
for(q in seq(from=(ColLength-WinColLength+1), to=ColLength,by=1)){
for(p in seq(WinRowLength)){
map <- matrix(c[count,],WinRowLength,WinColLength);
weightedM <- map*MatrixData2D_blk;
vOUT[p,q] <- sum(sum(weightedM));
count <- count +1;
}
}

# right-bottom
MatrixData2D_blk <- MatrixData2D[(RowLength-WinRowLength+1):RowLength,(ColLength-WinColLength+1):ColLength];
count <- 1;
for(q in seq(from=(ColLength-WinColLength+1),to=ColLength,by=1)){
for(p in seq(from=(RowLength-WinRowLength+1),to=RowLength,by=1)){
map <- matrix(c[count,],WinRowLength,WinColLength);
weightedM <- map*MatrixData2D_blk;
vOUT[p,q] <- sum(sum(weightedM));
count <- count +1;
}
}

## cells at the edges
# left row
for(pp in seq(from=2,to=(RowLength-2*WinRowLength+1), by=1)){
qq <- 1;
MatrixData2D_blk <- MatrixData2D[pp:(pp+WinRowLength-1),qq:(qq+WinColLength-1)];

for(jj in seq(hWinColLength)){
map <- matrix(c[WinRowLength*jj,],WinRowLength,WinColLength);
weightedM <- map*MatrixData2D_blk;
vOUT[pp+WinRowLength-1,jj] <- sum(sum(weightedM));
}
}

# right row
for(pp in seq(from=2,to=(RowLength-2*WinRowLength+1),by=1)){
qq <- ColLength-WinColLength+1;
MatrixData2D_blk <- MatrixData2D[pp:(pp+WinRowLength-1),qq:ColLength];

for(jj in seq(hWinColLength)){
map <- matrix(c[(WinRowLength*(hWinColLength+1+jj)),],WinRowLength,WinColLength);
weightedM <- map*MatrixData2D_blk;
vOUT[pp+WinRowLength-1,ColLength-hWinColLength+jj] <- sum(sum(weightedM));
}
}

# top row
pp = 1;
for(qq in seq(from=2,to=(ColLength-2*WinColLength+1),by=1)){
MatrixData2D_blk <- MatrixData2D[pp:(pp+WinRowLength-1),qq:(qq+WinColLength-1)];
 
for(jj in seq(hWinRowLength)){
map <- matrix(c[WinRowLength*(WinColLength-1)+jj,],WinRowLength,WinColLength);
weightedM <- map*MatrixData2D_blk;
vOUT[jj,(qq+WinColLength-1)] <- sum(sum(weightedM));
}
}

# bottom row
pp = RowLength-WinRowLength+1;
for(qq in seq(from=2,to=(ColLength-2*WinColLength+1),by=1)){
MatrixData2D_blk <- MatrixData2D[pp:(pp+WinRowLength-1),qq:(qq+WinColLength-1)];
    
jjc = hWinRowLength -1;
for(jj in seq(hWinRowLength)){
map <- matrix(c[WinRowLength*WinColLength-jjc,],WinRowLength,WinColLength);
weightedM <- map*MatrixData2D_blk;
vOUT[(pp+WinRowLength-1-jjc),qq+WinColLength-1] <- sum(sum(weightedM));
jjc <- jjc -1;
}
}

# center block
center <- WinRowLength*(WinColLength-1)/2+(WinRowLength-1)/2+1;
mapCenter <- matrix(c[center,],WinRowLength,WinColLength);
for(pp in seq(RowLength-WinRowLength+1)){
for(qq in seq(ColLength-WinColLength+1)){
MatrixData2D_blk <- MatrixData2D[pp:(pp+WinRowLength-1),qq:(qq+WinColLength-1)];

weightedM <- mapCenter*MatrixData2D_blk;
vOUT[pp+hWinRowLength,qq+hWinColLength] <- sum(sum(weightedM));
}
}

return(vOUT)
}