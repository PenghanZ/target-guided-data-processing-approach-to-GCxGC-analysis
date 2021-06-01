#### Function6 <- paste0(FolderRoute,"/01Functions/DataIndexDict.r")
#### source(Function6)
#### FullFileName <- paste0(FolderRoute,"/02RawGCxGCMSData/",FileNameDot)
#### IndexTable <- DataIndexDict(modulation_time,FullFileName)
#### AnnotatedTempIndex <- 1
#### AllAnnotationsName <- paste0(FolderRoute,"/05AnnotationResult/",FileName,".xlsx")
#### AllAnnotations <- read.xlsx(AllAnnotationsName, 1,stringsAsFactors = F)
#### AllAnnotations <- AllAnnotations[-1,]
#### AllAnnotatedUniquePkLabel <- AllAnnotations[,"UniquePkLabel"]
#### PeakLabelTICDataName <- paste0(FolderRoute,"/04LabeledPeakTIC/",FileName,".RData")
#### load(PeakLabelTICDataName) #used by getCroppedPeakRegionMS as PkLabeledTIC
#### Function8 <- paste0(FolderRoute,"/01Functions/getCroppedPeakRegionMS.r")
#### source(Function8)
#### CroppedPeakRegionMS <- getCroppedPeakRegionMS(IndexTable,AnnotatedTempIndex)

getCroppedPeakRegionMS <- function(IndexTable,AnnotatedTempIndex){

Function7 <- paste0(FolderRoute,"/01Functions/getCroppedPeakRegionIndex.r")
source(Function7)

CroppedPeakRegionIndex <- getCroppedPeakRegionIndex(IndexTable,AnnotatedTempIndex)
AnnotatedUniquePkLabel <- unique(AllAnnotatedUniquePkLabel[which(AllAnnotations[,"TemplateIndex"] == AnnotatedTempIndex)])

Function5 <- paste0(FolderRoute,"/01Functions/getCrtMS4LabeledPk.r")
source(Function5)

MS4LabeledPk <- getCrtMS4LabeledPk(CurrentFileName, PkLabeledTIC, AnnotatedUniquePkLabel, DeNoisedData, 1) ###last number is one because there is only 1 PkLabel

### according to each CroppedPeakRegionIndex generate MS 3d array
CroppedPeakRegionMS <- array(,c(dim(CroppedPeakRegionIndex),dim(MS4LabeledPk)[2]))

dimnames(CroppedPeakRegionMS)[[1]] <- rownames(CroppedPeakRegionIndex)
dimnames(CroppedPeakRegionMS)[[2]] <- colnames(CroppedPeakRegionIndex)
dimnames(CroppedPeakRegionMS)[[3]] <- colnames(MS4LabeledPk)

for(FirstDim in dimnames(CroppedPeakRegionMS)[[1]]){
# FirstDim <- "2.34"
for(SecondDim in dimnames(CroppedPeakRegionMS)[[2]]){
# SecondDim <- "586"
if(CroppedPeakRegionIndex[FirstDim,SecondDim] == 0){
CroppedPeakRegionMS[FirstDim,SecondDim,] <- 0
}
else{
CroppedPeakRegionMS[FirstDim,SecondDim,] <- MS4LabeledPk[as.character(CroppedPeakRegionIndex[FirstDim,SecondDim]),]
}

}
}

return(CroppedPeakRegionMS)   ### DataIndexMatrixUpdown = DataIndexMatrix

}
