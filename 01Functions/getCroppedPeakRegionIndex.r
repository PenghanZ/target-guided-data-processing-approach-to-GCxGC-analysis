#### Function6 <- paste0(FolderRoute,"/01Functions/DataIndexDict.r")
#### source(Function6)
#### FullFileName <- paste0(FolderRoute,"/02RawGCxGCMSData/",FileNameDot)
#### IndexTable <- DataIndexDict(modulation_time,FullFileName)
#### AnnotatedTempIndex <- 1
#### AllAnnotationsName <- paste0(FolderRoute,"/05AnnotationResult/",FileName,".xlsx")
#### AllAnnotations <- read.xlsx(AllAnnotationsName, 1,stringsAsFactors = F)
#### AllAnnotations <- AllAnnotations[-1,]
#### AllAnnotatedUniquePkLabel <- AllAnnotations[,"UniquePkLabel"]

getCroppedPeakRegionIndex <- function(IndexTable,AnnotatedTempIndex){

#print(AnnotatedTempIndex)
AnnotatedUniquePkLabel <- unique(AllAnnotatedUniquePkLabel[which(AllAnnotations[,"TemplateIndex"] == AnnotatedTempIndex)]) ###put unique function outmost, because some templates may have same peak label
PeakRegionJudge <- PkLabeledTIC == AnnotatedUniquePkLabel
nonPeakRegionJudge <- !PeakRegionJudge
CropRows <- vector(,length = dim(PeakRegionJudge)[1])
for(RowNum in seq(dim(PeakRegionJudge)[1])){
CropRows[RowNum] <- any(PeakRegionJudge[RowNum,] == TRUE)
}

CropColumns <- vector(,length = dim(PeakRegionJudge)[2])
for(ColumnNum in seq(dim(PeakRegionJudge)[2])){
CropColumns[ColumnNum] <- any(PeakRegionJudge[,ColumnNum] == TRUE)
}

CroppednonPeakRegionJudge <- nonPeakRegionJudge[CropRows,CropColumns]
CroppedPeakRegionIndex <- as.matrix(IndexTable[CropRows,CropColumns], drop = FALSE)
colnames(CroppedPeakRegionIndex) <- colnames(IndexTable)[CropColumns]
rownames(CroppedPeakRegionIndex) <- rownames(IndexTable)[CropRows]
CroppedPeakRegionIndex[CroppednonPeakRegionJudge] <- 0

return(CroppedPeakRegionIndex)   ### DataIndexMatrixUpdown = DataIndexMatrix

}
