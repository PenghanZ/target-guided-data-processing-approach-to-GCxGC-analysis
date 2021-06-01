### FolderRoute <- "D:/TempMatching"
### LibraryName <- "Lib4AnnotationTest.SDF"
### MzNumThrasholding <- 15 # if an apex region cotains less than 15 mz, delete
### Function12 <- paste0(FolderRoute,"/01Functions/AllUniqueMzDetection.r")
### source(Function12)
### AllUniqueMzDetection(FolderRoute, LibraryName, MzNumThrasholding)

AllUniqueMzDetection <- function(FolderRoute, LibraryName, MzNumThrasholding){


############################ Mz grouping according to the apex detection result and Detect unique template MZ #########################
library(ncdf4)
library(ggplot2)
library(ChemmineR)
library(xlsx)


###for each raw data file and template index
# FolderRoute <- "D:/TempMatching"
RawDataFolder <- paste0(FolderRoute,"/02RawGCxGCMSData")
FileList <- list.files(RawDataFolder)


Function12 <- paste0(FolderRoute,"/01Functions/CountNonZeros.r")   ### used for calculate ApexSum
source(Function12)

for(FileNameDot in FileList){
# FileNameDot <- FileList[1]
FileName <- gsub(".cdf", "", FileNameDot)
AllAnnotationsName <- paste0(FolderRoute,"/05AnnotationResult/",FileName,".xlsx")
AllAnnotations <- read.xlsx(AllAnnotationsName, 1,stringsAsFactors = F)
AllAnnotations <- AllAnnotations[-1,]
AllAnnotatedTempIndex <- unique(AllAnnotations[,"TemplateIndex"])

for(AnnotatedTempIndex in AllAnnotatedTempIndex){
# AnnotatedTempIndex <- 3
# AnnotatedTempIndex <- 1
print(AnnotatedTempIndex)

### calculate the Apex MZ sum for each pixel
ApexResultAllMzFileRoute <- paste0(FolderRoute,"/06ApexResultAllMz")
TempIndex <- as.character(AnnotatedTempIndex)
ApexResultAllMzFileName <- paste0(ApexResultAllMzFileRoute,"/",FileName,"TempIndex",TempIndex,".RData")
load(ApexResultAllMzFileName)


ApexSum <- apply(ApexResultAllMz,c(1,2),FUN=CountNonZeros)
BinaryApexSum <- ApexSum
BinaryApexSum[which(BinaryApexSum > 0)] <- 1

Function13 <- paste0(FolderRoute,"/01Functions/BWConnLabel.r")
source(Function13)
PkLabeledApexSum <- BWConnLabel(BinaryApexSum)
rownames(PkLabeledApexSum) <- rownames(BinaryApexSum)
colnames(PkLabeledApexSum) <- colnames(BinaryApexSum)


### filter apex which MZ grouping number less than 15 
FilterredApexSum <- ApexSum
for(ApexRegionIndex in seq(max(PkLabeledApexSum))){  ### filter all the region

ApexRegionTotalMzBum <- sum(ApexSum[which(PkLabeledApexSum == ApexRegionIndex)])

if(ApexRegionTotalMzBum < MzNumThrasholding){
FilterredApexSum[which(PkLabeledApexSum == ApexRegionIndex)] <- 0
}

}

FilterredPkLabeledApexSum <- PkLabeledApexSum
FilterredPkLabeledApexSum[which(FilterredApexSum == 0)] <- 0
SubPkApexAll <- unique(FilterredPkLabeledApexSum[which(FilterredPkLabeledApexSum > 0)])

### get grouped MZ for each Subpeak
SubPkMzGrouping <- list()

for(EachSubPkApexsIndex in SubPkApexAll){
# EachSubPkApexsIndex <- 12


SubPkApexPixelIndexs <- which(FilterredPkLabeledApexSum == EachSubPkApexsIndex, arr.ind = TRUE)
SubPkApexPixelIndexsLength <- length(SubPkApexPixelIndexs)/2
SubPkApexPixelMzMatrix <- matrix(,nrow = SubPkApexPixelIndexsLength,ncol = dim(ApexResultAllMz)[3])


for(EachSubPkApexPixel in seq(SubPkApexPixelIndexsLength)){
# EachSubPkApexPixel <- 1
PixelRowCol <- SubPkApexPixelIndexs[EachSubPkApexPixel,]
PixelMzs <- ApexResultAllMz[PixelRowCol[1],PixelRowCol[2],]
SubPkApexPixelMzMatrix[EachSubPkApexPixel,] <- PixelMzs

}

SubPkIndex <- which(SubPkApexAll == EachSubPkApexsIndex)
SubPkMzGrouping [[SubPkIndex]] <- unique(as.vector(SubPkApexPixelMzMatrix))
SubPkName <- paste0(FileName,"TempIndex",TempIndex,"SubPk",EachSubPkApexsIndex)
names(SubPkMzGrouping)[SubPkIndex] <- SubPkName

}

### according to the Template and Groupped Mz of apexs detect the unique MZ
# load library

MSLibName <- paste0(FolderRoute, "/00Library/", LibraryName)
PkTemplate <- read.SDFset(MSLibName)

Function4 <- paste0(FolderRoute,"/01Functions/getTemplateMS.r")
source(Function4)
TemplateMS <- getTemplateMS(FolderRoute, LibraryName, as.numeric(AnnotatedTempIndex))

seqTemplateMS <- t(apply(TemplateMS,1,sort,decreasing = TRUE))
seqTemplateMz <- colnames(seqTemplateMS)

# testing 
UniqueTestResult <- matrix(0,nrow = length(SubPkApexAll), ncol = length(seqTemplateMz))
rownames(UniqueTestResult) <- names(SubPkMzGrouping)
colnames(UniqueTestResult) <- seqTemplateMz

for(eachSubPkMzGrouping in names(SubPkMzGrouping)){

### load the SubPkMzGrouping data
# eachSubPkMzGrouping <- names(SubPkMzGrouping)[1]
testingSubPkMz <- SubPkMzGrouping[[eachSubPkMzGrouping]]

for(eachTemplateMZ in seqTemplateMz){

# eachTemplateMZ <- seqTemplateMz[1]
if(as.numeric(eachTemplateMZ) %in% testingSubPkMz){
UniqueTestResult[eachSubPkMzGrouping,eachTemplateMZ] <- 1
}

}

}

UniqueTestResultSum <- apply(UniqueTestResult,2,FUN=sum)
FilteredUniqueTestResultSum <- UniqueTestResult[,which(UniqueTestResultSum == 1),drop=FALSE]


SavingRoute <- paste0(FolderRoute,"/07UniqueMz")
FileName <- gsub(".cdf", "", FileNameDot)
TempIndex <- as.character(AnnotatedTempIndex)
WriteFile <- paste0(SavingRoute,"/",FileName,"TempIndex",TempIndex,".RData")
save(FilteredUniqueTestResultSum, file = WriteFile)

}

}

}