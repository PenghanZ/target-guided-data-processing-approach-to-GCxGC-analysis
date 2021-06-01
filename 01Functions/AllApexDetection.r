# FolderRoute <- "D:/TempMatching"
# OperatorColLength <- 3 # odd number
# OperatorRowLength <- 21 ## 0.1 second min peak width, odd number
# OperatorRowLength <- 201 ## 1 second min peak width, odd number
# modulation_time <- 7 #### seconds
# MSScanHz <- 0.005
# AllApexDetection(FolderRoute, modulation_time)
# Function11 <- paste0(FolderRoute,"/01Functions/AllApexDetection.r")
# source(Function11)
# AllApexDetection(FolderRoute, modulation_time, OperatorColLength, OperatorRowLength, ModulationTime, MSScanHz)


AllApexDetection <- function(FolderRoute, modulation_time, OperatorColLength, OperatorRowLength, MSScanHz, SmoothColLength, SmoothRowLength){

library(ncdf4)
library(ggplot2)
library(ChemmineR)
library(xlsx)

#FolderRoute <- "F:/TempMatching"
#FolderRoute <- "D:/TempMatching"
#modulation_time <- 7 #### seconds

########################### load library ###########################
# LibraryName <- "Lib4AnnotationTest.SDF"
# MSLibName <- paste0(FolderRoute, "/00Library/", LibraryName)
# PkTemplate <- read.SDFset(MSLibName)
# CmpdList <- PkTemplate@ID


########################### for each raw data file ###########################
Function4 <- paste0(FolderRoute,"/01Functions/getTemplateMS.r")
source(Function4)

Function5 <- paste0(FolderRoute,"/01Functions/getCrtMS4LabeledPk.r")
source(Function5)


RawDataFolder <- paste0(FolderRoute,"/02RawGCxGCMSData")
FileList <- list.files(RawDataFolder)

for(FileNameDot in FileList)
{
#FileNameDot <- FileList[1]
###load the corrected MZ data and PeakLabelTIC data
CurrentFileName <<- paste0(FolderRoute,"/02RawGCxGCMSData/",FileNameDot) #used by getCroppedPeakRegionMS
FileName <- gsub(".cdf", "", FileNameDot)
CorrectedMZDataName <- paste0(FolderRoute,"/03BaselineCorrectData/",FileName,".RData")
PeakLabelTICDataName <- paste0(FolderRoute,"/04LabeledPeakTIC/",FileName,".RData")
AllAnnotationsName <- paste0(FolderRoute,"/05AnnotationResult/",FileName,".xlsx")

load(CorrectedMZDataName) #used by getCroppedPeakRegionMS as DeNoisedData
assign("DeNoisedData", DeNoisedData, envir = .GlobalEnv)

load(PeakLabelTICDataName) #used by getCroppedPeakRegionMS as PkLabeledTIC
assign("PkLabeledTIC", PkLabeledTIC, envir = .GlobalEnv)

AllAnnotations <<- read.xlsx(AllAnnotationsName, 1,stringsAsFactors = F)
AllAnnotations <<- AllAnnotations[-1,] #used by getCroppedPeakRegionMS

AllAnnotatedTempIndex <- unique(AllAnnotations[,"TemplateIndex"])
AllAnnotatedUniquePkLabel <<- AllAnnotations[,"UniquePkLabel"]  #used by getCroppedPeakRegionMS


########################### for each annotated template perform the apex detection ###########################
Function6 <- paste0(FolderRoute,"/01Functions/DataIndexDict.r")
source(Function6)
FullFileName <- paste0(FolderRoute,"/02RawGCxGCMSData/",FileNameDot)
IndexTable <- DataIndexDict(modulation_time,FullFileName)

for(AnnotatedTempIndex in AllAnnotatedTempIndex){
# AnnotatedTempIndex <- 4
Function8 <- paste0(FolderRoute,"/01Functions/getCroppedPeakRegionMS.r")
source(Function8)
CroppedPeakRegionMS <- getCroppedPeakRegionMS(IndexTable,AnnotatedTempIndex)

### Apex detection result
Function9 <- paste0(FolderRoute,"/01Functions/sG2dSmoothing.r")
source(Function9)
Function10 <- paste0(FolderRoute,"/01Functions/ApexDetectionMz.r")
source(Function10)
Function11 <- paste0(FolderRoute,"/01Functions/MA2dSmoothing.r")
source(Function11)


ApexResultAllMz <- array(,dim(CroppedPeakRegionMS))
dimnames(ApexResultAllMz) <- dimnames(CroppedPeakRegionMS)

############### for each ion channel for the cropped peak reagion, perform S.G. smoothing or not?################
AllIonChannel <- dimnames(CroppedPeakRegionMS)[[3]]
for(EachIonChannel in AllIonChannel){
#EachIonChannel <- AllIonChannel[1]
#print(EachIonChannel)
MatrixData2D <- matrix(CroppedPeakRegionMS[,,EachIonChannel],nrow = dim(CroppedPeakRegionMS)[1], ncol = dim(CroppedPeakRegionMS)[2])
rownames(MatrixData2D) <- dimnames(CroppedPeakRegionMS)[[1]]
colnames(MatrixData2D) <- dimnames(CroppedPeakRegionMS)[[2]]

# Smoothing window

MAPkMz <- MA2dSmoothing(MatrixData2D, SmoothColLength, SmoothRowLength)

#sGPkMz <- sG2dSmoothing(MatrixData2D, SmoothColLength, SmoothRowLength)

############### for each Soomthed ion channel , perform apex detection ################


MzValue <- as.numeric(EachIonChannel)
#ApexResultMz <- ApexDetectionMz(MatrixData2D, OperatorColLength, OperatorRowLength, modulation_time, MSScanHz, MzValue) ### don't use Soomthing
ApexResultMz <- ApexDetectionMz(MAPkMz, OperatorColLength, OperatorRowLength, modulation_time, MSScanHz, MzValue) ### use moving average Soomthing
#ApexResultMz <- ApexDetectionMz(sGPkMz, OperatorColLength, OperatorRowLength, modulation_time, MSScanHz, MzValue) ### use sG Soomthing
ApexResultAllMz[,,EachIonChannel] <- ApexResultMz
}

# 40 32079 586 2.11
# 41 6406 586 2.11

# save the Apex result for each templat matching for each data file
SavingRoute <- paste0(FolderRoute,"/06ApexResultAllMz")
FileName <- gsub(".cdf", "", FileNameDot)
TempIndex <- as.character(AnnotatedTempIndex)
WriteFile <- paste0(SavingRoute,"/",FileName,"TempIndex",TempIndex,".RData")
save(ApexResultAllMz, file = WriteFile)

}

}

}