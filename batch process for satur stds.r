############################ template matching annotation batch process ######################
library(ncdf4)
library(ggplot2)
library(ChemmineR)
library(xlsx)

FolderRoute <<- "D:/TempMatching10xWMStd"
modulation_time <<- 7 #### seconds



############################ data denoise on each MZ ######################
#Function1 <- paste0(FolderRoute,"/01Functions/AllBaselineCorrect.r")
#source(Function1)
##modulation_time <- 7
#AllBaselineCorrect(FolderRoute, modulation_time)
###outcome save in folder 03BaselineCorrectData

############################ fill data on each MZ ######################
Function1 <- paste0(FolderRoute,"/01Functions/AllFillMissingData.r")
source(Function1)
#modulation_time <- 7
AllFillMissingData(FolderRoute, modulation_time)
###outcome save in folder 03FilledData





########################### peak regin detection and label on TICMatrix ###########################
Function2 <- paste0(FolderRoute,"/01Functions/AllPeakLabelTIC.r")
source(Function2)
modulation_time <- 7
AllPeakLabelTIC(FolderRoute, modulation_time, 10, 1.7)
###outcome save in folder 04LabeledPeakTIC





########################### load library ###########################
LibraryName <- "SaturationTest.SDF"
MSLibName <- paste0(FolderRoute, "/00Library/", LibraryName)
PkTemplate <- read.SDFset(MSLibName)
CmpdList <- PkTemplate@ID

########################### for each raw data file ###########################
Function3 <- paste0(FolderRoute,"/01Functions/getTemplateRT.r")
source(Function3)

Function4 <- paste0(FolderRoute,"/01Functions/getTemplateMS.r")
source(Function4)

Function5 <- paste0(FolderRoute,"/01Functions/getUniquePkLabel.r")
source(Function5)

Function6 <- paste0(FolderRoute,"/01Functions/getCrtMS4LabeledPk.r")
source(Function6)

Function7 <- paste0(FolderRoute,"/01Functions/PkAnnotation.r")
source(Function7)

Function13 <- paste0(FolderRoute,"/01Functions/getTemplateName.r")
source(Function13)

RawDataFolder <- paste0(FolderRoute,"/02RawGCxGCMSData")
FileList <- list.files(RawDataFolder)


for(FileNameDot in FileList)
{
#AllAnnotations <- matrix(c("MatchRule","TemplateIndex","UniquePkLabel","PixelIndexNum","MatchResult"), nrow = 1,ncol = 5)
AllAnnotations <- matrix(c("MatchRule","TemplateIndex","TemplateName","UniquePkLabel","PixelIndexNum","MatchResult"), nrow = 1,ncol = 6) ###+1 column

#FileNameDot <- FileList[1]
CurrentFileName <- paste0(RawDataFolder,"/",FileNameDot)
print(c("CurrentFileName",CurrentFileName))

###load the corrected MZ data and PeakLabelTIC data
FileName <- gsub(".cdf", "", FileNameDot)
CorrectedMZDataName <- paste0(FolderRoute,"/03BaselineCorrectData/",FileName,".RData")
PeakLabelTICDataName <- paste0(FolderRoute,"/04LabeledPeakTIC/",FileName,".RData")
load(CorrectedMZDataName)
load(PeakLabelTICDataName)

########################### for each compound template in the library ###########################
for(TemplateIndex in 1 : length(CmpdList))
{
#TemplateIndex <- 5
print(c("TemplateIndex",TemplateIndex))

###load the template MS and RT
#Function3 <- paste0(FolderRoute,"/01Functions/getTemplateRT.r")
#source(Function3)
TemplateRT <- getTemplateRT(FolderRoute, LibraryName, TemplateIndex)

#Function4 <- paste0(FolderRoute,"/01Functions/getTemplateMS.r")
#source(Function4)
TemplateMS <- getTemplateMS(FolderRoute, LibraryName, TemplateIndex)

#Function13 <- paste0(FolderRoute,"/01Functions/getTemplateName.r")
#source(Function13)
TemplateName <- getTemplateName(FolderRoute, LibraryName, TemplateIndex)

###get the unique peak label in the RT window
DeltaRT1 <- 15 #in seconds
DeltaRT2 <- 1 #in seconds

#Function5 <- paste0(FolderRoute,"/01Functions/getUniquePkLabel.r")
#source(Function5)
UniquePkLabel <- getUniquePkLabel(CurrentFileName, PkLabeledTIC, TemplateRT, DeltaRT1, DeltaRT2)
#print(c("UniquePkLabel",UniquePkLabel))


########################### for the each UniquePkLabel construct mass spectrum and annotation ###########################
for(UniquePkLabelNum in 1 : length(UniquePkLabel))
{
#print(c("UniquePkLabelNum",UniquePkLabelNum))

MS4LabeledPk <- getCrtMS4LabeledPk(CurrentFileName, PkLabeledTIC, UniquePkLabel, DeNoisedData, UniquePkLabelNum)
###Annotation
ExamMzNum <- 5  ### if some template can't be annotated, change this number and try again. 
AnnotationResult <- PkAnnotation(TemplateIndex, TemplateMS, TemplateName, UniquePkLabelNum, MS4LabeledPk, ExamMzNum)
if(sum(as.numeric(AnnotationResult[,"MatchResult"])) == 3)
{
rownames(AnnotationResult) <- NULL
AllAnnotations <- rbind(AllAnnotations, AnnotationResult)
}
#print(AnnotationResult)


}


}

###write excel 
AllAnnotationsName <- paste0(FolderRoute,"/05AnnotationResult/",FileName,".xlsx")
write.xlsx(AllAnnotations, AllAnnotationsName)

}
### check the annotation result manually, if multiple region is detected for one template, use different threshold(PkLabelTIC.r) for the peak region detection.

############################ apex detection according to the template matching result ######################

OperatorColLength <<- 3 # odd number
OperatorRowLength <<- 21 ## 0.5 second min peak width, odd number
#OperatorRowLength <<- 201 ## 1 second min peak width, odd number

MSScanHz <<- 0.005
SmoothColLength <<- 1 # not necessary to change, 2 dimention has much more data points
SmoothRowLength <<- 101

Function11 <- paste0(FolderRoute,"/01Functions/AllApexDetection.r")
source(Function11)
AllApexDetection(FolderRoute, modulation_time, OperatorColLength, OperatorRowLength, MSScanHz, SmoothColLength, SmoothRowLength)

############################ Mz grouping according to the apex detection result and Detect unique template MZ #########################


LibraryName <- "SaturationTest.SDF"
MzNumThrasholding <- 15 # if an apex region cotains less than 15 mz, delete
Function12 <- paste0(FolderRoute,"/01Functions/AllUniqueMzDetection.r")
source(Function12)
AllUniqueMzDetection(FolderRoute, LibraryName, MzNumThrasholding)


### check the UniqueMz result

DataFolder <- paste0(FolderRoute,"/07UniqueMz")
FileList <- list.files(DataFolder)
#CurrentFileName <- paste0(DataFolder,"/",FileList[4])
CurrentFileName <- paste0(DataFolder,"/",FileList[1])
load(CurrentFileName)



########################### select one unsaturated Mz and perform quantification  ###########################
RawDataFolder <- paste0(FolderRoute,"/02RawGCxGCMSData")
FileList <- list.files(RawDataFolder)

for(FileNameDot in FileList){
# FileNameDot <- FileList[1]
CurrentFileName <<- paste0(FolderRoute,"/02RawGCxGCMSData/",FileNameDot) #used by getCroppedPeakRegionMS
###load the corrected MZ data, PeakLabelTIC data and Annotation result
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

AllAnnotatedTempIndex <<- unique(AllAnnotations[,"TemplateIndex"])
AllAnnotatedTempName <<- AllAnnotations[,"TemplateName"]
AllAnnotatedUniquePkLabel <<- AllAnnotations[,"UniquePkLabel"]


########################### according to the Annotated template perform quantification ###########################
Function6 <- paste0(FolderRoute,"/01Functions/DataIndexDict.r")
source(Function6)
modulation_time <<- 7
FullFileName <- paste0(FolderRoute,"/02RawGCxGCMSData/",FileNameDot)
IndexTable <- DataIndexDict(modulation_time,FullFileName)

AllQuantResult <- matrix(0, nrow = length(AllAnnotatedTempIndex), ncol = 5)
colnames(AllQuantResult) <- c("TemplateIndex","TemplateName","UniquePkLabel","UnstrtUniqueMz","PeakArea")


### for each template load the mass spectrum and unique mass result
Function8 <- paste0(FolderRoute,"/01Functions/getCroppedPeakRegionMS.r")
source(Function8)

for(AnnotatedTempIndex in AllAnnotatedTempIndex){
# AnnotatedTempIndex <- AllAnnotatedTempIndex[11]

#assign("AnnotatedTempIndex", AnnotatedTempIndex, envir = .GlobalEnv)
AnnotatedTempIndexNum <- which(AllAnnotatedTempIndex == AnnotatedTempIndex)
AllQuantResult[AnnotatedTempIndexNum,"TemplateIndex"] <- AnnotatedTempIndex
AllQuantResult[AnnotatedTempIndexNum,"TemplateName"] <- unique(AllAnnotatedTempName[which(AllAnnotations[,"TemplateIndex"] == AnnotatedTempIndex)])
AllQuantResult[AnnotatedTempIndexNum,"UniquePkLabel"] <- unique(AllAnnotatedUniquePkLabel[which(AllAnnotations[,"TemplateIndex"] == AnnotatedTempIndex)])

### load the mass spectrum
#Function8 <- paste0(FolderRoute,"/01Functions/getCroppedPeakRegionMS.r")
#source(Function8)
CroppedPeakRegionMS <- getCroppedPeakRegionMS(IndexTable,AnnotatedTempIndex)

### load the unique mass result
UniqueMzResultName <- paste0(FolderRoute,"//07UniqueMz/",FileName,"TempIndex",AnnotatedTempIndex,".RData")
load(UniqueMzResultName)  ###file name FilteredUniqueTestResultSum
UniqueMz <- colnames(FilteredUniqueTestResultSum)

### for each unique Mz, detect the saturation and use it for the quantification

for(EachUniqueMz in UniqueMz){
# EachUniqueMz <- UniqueMz[1]
CroppedPeakRegionUniqueMz <- CroppedPeakRegionMS[,,EachUniqueMz]
MaxSignalRegionUniqueMz <- max(CroppedPeakRegionUniqueMz)
if(MaxSignalRegionUniqueMz < 1800000){

AllQuantResult[AnnotatedTempIndexNum,"UnstrtUniqueMz"] <- EachUniqueMz

PkAreaUniqueMz <- sum(CroppedPeakRegionUniqueMz)
AllQuantResult[AnnotatedTempIndexNum,"PeakArea"] <- PkAreaUniqueMz

break
}

}

}
###write excel, check the result manually, if there is wrong unique Mz(mostly because of the errors in the template or peaks overlapping for all the Mzs, may be 2 times detection), delete the rows
AllQuantResultName <- paste0(FolderRoute,"/08QuantResult/",FileName,".xlsx")
write.xlsx(AllQuantResult, AllQuantResultName)

}



########################### base on the template and selected MZ perform data cleaning from denoised data and edit raw nCDF ###########################

RawDataFolder <- paste0(FolderRoute,"/02RawGCxGCMSData")
FileList <- list.files(RawDataFolder)

### load library
LibraryName <- "SaturationTest.SDF"
MSLibName <- paste0(FolderRoute, "/00Library/", LibraryName)
PkTemplate <- read.SDFset(MSLibName)

for(FileNameDot in FileList){
# FileNameDot <- FileList[1]

#load raw data 
CurrentFileName <- paste0(RawDataFolder,"/",FileNameDot)
print(c("CurrentFileName",CurrentFileName))

FileName <- gsub(".cdf", "", FileNameDot)
#load denoised data 
CorrectedMZDataName <- paste0(FolderRoute,"/03BaselineCorrectData/",FileName,".RData")
load(CorrectedMZDataName) #used by getCroppedPeakRegionMS as DeNoisedData
assign("DeNoisedData", DeNoisedData, envir = .GlobalEnv)

#load Pk label data
PeakLabelTICDataName <- paste0(FolderRoute,"/04LabeledPeakTIC/",FileName,".RData")
load(PeakLabelTICDataName) #used by getCroppedPeakRegionMS as PkLabeledTIC
assign("PkLabeledTIC", PkLabeledTIC, envir = .GlobalEnv)

#load chromatogram index data
Function6 <- paste0(FolderRoute,"/01Functions/DataIndexDict.r")
source(Function6)
modulation_time <- 7
#FullFileName <- paste0(FolderRoute,"/02RawGCxGCMSData/",FileNameDot)
IndexTable <<- DataIndexDict(modulation_time,CurrentFileName)

#load unique MZ and quantification result 

AllQuantResultName <- paste0(FolderRoute,"/08QuantResult/",FileName,".xlsx")
AllQuantResult <- read.xlsx(AllQuantResultName, 1,stringsAsFactors = F)
assign("AllAnnotations", AllQuantResult, envir = .GlobalEnv) ###assign another name 4 the function getCroppedPeakRegionMS & getCroppedPeakRegionIndex
AllQuantTempIndex <- AllAnnotations[,"TemplateIndex"]
AllAnnotatedUniquePkLabel <<- AllAnnotations[,"UniquePkLabel"] ###assign another name 4 function getCroppedPeakRegionMS & getCroppedPeakRegionIndex
AllQuantUnstrtUniqueMz <- AllAnnotations[,"UnstrtUniqueMz"]


for(EachQuantTempIndex in AllQuantTempIndex){
# EachQuantTempIndex <- AllQuantTempIndex[1]
print(c("EachQuantTempIndex" ,EachQuantTempIndex))
UnstrtUniqueMz <- AllQuantUnstrtUniqueMz[which(AllQuantTempIndex == EachQuantTempIndex)]

#Load the Index table for labeled peak region
Function7 <- paste0(FolderRoute,"/01Functions/getCroppedPeakRegionIndex.r")
source(Function7)
CroppedPeakRegionIndex <- getCroppedPeakRegionIndex(IndexTable,EachQuantTempIndex)

#Load the denoised MS for labeled peak region
Function8 <- paste0(FolderRoute,"/01Functions/getCroppedPeakRegionMS.r")
source(Function8)
CroppedPeakRegionMS <- getCroppedPeakRegionMS(IndexTable,EachQuantTempIndex)

#Load template MS
FilledTemplateMS <- matrix(0,1,dim(CroppedPeakRegionMS)[3])
colnames(FilledTemplateMS) <- dimnames(CroppedPeakRegionMS)[[3]]
Function4 <- paste0(FolderRoute,"/01Functions/getTemplateMS.r")
source(Function4)
TemplateMS <- getTemplateMS(FolderRoute, LibraryName, as.numeric(EachQuantTempIndex))

for(EachCol in colnames(TemplateMS)){
# EachCol <- colnames(TemplateMS)[1]
FilledTemplateMS[1,EachCol] <- TemplateMS[1,EachCol]
}

#for each pixel scaling up the signal and perform the substraction
for(EachRow in rownames(CroppedPeakRegionMS)){
# EachRow <- rownames(CroppedPeakRegionMS)[1]

for(EachCol in colnames(CroppedPeakRegionMS)){
# EachCol <- colnames(CroppedPeakRegionMS)[1]

if(CroppedPeakRegionIndex[EachRow,EachCol] == 0){  ### check the non peak region but to fill the matrix 
next
}else{
PixelMS <- CroppedPeakRegionMS[EachRow,EachCol,]
names(PixelMS) <- dimnames(CroppedPeakRegionMS)[[3]]
ScaledTemplateMS <- FilledTemplateMS / FilledTemplateMS[1,UnstrtUniqueMz] * PixelMS[UnstrtUniqueMz]
ResidualPixelMS <- PixelMS - ScaledTemplateMS
ResidualPixelMS[which(ResidualPixelMS < 0)] <- 0

# write the ResidualPixelMS into DeNoisedData
DeNoisedData[EachRow,EachCol,] <- ResidualPixelMS
}  #for if
}  #for each col
}  #for each row
}  #for EachQuantTempIndex


### edit the cdf file
DeNoisedDataSum <- apply(DeNoisedData,c(1,2),FUN=sum)
print(c("Edit",CurrentFileName))
RawData <- nc_open(CurrentFileName, write=TRUE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE)

for(EachColIndexTable in colnames(IndexTable)){
# EachColIndexTable <- colnames(IndexTable)[1]
# print(c("EachColIndexTable",EachColIndexTable))
for(EachRowIndexTable in rownames(IndexTable)){
# EachRowIndexTable <- rownames(IndexTable)[1]

InputValueTIC <- DeNoisedDataSum[EachRowIndexTable,EachColIndexTable]
InputLocationTIC <- IndexTable[EachRowIndexTable,EachColIndexTable]
ncvar_put(RawData, varid = "total_intensity", vals = InputValueTIC, start = InputLocationTIC, count=1, verbose=FALSE)

InputValueIntensityValues <- DeNoisedData[EachRowIndexTable,EachColIndexTable,]
InputLocationIntensityValues <- (InputLocationTIC - 1) * dim(DeNoisedData)[3] + 1
ncvar_put(RawData, varid = "intensity_values", vals = InputValueIntensityValues, start = InputLocationIntensityValues, count=dim(DeNoisedData)[3], verbose=FALSE)



}
}

MzMin <- ncvar_get(RawData, varid = "mass_range_min")[1]
MzMax <- ncvar_get(RawData, varid = "mass_range_max")[1]
TICdata <- ncvar_get(RawData, varid = "total_intensity")
TotalResolution <- length(TICdata)

InputValueMassValues <- rep(seq(from = MzMin, to = MzMax), times =  TotalResolution)
ncvar_put(RawData, varid = "mass_values", vals = InputValueMassValues, start = 1, count=length(InputValueMassValues), verbose=FALSE)

InputValuePointCount <- rep(MzMax-MzMin+1, TotalResolution)
ncvar_put(RawData, varid = "point_count", vals = InputValuePointCount, start = 1, count=TotalResolution, verbose=FALSE)

InputValueScanIndex <- seq(from = 0, to = (MzMax-MzMin+1)*(TotalResolution-1), by = (MzMax-MzMin+1))
ncvar_put(RawData, varid = "scan_index", vals = InputValueScanIndex, start = 1, count=TotalResolution, verbose=FALSE)

nc_close(RawData)
print("Edit End")

} #for Each raw data file



# before the cleaning 
DeNoisedDataSum <- apply(DeNoisedData,c(1,2),FUN=sum)
AttributeMatrixPlot(DeNoisedDataSum,modulation_time, RawDataFileName)


########################### after the cleaning plot the TIC result ###########################
library(ncdf4)
library(ggplot2)
library(ChemmineR)
library(xlsx)
modulation_time <- 7

FolderRoute <- "D:/TempMatchingSatur"
RawDataFolder <- paste0(FolderRoute,"/02RawGCxGCMSData")
FileList <- list.files(RawDataFolder)
RawDataFileName <- paste0(RawDataFolder,"/",FileList[1]) ### 2000
# RawDataFileName <- paste0(RawDataFolder,"/",FileList[4]) ### 200
# RawDataFileName <- paste0(RawDataFolder,"/",FileList[7]) ### 20
# RawDataFileName <- paste0(RawDataFolder,"/",FileList[10]) ### 2

RawData <- nc_open(RawDataFileName, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE)
TICdata <- ncvar_get(RawData, varid = "total_intensity")

Function1 <- paste0(FolderRoute,"/01Functions/AttributeMatrix.r")
source(Function1)
TICMatrix <- AttributeMatrix(TICdata,modulation_time,RawDataFileName)

Function2 <- paste0(FolderRoute,"/01Functions/AttributeMatrixPlot.r")
source(Function2)
AttributeMatrixPlot(TICMatrix,modulation_time, RawDataFileName)


EditedDataFolder <- paste0(FolderRoute,"/02.1CleanedRawdata")
EditedFileList <- list.files(EditedDataFolder)
EditedDataFileName <- paste0(EditedDataFolder,"/",FileList[1]) ### 2000
# EditedDataFileName <- paste0(EditedDataFolder,"/",FileList[4]) ### 200
# EditedDataFileName <- paste0(EditedDataFolder,"/",FileList[7]) ### 20
# EditedDataFileName <- paste0(EditedDataFolder,"/",FileList[10]) ### 2

EditedData <- nc_open(EditedDataFileName, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE)
EditedTICdata <- ncvar_get(EditedData, varid = "total_intensity")

Function1 <- paste0(FolderRoute,"/01Functions/AttributeMatrix.r")
source(Function1)
EditedTICMatrix <- AttributeMatrix(EditedTICdata,modulation_time,EditedDataFileName)

Function2 <- paste0(FolderRoute,"/01Functions/AttributeMatrixPlot.r")
source(Function2)
AttributeMatrixPlot(EditedTICMatrix,modulation_time, EditedDataFileName)




IntensityValues <- ncvar_get(RawData, varid = "intensity_values")

MassValues <- ncvar_get(RawData, varid = "mass_values")



########################### Test the max signal intensity for the each mz ###########################

FolderRoute <- "D:/TempMatching"
filename <- paste0(FolderRoute,"/02RawGCxGCMSData/Std_mix_con_1_1.cdf")
# filename1 <- paste0(FolderRoute,"/00Backup/02RawGCxGCMSData/All_Std_mix_1_8_1.cdf")
# Function1 <- paste0(FolderRoute,"/01Functions/SingleMzExtract.r")
# source(Function1)
# MzData <- SingleMzExtract(MzValue, filename)

library(ncdf4)
data <- nc_open(filename, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE)
# data <- nc_open(filename1, write=FALSE, readunlim=TRUE, verbose=FALSE, auto_GMT=TRUE, suppress_dimvals=FALSE)
MzMin <- ncvar_get(data, varid = "mass_range_min")[1]
MzMax <- ncvar_get(data, varid = "mass_range_max")[1]
MzRange <- as.character(c(MzMin:MzMax))
MaxIntensityResult <- matrix(0,nrow = 1, ncol = length(MzRange))
rownames(MaxIntensityResult) <- "MaxIntensity"
colnames(MaxIntensityResult) <- MzRange

AllMzIndex <- ncvar_get(data, varid = "mass_values")
AllMzIntensity <- ncvar_get(data, varid = "intensity_values")

for(EachMz in MzRange){
print(EachMz)
# EachMz <- MzRange[1]
MzIndex <- AllMzIndex == as.numeric(EachMz)
MzIntensity <- AllMzIntensity[MzIndex]

MaxIntensityResult[1,EachMz] <- max(MzIntensity)

}


########################### Normalized fake MS template generation and plotting ###########################
Function12 <- paste0(FolderRoute,"/01Functions/getNormFakeTemplateMS.r")
source(Function12)
NormFakeTemplateMS <- getNormFakeTemplateMS(MS4LabeledPk)



########################### pk annotation base on the template ###########################
#NormTemplateMS <- NormFakeTemplateMS
NormTemplateMS <- TemplateMS
ExamMzNum <- 5
Function14 <- paste0(FolderRoute,"/01Functions/PkAnnotation.r")
source(Function14)
AnnotationResult <- PkAnnotation(NormTemplateMS, MS4LabeledPk, ExamMzNum)






