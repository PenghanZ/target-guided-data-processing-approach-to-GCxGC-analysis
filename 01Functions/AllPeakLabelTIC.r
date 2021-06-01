
#FolderRoute <- "F:/TempMatching"
#FolderRoute <- "D:/TempMatching"
#Function1 <- paste0(FolderRoute,"/01Functions/AllPeakLabelTIC.r")
#source(Function1)
#modulation_time <- 7
#### XSlice <- 10
#### XSignal <- 1.5
#AllPeakLabelTIC(FolderRoute, modulation_time)


AllPeakLabelTIC <- function(FolderRoute, modulation_time, XSlice, XSignal){

############################ processing parameters ############################
Function1 <- paste0(FolderRoute,"/01Functions/PkLabelTIC2.r")
source(Function1)

RawDataFolder <- paste0(FolderRoute,"/02RawGCxGCMSData")


########################### File Listing ##########################
FileList <- list.files(RawDataFolder)

########################### Peak Labeling ##########################
for(i in 1:length(FileList))
{
CurrentFile <- paste0(RawDataFolder,"/",FileList[i])
PkLabeledTIC <- PkLabelTIC2(CurrentFile,modulation_time, XSlice, XSignal)

########################### Peak Exporting ##########################

SavingRoute <- paste0(FolderRoute,"/04LabeledPeakTIC")
FileName <- gsub(".cdf", "", FileList[i])
WriteFile <- paste0(SavingRoute,"/",FileName,".RData")
save(PkLabeledTIC, file = WriteFile)
}
}