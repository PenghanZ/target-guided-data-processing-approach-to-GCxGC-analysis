
#FolderRoute <- "F:/TempMatching"
#FolderRoute <- "D:/TempMatching"
#Function1 <- paste0(FolderRoute,"/01Functions/AllBaselineCorrect.r")
#source(Function1)
#modulation_time <- 7
#AllBaselineCorrect(FolderRoute, modulation_time)


AllFillMissingData <- function(FolderRoute, modulation_time){

############################ processing parameters ############################

Function1 <- paste0(FolderRoute,"/01Functions/FillMissingData.r")
source(Function1)
RawDataFolder <- paste0(FolderRoute,"/02RawGCxGCMSData")


########################### File Listing ##########################
FileList <- list.files(RawDataFolder)

########################### Baseline Correcting  ##########################
for(i in 1:length(FileList))
{
CurrentFile <- paste0(RawDataFolder,"/",FileList[i])
DeNoisedData <- FillMissingData(CurrentFile, modulation_time)

########################### Peak Exporting ##########################

SavingRoute <- paste0(FolderRoute,"/03FilledData")
FileName <- gsub(".cdf", "", FileList[i])
WriteFile <- paste0(SavingRoute,"/",FileName,".RData")
save(DeNoisedData, file = WriteFile)


}
}