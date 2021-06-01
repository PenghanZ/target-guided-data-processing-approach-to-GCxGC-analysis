### library(ncdf4)
### library(ggplot2)
### library(ChemmineR)

### FolderRoute <- "F:/TempMatching"
### FolderRoute <- "D:/TempMatching"

### LibName <- "122STDS.SDF"
### TemplateNum <- 1
### Function8 <- paste0(FolderRoute,"/01Functions/getTemplateMS.r")
### source(Function8)

### TemplateMS <- getTemplateMS(FolderRoute, LibName, TemplateNum)



getTemplateMS <- function(FolderRoute, LibName, TemplateNum){

MSLibName <- paste0(FolderRoute,"/00Library/", LibName)

PkTemplate <- read.SDFset(MSLibName)
CmpdMSData0 <- PkTemplate@SDF[[TemplateNum]]@datablock[["MASS SPECTRAL PEAKS"]]
CmpdMSData1 <- unlist(strsplit(PkTemplate@SDF[[TemplateNum]]@datablock[["MASS SPECTRAL PEAKS"]], "__"),use.names=FALSE)
CmpdMSData2 <- unlist(strsplit(CmpdMSData1, " "),use.names=FALSE)
#CmpdMSData3 <- CmpdMSData2[-which(CmpdMSData2 == "")]
CmpdMSData3 <- CmpdMSData2[which(CmpdMSData2 != "")]

MzValueIndex <- seq(1, length(CmpdMSData3)-1, by=2)
MzIntensityIndex <- seq(2, length(CmpdMSData3), by=2)
MzValue <- CmpdMSData3[MzValueIndex]
MzIntensity <- as.numeric(CmpdMSData3[MzIntensityIndex])


TemplateMS <- matrix(ncol = length(CmpdMSData3)/2, nrow = 1)
colnames(TemplateMS) <- MzValue
TemplateMS[1,] <- MzIntensity



return(TemplateMS)

}
