### FolderRoute <- "F:/TempMatching"
### FolderRoute <- "D:/TempMatching"
### LibName <- "122STDS.SDF"
### TemplateNum <- 1
### Function8 <- paste0(FolderRoute,"/01Functions/getTemplateRT.r")
### source(Function8)
### TemplateRT <- getTemplateRT(FolderRoute, LibName, TemplateNum)



getTemplateRT <- function(FolderRoute, LibName, TemplateNum){

MSLibName <- paste0(FolderRoute,"/00Library/", LibName)
PkTemplate <- read.SDFset(MSLibName)

CmpdSynonyms <- unlist(strsplit(PkTemplate@SDF[[TemplateNum]]@datablock[["SYNONYMS"]], " "),use.names=FALSE)
RetTime1 <- CmpdSynonyms[which(grepl("##RT1", CmpdSynonyms))]
RetTime1 <- as.numeric(gsub("##RT1=", "", RetTime1))
RetTime2 <- CmpdSynonyms[which(grepl("##RT2", CmpdSynonyms))]
RetTime2 <- as.numeric(gsub("##RT2=", "", RetTime2))

RetTime <- c(RetTime1,RetTime2)
return(RetTime)

}
