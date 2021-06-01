### FolderRoute <- "F:/TempMatching"
### FolderRoute <- "D:/TempMatching"
### LibName <- "122STDS.SDF"
### TemplateNum <- 1
### Function8 <- paste0(FolderRoute,"/01Functions/getTemplateName.r")
### source(Function8)
### TemplateName <- getTemplateName(FolderRoute, LibName, TemplateNum)



getTemplateName <- function(FolderRoute, LibName, TemplateNum){


MSLibName <- paste0(FolderRoute,"/00Library/", LibName)
PkTemplate <- read.SDFset(MSLibName)
TemplateName <- PkTemplate@SDF[[TemplateNum]]@datablock[["NAME"]]


return(TemplateName)

}
