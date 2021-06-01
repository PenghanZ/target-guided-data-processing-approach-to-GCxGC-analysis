

#### Function11 <- paste0(FolderRoute,"/01Functions/getCrtMS4LabeledPk.r")
#### source(Function11)
#### DeNoisedDataName <- "DeNoisedData1.RData"
#### PkLabelNum <- 1
#### MS4LabeledPk <- getCrtMS4LabeledPk(PkLabeledTIC, UniquePkLabel, DeNoisedDataName, PkLabelNum)


#### Function12 <- paste0(FolderRoute,"/01Functions/getNormFakeTemplateMS.r")
#### source(Function12)
#### NormFakeTemplateMS <- getNormFakeTemplateMS(MS4LabeledPk)

getNormFakeTemplateMS <- function(MS4LabeledPk){

FakeTemplateMS <- matrix(nrow = 1, ncol = dim(MS4LabeledPk)[2]) ####will use the library template for the real sample analysis
colnames(FakeTemplateMS) <- colnames(MS4LabeledPk)

# FakeTemplateMS[1,] <- MS4LabeledPk[10,]   ### take a middle row for the template 
# CutPoint <- quantile(MS4LabeledPk[10,],0.75)

FakeTemplateMS[1,] <- MS4LabeledPk[floor(dim(MS4LabeledPk)[1]/2),]   ### take a middle row for the template 
CutPoint <- quantile(MS4LabeledPk[floor(dim(MS4LabeledPk)[1]/2),],0.75)
FakeTemplateMS <- FakeTemplateMS[,-which(FakeTemplateMS < CutPoint),drop = FALSE]
NormFakeTemplateMS <- floor(FakeTemplateMS/max(FakeTemplateMS)*999)

############################################################################

return(NormFakeTemplateMS)
}
