#### Function11 <- paste0(FolderRoute,"/01Functions/getCrtMS4LabeledPk.r")
#### source(Function11)
#### DeNoisedDataName <- "DeNoisedData1.RData"
#### UniquePkLabelNum <- 1
#### MS4LabeledPk <- getCrtMS4LabeledPk(PkLabeledTIC, UniquePkLabel, DeNoisedDataName, UniquePkLabelNum)

#### Function12 <- paste0(FolderRoute,"/01Functions/getNormFakeTemplateMS.r")
#### source(Function12)
#### NormFakeTemplateMS <- getNormFakeTemplateMS(MS4LabeledPk)
#### NormTemplateMS <- NormFakeTemplateMS

#### ExamMzNum <- 5

#### Function13 <- paste0(FolderRoute,"/01Functions/PurenessMSSeq.r")
#### source(Function13)
#### AnnotationResult <- PurenessMSSeq(TemplateIndex, NormTemplateMS, UniquePkLabelNum, MS4LabeledPk, ExamMzNum)

PurenessMSSeq <- function(TemplateIndex, NormTemplateMS, UniquePkLabelNum, MS4LabeledPk, ExamMzNum){

AnnotationResult <- matrix(0, nrow = 3,ncol = 5)
rownames(AnnotationResult) <- c("MzExistance", "MzSequence", "MassSimilarity")
colnames(AnnotationResult) <- c("MatchRule", "TemplateIndex", "UniquePkLabel", "PixelIndexNum", "MatchResult")
AnnotationResult[,"MatchRule"] <- rownames(AnnotationResult)


### check the MzExistance (if can find a pixel not peak overlapping or minor overlapping, no matter the detecotr saturation)
NormExamTemplateMS <- NormTemplateMS[,which(NormTemplateMS %in% sort(NormTemplateMS, decreasing = TRUE)[1:ExamMzNum]), drop = FALSE]

AllPixelIndex <- rownames(MS4LabeledPk)
for (PixelIndex in AllPixelIndex){

CurrentSampleMS <- MS4LabeledPk[PixelIndex,,drop = FALSE]
ExamSampleMS <- CurrentSampleMS[,which(CurrentSampleMS %in% sort(CurrentSampleMS, decreasing = TRUE)[1:ExamMzNum]), drop = FALSE] ### mass spectrum by several most intensive signals


if (all(colnames(ExamSampleMS) %in% colnames(NormExamTemplateMS))) { 
AnnotationResult["MzExistance", "MatchResult"] <- 1
AnnotationResult["MzExistance", "PixelIndexNum"] <- PixelIndex
AnnotationResult["MzExistance", "UniquePkLabel"] <- UniquePkLabel[UniquePkLabelNum]
AnnotationResult["MzExistance", "TemplateIndex"] <- TemplateIndex
break 
}
}


####### check the MzSequence (if can find a pixel not or minor detecotr saturation, no matter the overlapping)
NormExamTemplateMS <- NormTemplateMS[,which(NormTemplateMS %in% sort(NormTemplateMS, decreasing = TRUE)[1:ExamMzNum]), drop = FALSE]
seqNormExamTemplateMS <- t(apply(NormExamTemplateMS,1,sort,decreasing = TRUE))
seqExamTemplateMz <- colnames(seqNormExamTemplateMS)

for (PixelIndex in AllPixelIndex){

CurrentSampleMS <- MS4LabeledPk[PixelIndex,,drop = FALSE]
ExamSampleMS <- CurrentSampleMS[,which(colnames(CurrentSampleMS) %in% colnames(NormExamTemplateMS)), drop = FALSE] ### mass spectrum by MzValue in the template
seqExamSampleMS <- t(apply(ExamSampleMS,1,sort,decreasing = TRUE))
seqExamSampleMz <- colnames(seqExamSampleMS)


if (all(seqExamSampleMz == seqExamTemplateMz)) { 
AnnotationResult["MzSequence", "MatchResult"] <- 1
AnnotationResult["MzSequence", "PixelIndexNum"] <- PixelIndex
AnnotationResult["MzSequence", "UniquePkLabel"] <- UniquePkLabel[UniquePkLabelNum]
AnnotationResult["MzSequence", "TemplateIndex"] <- TemplateIndex
break 
}
}


####### check the MassSimilarity (at least one pixel give the similarity score above a certain value)
NormExamTemplateMS <- NormTemplateMS[,which(NormTemplateMS %in% sort(NormTemplateMS, decreasing = TRUE)[1:ExamMzNum]), drop = FALSE]
### NormExamTemplateMS[1, c(3,5)] <- 1

W8edNormExamTemplateMS <- NormExamTemplateMS^0.6 * as.numeric(colnames(NormExamTemplateMS))^3


for (PixelIndex in AllPixelIndex){

CurrentSampleMS <- MS4LabeledPk[PixelIndex,,drop = FALSE]
ExamSampleMS <- CurrentSampleMS[,which(colnames(CurrentSampleMS) %in% colnames(NormExamTemplateMS)), drop = FALSE] ### mass spectrum by MzValue in the template

if(max(ExamSampleMS) == 0){
next
} else {
NormExamSampleMS <- floor(ExamSampleMS/max(ExamSampleMS)*999)
}

W8edNormExamSampleMS <- NormExamSampleMS^0.6 * as.numeric(colnames(NormExamSampleMS))^3

DotProduct <- (sum(W8edNormExamTemplateMS * W8edNormExamSampleMS))^2 / (sum(W8edNormExamTemplateMS^2) * sum(W8edNormExamSampleMS^2))


if (DotProduct > 0.995) {
AnnotationResult["MassSimilarity", "MatchResult"] <- 1
AnnotationResult["MassSimilarity", "PixelIndexNum"] <- PixelIndex
AnnotationResult["MassSimilarity", "UniquePkLabel"] <- UniquePkLabel[UniquePkLabelNum]
AnnotationResult["MassSimilarity", "TemplateIndex"] <- TemplateIndex
break 
}
}

############################################################################

return(AnnotationResult)
}
