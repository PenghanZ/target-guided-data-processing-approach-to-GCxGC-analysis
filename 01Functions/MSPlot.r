#### Function13 <- paste0(FolderRoute,"/01Functions/MSPlot.r")
#### source(Function13)
#### MSPlot(NormFakeTemplateMS)

MSPlot <- function(NormFakeTemplateMS){

### prepare the data for ggplot 2
MzValue4Plot <- as.numeric(colnames(NormFakeTemplateMS))
MsIntensity4Plot <- NormFakeTemplateMS[1,]
Data4Plot <- data.frame(MzValue4Plot, MsIntensity4Plot)

p1 <- ggplot(Data4Plot, aes(x = MzValue4Plot, y = MsIntensity4Plot)) + geom_bar(stat="identity")
p1 +labs(x = "MzValue", y = "Intensity") + theme(panel.background = element_rect(fill = NA), axis.line = element_line(colour = "grey50", size = 1),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5, size = 10)) + scale_x_continuous (limits = c(35,355), breaks = seq(40,350,15)) + geom_text(stat='identity', aes(label= ..y..), hjust = 0.5, vjust = -0.5, size = 2, check_overlap = TRUE,colour = "red")

}
