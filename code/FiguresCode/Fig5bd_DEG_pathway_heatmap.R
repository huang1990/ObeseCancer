setwd("./data/processedData/FiguresData/")
library(pheatmap)

data = read.table("DEGNorOb_allPathwaymerged.txt", sep = '\t', header = T)
head(data)
rownames(data) <- data$Pathway
data$Pathway <- NULL
##color
myColor <- c("wheat4","white","hotpink3")
myBreaks <- c(-1,-0.1,0.2,1)

pdf(file = "./Figures/Fig5d_ob_pathway_heatmap.pdf",
    width=10, height=5)

pheatmap(data, show_rownames=T, show_colnames=T,
         cluster_cols=F,
         cluster_rows=F, color=myColor, breaks=myBreaks,
         angle_col = 0,legend=F)

dev.off()

####Nor VS overweight
data = read.table("allNorOvPathwaymerged.txt", sep = '\t', header = T)
head(data)
rownames(data) <- data$Pathway
data$Pathway <- NULL
##color
myColor <- c("wheat4","white","skyblue3")
myBreaks <- c(-1,-0.1,0.2,1)

pdf(file = "./Figures/Fig5b_ov_pathway_heatmap.pdf",
    width=10, height=2.8)

pheatmap(data, show_rownames=T, show_colnames=T,
         cluster_cols=F,
         cluster_rows=F, color=myColor, breaks=myBreaks,
         angle_col = 0,legend=F) 

dev.off()

