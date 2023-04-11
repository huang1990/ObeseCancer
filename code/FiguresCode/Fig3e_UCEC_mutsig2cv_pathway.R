setwd("./data/processedData/FiguresData/")
a=read.csv("UCEC_fatsig_KEGG_2021_Human_table.txt",sep = '\t')

a=subset(a, a$Adjusted.P.value<0.05)

a <- a[!grepl(c("Viral carcinogenesis|cancer|carcinoma|Melanoma|Glioma|infection|Hepatitis B|Hepatitis C|Chronic myeloid leukemia|myeloid leukemia|herpesvirus infection|
                |Relaxin signaling pathway|Apelin signaling pathway"), a$Term),]

a$log2adjustP=-log2(a$Adjusted.P.value)
a<-a[1:20,]

#######dot plot 
library("RColorBrewer")
a$GeneRatio <-as.numeric(gsub("\\/.*", "", a$Overlap))/as.numeric(gsub(".*/", "", a$Overlap))
a$GeneN <- as.numeric(gsub("\\/.*", "", a$Overlap))

pdf(file = "./Figures/Fig3e_UCEC_sigGene_Pathway.pdf",
    width=10,
    height=6)

ggplot(a, aes(x=GeneRatio,y=Term))+
  geom_point(data = a, aes(x =GeneRatio ,y=Term,size =GeneN, color=Adjusted.P.value)) +
  scale_colour_gradient(low = "blue", high = "red", na.value = NA) +
  labs(size="Gene number", color="Adjusted P-value") +
  xlab("Rich factor") + labs(title = "Enriched Pathways", y="")+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black",size=0.5),
    # axis.line = element_line(colour = "black",size=0.5),
    axis.title=element_text(size=15,color="black"),
    axis.text.x=element_text(size=15,color="black"),
    axis.text.y=element_text(size=15,color="black"),
    plot.title = element_text(size=16, hjust = 0.5),
    legend.text = element_text(size=14,color="black"),
    legend.title = element_text(size=14,color="black")) 
dev.off()


