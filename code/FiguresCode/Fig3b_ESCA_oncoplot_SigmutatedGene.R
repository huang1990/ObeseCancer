setwd("./data/processedData/FiguresData/")
library(maftools)

maf_raw=read.csv2("ESCA.Norob.SigGene.maf",sep = "\t", header=T)
#maf_filter=maf_raw[,c(1:16,35:38)]
#maf_filter$Tumor_Sample_Barcode = substr(maf_filter$Tumor_Sample_Barcode,1,12)
maf_raw$Tumor_Sample_Barcode =substr(maf_raw$Tumor_Sample_Barcode,1,12)
maf_raw$Hugo_Symbol <- as.character(maf_raw$Hugo_Symbol)
maf_raw <- maf_raw[!maf_raw$Hugo_Symbol %in% c("PKHD1L1","LRP2","DNAH3", "DYNC2H1",
                                               "SPTA1", "HCN1","PDZD2", "BAZ2B",
                                               "CTNND1", "ERBB2"), ]

clinical=read.table("all_clinical.txt",header=T)
colnames(clinical)[1]="Tumor_Sample_Barcode"

esca=read.maf(maf = maf_raw,clinicalData = clinical)
#Shows sample summry.
getSampleSummary(esca)
#Shows gene summary.
getGeneSummary(esca)
#shows clinical data associated with samples
getClinicalData(esca)
#Shows all fields in MAF
getFields(esca)
#Writes maf summary to an output file with basename laml.
#write.mafSummary(maf = esca, basename = 'ucec-ob')

par(mar=rep(2, 4))


plotmafSummary(maf = esca, rmOutlier = TRUE, addStat = 'median', dashboard = TRUE, titvRaw = FALSE)
dev.off()
par(mar=c(3,3,2,2))
#oncoplot for top ten mutated genes.
typecolors <- c("wheat4", "hotpink3")
names(typecolors)=c("normal","obesity")
typecolors=list(typecolors)

#One can use any colors, here in this example color palette from RColorBrewer package is used
vc_cols <- c("#E69F00", "#1F78B4", "#FB9A99", "#F0E442", "#663333","#CC6666", "#9999CC", "#66CC99")
names(vc_cols) = c(
  'Frame_Shift_Del',
  'Missense_Mutation',
  'Nonsense_Mutation',
  'Multi_Hit',
  'Frame_Shift_Ins',
  'In_Frame_Ins',
  'Splice_Site',
  'In_Frame_Del'
)


oncoplot(maf = esca, 
         colors = vc_cols,
         titleFontSize = 1.5,
         top = 20, 
         bgCol = "white",
         draw_titv = F,
         clinicalFeatures = c('type'),
         sortByAnnotation = TRUE, logColBar = TRUE,
         annotationColor = c(typecolors),
         fontSize = 0.6,
         legendFontSize = 1,
         annotationFontSize	= 1)


###right bar plot 
df <- read.table("ESCA.NorObFre.txt", sep = "\t", header = F)
colnames(df) <- c("Gene", "obFre", "norFre")

df_new <- tidyr::gather(df,key='typ',value='Fre',2:3,-1)
df_new <- df_new[df_new$Gene %in% c("NBEA", "MDGA2", "KMT2C", "HUWE1", "FRY", 
                                              "ZNF43","WBSCR17", "CCDC85A", 
                                              "ABCC9","ZNF536", "KCNQ3",
                                              "LAMA1","HYDIN", "CDH11","RYR3",
                                              "TENM4","FAT4","DCDC1","SYNE1","DNAH5"),]

df_new$Gene <- factor(df_new$Gene, levels = c("NBEA", "MDGA2", "KMT2C", "HUWE1", "FRY", 
                                              "ZNF43","WBSCR17", "CCDC85A", 
                                              "ABCC9","ZNF536", "KCNQ3",
                                              "LAMA1","HYDIN", "CDH11","RYR3",
                                              "TENM4","FAT4","DCDC1","SYNE1","DNAH5"))


ggplot(df_new, aes(Fre, Gene, fill = typ)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("wheat4", "hotpink3")) +
  #labs(fill = "BMI type") +
  ylab("") +
  xlab("cacner")+
  theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) +
  theme(axis.text.y = element_blank(),
        plot.title = element_text(size = 15, color = "black", hjust = 0.5),
        axis.text.x = element_text(size=15, color = "black"), 
        axis.title = element_blank(),
        legend.position = "None") +
  ggtitle("Recalibrated frequencies")

