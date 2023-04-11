setwd("./data/processedData/FiguresData/")
library(ggpubr)

df <- read.table("UCEC_Norvsobesity_summary_MutatedGene.txt", sep = "\t", header = T)
df <- df[df$summary.fdr.sig<=0.2,]
df <- df[,c(1,6,7)]
colnames(df) <- c("Gene", "ob", "nor")
df_new <- tidyr::gather(df,key='typ',value='Fre',2:3,-1)
df_new$Fre <- as.numeric(df_new$Fre)

pdf(file = "./Figures/Fig3c_UCEC_mutFre.pdf",   
    width=6.5,
    height=3.5)

ggplot(df_new, aes(x=reorder(Gene, -Fre), y=Fre, fill = typ)) +
  geom_bar(position="stack",stat = "identity") +
  scale_fill_manual(values=c("wheat4", "hotpink3"), labels=c("Normal-weight biased", "Obesity biased")) +   #red3
  labs(fill = "", title = "UCEC") +
  ylab("Reculibrated Frequencies") +
  xlab("")+
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
  theme(axis.text.y = element_text(size=13, color = "black"),
        axis.text.x = element_text(size=12, color = "black",angle = 45, vjust = 1, hjust=1),
        axis.title.x = element_blank(), plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.y = element_text(size=13, color = "black"),
        legend.text = element_text(size=12, color = "black"),
        legend.position = c(0.8,0.85))
#legend.text = element_text(size=15),
#legend.title = element_text(size=16),
#legend.position = c(0.2,0.85))
dev.off()
