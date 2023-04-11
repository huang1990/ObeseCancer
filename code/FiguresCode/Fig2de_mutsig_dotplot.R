setwd("./data/processedData/FiguresData/")
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
###normal VS obesity
df <- read.table("NorVSob_MutSig_all.txt", sep = "\t", header = F)
colnames(df) <- c("Immucell", "pvale" ,"FDR", "OBmean", "Normean","cancer")
head(df)
df$type <- ifelse(df$OBmean<df$Normean,"Normal-weight biased","Obesity biased")
df['sig'] <- "FDR>0.2"
df[is.na(df)] <- 1
df[df$FDR<=0.2,]['sig'] <- "FDR<0.2"
df$Immucell <- gsub("\\.", "", df$Immucell)
df <- df[df$cancer%in% c("UCEC", "READ", "LIHC", "ESCA", "DLBC", "CHOL"),]

####
pdf(file = "./Figures/Fig2e_mutsig_dotplot.pdf",  
    width=7.2,
    height=4) # The height of the plot in inches



p1 <- ggplot(df,aes(Immucell, cancer, group = cancer, color=type))+theme_classic() +
  theme(axis.text.x = element_text(size=14, angle = 45, color="black", vjust = 1, hjust=1),
        axis.text.y = element_text(size=14, color="black", vjust = 0.5, hjust=1),
        axis.title.y = element_text(size=14, color="black"),
        axis.title.x = element_text(size=14, color="black"),
        legend.text = element_text(size=12,color = "black"),
        legend.title = element_text(size=13, color = "black"))+
  geom_point(aes(size=-log10(pvale),shape=factor(sig)),range=c(0,1))+
  scale_size("Pvalue", breaks = c(0.99,0.6,0.3,0.01), labels = c(0.01,0.05,0.5,0.99))+
  scale_alpha_manual(values=c("FDR>0.2"=0, "FDR<0.2"=1))+
  scale_shape_manual(values=c(16,1)) +
  scale_color_manual(values=c("wheat4", "hotpink3")) +
  scale_x_discrete(limits = c("Signature2","Signature3","Signature4","Signature6",
                              "Signature9","Signature10","Signature11",
                              "Signature13","Signature22")) +
  #guides(color = F) +
  labs(x="", y="", shape ="", color="") + 
  guides(color = guide_legend(order = 1),
           shape = guide_legend(order = 2),
           size = guide_legend(order=3)) +
  theme(legend.margin = margin(1, 1, 1, 1))


dev.off()



######NOr VS overweight
df <- read.table("NorVSov_MutSig_all.txt", sep = "\t", header = F)
colnames(df) <- c("Immucell", "pvale" ,"FDR", "OBmean", "Normean","cancer")

head(df)
df$type <- ifelse(df$OBmean<df$Normean,"Normal-weight biased","Overweight biased")
df['sig'] <- "FDR>0.2"
df[is.na(df)] <- 1
df[df$FDR<=0.2,]['sig'] <- "FDR<0.2"
df$Immucell <- gsub("\\.", "", df$Immucell)
df <- df[!df$Immucell=="Signature5", ]
df <- df[df$cancer%in% c("UCEC", "READ", "LIHC", "CHOL"),]


###
pdf(file = "/Users/emmahuang/Downloads/Chong's_lab/obesity/obesity/organize_version/clinical/confoundingFactor_balance/NewPSW/Updated_Figure/Final_Figure/Figures/Fig2d_mutsig_dotplot.pdf",   # The directory you want to save the file in
    width=5.8,
    height=4) # The height of the plot in inches
p2 <- ggplot(df,aes(Immucell, cancer, group = cancer, color=type))+theme_classic() +
  theme(axis.text.x = element_text(size=14, angle = 45, color="black", vjust = 1, hjust=1),
        axis.text.y = element_text(size=14, color="black", vjust = 0.5, hjust=1),
        axis.title.y = element_text(size=14, color="black"),
        axis.title.x = element_text(size=14, color="black"),
        legend.text = element_text(size=12,color = "black"),
        legend.title = element_text(size=13, color = "black"))+
  geom_point(aes(size=-log10(pvale),shape=factor(sig)),range=c(0,1))+
  scale_size("Pvalue", breaks = c(0.99,0.6,0.3,0.01), labels = c(0.01,0.05,0.5,0.99))+
  scale_alpha_manual(values=c("FDR>0.2"=0, "FDR<0.2"=1))+
  scale_shape_manual(values=c(16,1)) +
  scale_color_manual(values=c("wheat4", "skyblue3")) +
  scale_x_discrete(limits = c("Signature1","Signature3","Signature4",
                              "Signature6","Signature20","Signature24")) +
  #guides(color = F) +
  labs(x="", y="", shape ="", color="") + 
  guides(color = guide_legend(order = 1),
           shape = guide_legend(order = 2),
           size = guide_legend(order=3)) +
  theme(legend.margin = margin(1, 1, 1, 1))

dev.off()
ggarrange(p2, p1, ncol = 2, nrow = 1)

ggsave("/Users/emmahuang/Downloads/Chong's_lab/obesity/obesity/organize_version/clinical/confoundingFactor_balance/NewPSW/Updated_Figure/Final_Figure/Figures/Fig2de_mutsig_dotplot.tiff", 
       units="in", width=12,
       height=4, dpi=400, compression = 'lzw')
