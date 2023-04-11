setwd("./data/processedData/FiguresData")
library(ggplot2)
library(plyr)
library(reshape)
df <- read.table("SignificantGeneNum.txt", sep = "\t", header = T)
head(df)
df$normalweight.biased <- c(2,0,0,0,2,30,0,1,9,0,0,10,0,1)
df <- ddply(df, .(Tumor), transform, pos = cumsum(overweight.biased+obesity.biased+normalweight.biased)-0.5*(overweight.biased+obesity.biased+normalweight.biased))
df_new <- tidyr::gather(df,key='group',value='num',c(2:4,-1))
head(df_new)
###ob
df_new$group <- gsub("\\.","_",df_new$group)
barwidth = 0.8

df_new <- df_new[df_new$Tumor %in% c("BLCA", "DLBC", "ESCA", "READ","UCEC"),]

df_new$group <- factor(df_new$group,levels = c("normalweight_biased","overweight_biased","obesity_biased"))

pdf(file = "./Figures/Fig3a_mutatedGene_num_test.pdf",
    width=2.2,
    height=3.5)

ggplot(df_new, aes(x=Tumor, y=num)) + 
  geom_bar(aes(fill=group),position = "stack",stat="identity") +
  #geom_text(data=subset(df_new, num != 0), aes(label=num, x=pos), size=3) +
  scale_fill_manual(values=c("wheat4","skyblue3","hotpink3")) +
  #guides(color = F) +
  labs(y="Number of Mutated Genes", x="", fill="") +  
  theme_classic() +
  theme(title = element_text(size=11,  color="black"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size=11, color="black",angle = 45,colour ="black",hjust = 1),
        #axis.text.x = element_blank(),
        axis.text.y = element_text(size=11, color="black"),
        axis.title.y = element_text(size=11,color="black"),
        # axis.title.x = element_text(size=18, face="bold", color="black"),
        legend.text = element_text(size=8, color="black"),
        legend.position = c(0.8,0.88))
        #legend.position ="none")

dev.off()

