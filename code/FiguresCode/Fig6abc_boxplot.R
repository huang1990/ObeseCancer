setwd("./data/processedData/FiguresData/")
library(weights)
imm <- read.table("READ.immune_BMI.label.txt", sep = "\t", header = T)
weight <- read.table("READ_Nor_obesity_weight.txt", sep = "\t", header = T)

merged <- merge(weight, imm, by.x="submitterID", by.y="TCGA.Participant.Barcode")
merged <- merged[!merged$submitterID=="TCGA-AF-2691", ]

#####plot
p3 <- ggplot(merged, mapping = aes(x=BMItype, y=Lymphocytes, fill=BMItype)) +
  geom_boxplot() +
  scale_fill_manual(values = c("normal" = "wheat4","obesity" = "hotpink3")) +
  labs(title = "READ", y="Lymphocytes") +
  theme_classic() +
  theme(axis.text.x= element_blank(), 
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 16, colour="black"),
        plot.title = element_text(hjust=0.5, size=16,colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16)) +
  stat_compare_means(method = "wilcox.test", label = "p.format", 
                     label.x.npc = "middle",                      
                     show.legend = F,
                     size = 5.5,
                     vjust = -0.01)


#####CESC
imm <- read.table("CESC.immune_BMI.label.txt", sep = "\t", header = T)
weight <- read.table("CESC_Nor_obesity_weight.txt", sep = "\t", header = T)
merged <- merge(weight, imm, by.x="submitterID", by.y="TCGA.Participant.Barcode")

#####plot
p2 <- ggplot(merged, mapping = aes(x=BMItype, y=Lymphocytes, fill=BMItype)) +
  geom_boxplot() +
  scale_fill_manual(values = c("normal" = "wheat4","obesity" = "hotpink3")) +
  labs(title = "CESC", y="Lymphocytes") +
  theme_classic() +
  theme(axis.text.x= element_blank(), 
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 16, colour="black"),
        plot.title = element_text(hjust=0.5, size=16,colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16)) +
        #legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.format", 
                     label.x.npc = "middle",                      
                     show.legend = F,
                     size = 5.5,
                     vjust = -0.01)


#####LIHC
imm <- read.table("LIHC.immune_BMI.label.txt", sep = "\t", header = T)
weight <- read.table("LIHC_Nor_obesity_weight.txt", sep = "\t", header = T)

merged <- merge(weight, imm, by.x="submitterID", by.y="TCGA.Participant.Barcode")
merged$Leukocyte.Fraction <- merged$Leukocyte.Fraction*merged$Weight
#####plot
p1 <-ggplot(merged, mapping = aes(x=BMItype, y=Leukocyte.Fraction, fill=BMItype)) +
  geom_boxplot() +
  scale_fill_manual(values = c("normal" = "wheat4","obesity" = "hotpink3")) +
  labs(title = "LIHC", y="Leukocyte Fraction") +
  theme_classic() +
  theme(axis.text.x= element_blank(), 
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 16, colour="black"),
        plot.title = element_text(hjust=0.5, size=16,colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16)) +
        #legend.position = "none") +
  stat_compare_means(method = "wilcox.test", 
                     label = "p.format", 
                     label.x.npc = "middle",  
                     show.legend = F,
                     size = 5.5,
                     vjust = -0.01)

pdf(file = "./Figures/Fig6abc_boxplot.pdf",
    width=11.5,
    height=3.8)
ggarrange(p1, p2, p3, ncol = 3, nrow = 1)

dev.off()

