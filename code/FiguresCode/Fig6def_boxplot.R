setwd("./data/processedData/FiguresData/")
library(weights)
library(ggpubr)
imm <- read.table("UCEC.immune_BMI.label.txt", sep = "\t", header = T)
weight <- read.table("UCEC_Nor_obesity_weight.txt", sep = "\t", header = T)

merged <- merge(weight, imm, by.x="submitterID", by.y="TCGA.Participant.Barcode")

#####plot
p3 <- ggplot(merged, mapping = aes(x=BMItype, y=T.Cells.Regulatory.Tregs, fill=BMItype)) +
  geom_boxplot() +
  scale_fill_manual(values = c("normal" = "wheat4","obesity" = "hotpink3")) +
  labs(title = "UCEC", y="Regulatory T cells") +
  theme_classic() +
  theme(axis.text.x= element_blank(), 
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 16, colour="black"),
        plot.title = element_text(hjust=0.5, size=16,colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16)) +
       # legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.format", 
                     label.x.npc = "middle",                      
                     show.legend = F,
                     size = 5.5,
                     vjust = -0.01)


p2 <- ggplot(merged, mapping = aes(x=BMItype, y=T.Cells.Follicular.Helper, fill=BMItype)) +
  geom_boxplot() +
  scale_fill_manual(values = c("normal" = "wheat4","obesity" = "hotpink3")) +
  labs(title = "UCEC", y="Follicular Helper T cells") +
  theme_classic() +
  theme(axis.text.x= element_blank(), 
        axis.text.y = element_text(size=16, colour="black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 16, colour="black"),
        plot.title = element_text(hjust=0.5, size=16,colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 16)) +
      #  legend.position = "none") +
  stat_compare_means(method = "wilcox.test", label = "p.format", 
                     label.x.npc = "middle",                      
                     show.legend = F,
                     size = 5.5,
                     vjust = -0.01)


nor <- merged[merged$BMItype=="normal", ]
ob <- merged[merged$BMItype=="obesity", ]
wilcox.test(nor$T.Cells.CD4.Memory.Activated, ob$T.Cells.CD4.Memory.Activated)
wtd.t.test(nor$T.Cells.CD4.Memory.Resting, ob$T.Cells.CD4.Memory.Resting, nor$Weight, ob$Weight)

p1 <- ggplot(merged, mapping = aes(x=BMItype, y=T.Cells.CD4.Memory.Resting*Weight, fill=BMItype)) +
  geom_boxplot() +
  scale_fill_manual(values = c("normal" = "wheat4","obesity" = "hotpink3")) +
  labs(title = "UCEC", y="Resting Memory CD4 T cells") +
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


pdf(file = "./Figures/Fig6def_boxplot.pdf",
    width=12,height=3.8)

ggarrange(p3, p2, p1, ncol = 3, nrow = 1)

dev.off()

