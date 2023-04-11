setwd("./data/processedData/FiguresData")

library(ggplot2)
library(Cairo)
library(ggrepel)
library(maftools)
library('NMF')
library(dplyr)

d="UCEC"
fat=read.delim(paste(d,".fat.gene", sep=""))
normal=read.delim(paste(d,".nor.sig", sep=""))
a=merge(normal,fat,by=("gene"))
b=a[a$p.x<=0.001 | a$p.y<=0.001,]
colnames(b) <- c("gene", "nor", "fat")
b$nor <- as.numeric(b$nor)
b$fat <- as.numeric(b$fat)
withType<- b %>% 
  mutate(type = case_when(
    nor<0.001 & fat > 0.05 ~ "normal",
    fat<0.001 & nor > 0.05 ~ "fat",
    fat<0.05 & nor < 0.05 ~ "both"
  ))


#write.csv(withType, "ESCA_mutsig.csv", sep = "\t", row.names = F, quote = F)  
##########plot 
withType$nor_log=-log10(withType$nor)
withType$fat_log=-log10(withType$fat)
set.seed(42)
library(ggplot2)
library(ggrepel)

dodge <- position_dodge(1)
withType<- withType %>% 
  mutate(colr = case_when(
    type == "normal" ~ "wheat4",
    type == "fat" ~ "maroon4",
    type == "both" ~ "darkseagreen4"
  ))

withType<- withType %>% 
  mutate(label = case_when(
    type == "normal" ~ "Normal Weight",
    type == "fat" ~ "Overweight or obesity",
    type == "both" ~ "Both"
  ))

withType <- withType[order(withType$label), ]

dodge <- position_dodge(1)

pdf(file = "./Figures/Fig3d_UCEC_sigGene_dotplot.pdf",  
    width=10,
    height=8)

ggplot(withType,aes(nor_log, fat_log,color=withType$colr))+
  scale_color_manual(values = withType$colr,
                     labels= withType$label, 
                     breaks = withType$colr) +
  geom_point(size =2) +
  geom_text_repel(
    aes(nor_log, fat_log,label=gene), 
    color = withType$colr,
    position = dodge,
    size = 7,
    alpha = 0.9,
    segment.size = .25,
    segment.alpha = .8,
    force = 1,
    show.legend = FALSE
  ) +
  coord_cartesian(clip = "off") +
  labs(title=d, x="Enriched in normal weight", 
       y="Enriched in overweight and obesity",
       face="bold", colour="black") +
  theme_classic() +
  theme(axis.title.x = element_text(color="black", size=24),
        axis.title.y = element_text(color="black", size=24),
        axis.text.x = element_text( colour="black", size=23),
        axis.text.y = element_text(colour="black", size=23),
        axis.line = element_line(size = 0.5, colour = "black"),
        legend.title = element_blank(),
        legend.text = element_text(size = 24, color = "black"),
        legend.position = c(0.8, 0.6),
        plot.title = element_text(hjust = 0.45, colour = "black", size = 24)) +
  guides(shape = guide_legend(override.aes = list(size = 3)))

dev.off()
