library(ggplot2)
library(ggpubr)
###READ
#########TMB 
d <- "READ"
mutation <- read.table(paste(d, ".mutation.count.txt", sep=""), sep = "\t", header = T)

weight <- read.table(paste(d, "_all_weight2.txt",sep=""), sep = "\t", header = T) #for UVM

BMI <- read.table(paste("addPurity.",d,"_clinical.txt", sep = ""), sep = "\t", header = T)
BMI <- BMI[,c("bmi","submitter_id")]
all <- merge(mutation, weight, by.x = "sampleID", by.y = "submitterID")
merged <- merge(all, BMI,by.x = "sampleID", by.y = "submitter_id")
#sub <- merged[merged$BMItype %in% c("normal", "overweight"), ]

#merged <- merged[merged$bmi<50,] ##for indels
merged$exon <- merged$exon/30
merged$indel <- merged$DEL+merged$INS

Wlm <- lm(merged$indel ~ merged$bmi, data=merged, weights = merged$Weight) 
summary(Wlm)
#
merged$TMB <- merged$exon*merged$Weight
merged$wSNV <- merged$SNV*merged$Weight
merged$windel <- (merged$DEL+merged$INS)*merged$Weight

median(merged[merged$BMItype=="normal",]$TMB)
median(merged[merged$BMItype %in% c("ex"),]$TMB)

p1 <- ggplot(data = merged, aes(y = TMB, x = bmi)) +
#ggplot(data = merged, aes(y = TMB, x = bmi)) +
  geom_point() +
  
  geom_smooth(method = lm, se = FALSE, 
              aes(weight = Weight, color = "Weighted"),
              size = 1,
              linetype = "dashed") +
  xlim(18,55) + 
  #ylim(0,2.5) +
  ggtitle(d) +
  ylab("Weighted TMB") +
  xlab("BMI") +
  theme_bw() +
  scale_colour_manual(name="", values=c("red")) +
  theme(plot.title = element_text(size = 12, hjust=0.5),
        axis.text = element_text(size=12,color = "black"), 
        axis.title = element_text(size=12),
        #legend.title = element_text(size=12, face = "bold"),
        #legend.text = element_text(size=12),
        legend.position = "None") 


################READ SNV

p2 <- ggplot(data = merged, aes(y = wSNV, x = bmi)) +
  geom_point() +
  
  geom_smooth(method = lm, se = FALSE, 
              aes(weight = Weight, color = "Weighted"),
              size = 1,
              linetype = "dashed") +
  xlim(18,45) + 
  #ylim(0,2.5) +
  ggtitle(d) +
  ylab("Weighted SNVs") +
  xlab("BMI") +
  theme_bw() +
  scale_colour_manual(name="", values=c("red")) +
  theme(plot.title = element_text(size = 12, hjust=0.5),
        axis.text = element_text(size=12, color = "black"), 
        axis.title = element_text(size=12),
        #legend.title = element_text(size=12, face = "bold"),
        #legend.text = element_text(size=12),
        legend.position = "None")

#############indels get all the r and p value from READ_Nor_ex_weight.txt

d <- "READ"
mutation <- read.table(paste(d, ".mutation.count.txt", sep=""), sep = "\t", header = T)

weight <- read.table(paste(d, "_Nor_ex_weight.txt",sep=""), sep = "\t", header = T)
BMI <- read.table(paste("addPurity.",d,"_clinical.txt", sep = ""), sep = "\t", header = T)
BMI <- BMI[,c("bmi","submitter_id")]
all <- merge(mutation, weight, by.x = "sampleID", by.y = "submitterID")
merged <- merge(all, BMI,by.x = "sampleID", by.y = "submitter_id")
#sub <- merged[merged$BMItype %in% c("normal", "overweight"), ]

merged <- merged[merged$bmi<50,] ##for indels
merged$exon <- merged$exon/30
merged$indel <- merged$DEL+merged$INS

Wlm <- lm(merged$indel ~ merged$bmi, data=merged, weights = merged$Weight) 
summary(Wlm)
#
merged$TMB <- merged$exon*merged$Weight
merged$wSNV <- merged$SNV*merged$Weight
merged$windel <- (merged$DEL+merged$INS)*merged$Weight

median(merged[merged$BMItype=="normal",]$TMB)
median(merged[merged$BMItype %in% c("ex"),]$TMB)

#####indel
p3 <- ggplot(data = merged, aes(y = indel, x = bmi)) +
  geom_point() +
  
  geom_smooth(method = lm, se = FALSE, 
              aes(weight = Weight, color = "Weighted"),
              size = 1,
              linetype = "dashed") +
  xlim(18,55) + 
  #ylim(0,2.5) +
  ggtitle(d) +
  ylab("Weighted INDELs") +
  xlab("BMI") +
  theme_bw() +
  scale_colour_manual(name="", values=c("red")) +
  theme(plot.title = element_text(size = 12, hjust=0.5),
        axis.text = element_text(size=12,color = "black"), 
        axis.title = element_text(size=12),
        #legend.title = element_text(size=12, face = "bold"),
        #legend.text = element_text(size=12),
        legend.position = "None")

figure <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1, 
                    hjust = 0,
                    vjust = 0.5,
                    font.label = list(size = 14, color ="black"))


ggsave("./Figures/Fig2ac_READ_mutfre_regression.tiff", units="in", width=8,
       height=2.5, dpi=300, compression = 'lzw')

