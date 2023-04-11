setwd("./data/processedData/FiguresData/")
library(dplyr)
library(ggplot2)
cytoband <- read.table("ESCA_Norvsobesity_result_CybandCNV.txt", sep = "\t", header = T)
head(cytoband)
cytoband$cy <- cytoband$result.feature
#cytoband$cyto <- ifelse(grepl('^[0-9]*$',cytoband$cy), paste0("X",cytoband$cy),cytoband$cy)

#gsub('21p11.2',pattern = "^(\\d+)[p|q]([0-9|\\.]+)",replacement ="\\1")
#gsub('21p11.2',pattern = "^(\\d+)[p|q]([0-9|\\.]+)",replacement ="\\2")

cytoband<- cytoband %>%
  mutate(chr = case_when(grepl("p", cy) ~ sub("\\p.*$", "\\1", cy),
                         grepl("q", cy) ~ sub("\\q.*$", "\\1", cy)
  ))
cytoband<- cytoband %>%
  mutate(cyto = case_when(grepl("p", cy) ~ sub(".*[0-9]p", "p\\1", cy),
                          grepl("q", cy) ~ sub(".*[0-9]q", "q\\1", cy)
  ))

#cytoband<- cytoband %>%
#  mutate(cytoType = case_when(result.mean.0.w <= 0 ~ "del",
#                             result.mean.0.w > 0 ~ "am"))
cytoband<- cytoband %>% mutate(cytoType = if_else(result.mean.0.w <= 0, 'del','am'))

cytoband<- cytoband %>%
  mutate(bias = case_when(abs(result.mean.0.w) > abs(result.mean.1.w) ~ "Obesity_biased",
                          abs(result.mean.0.w) < abs(result.mean.1.w) ~ "Normal_biased"))

head(cytoband)

cytoband_new <- cytoband[,c("cy","cyto","chr", "result.fdr", "cytoType", "bias")]
cytoband_new$chr_n <- paste0("chr", cytoband_new$chr)
cytoband_new$chr_cyto <- paste(cytoband_new$chr_n, cytoband_new$cyto, sep = "_")
pos <- read.table("cytoBand.txt", sep = "\t", header = F)
colnames(pos) <- c("chr", "start", "end", "cyto", "type")
pos$chr_cyto <- paste(pos$chr, pos$cyto, sep = "_")
merged <- merge(cytoband_new, pos, by.x = "chr_cyto", by.y = "chr_cyto")

#####manhattan plot
gwasResults <- merged[,c("chr.x", "cy", "result.fdr", "start", "cytoType", "bias")]
gwasResults <- rename(gwasResults, c(CHR=chr.x, SNP=cy,P=result.fdr,BP=start,cytoType=cytoType,bias=bias))
#ggwasResults <- gwasResults[!(gwasResults$CHR=="X"),]
gwasResults$BP[gwasResults$BP == 0] <- 1
gwasResults['CHR'] = as.numeric(gwasResults[,'CHR'])
gwasResults=gwasResults[order(gwasResults['CHR']),]

don <- gwasResults %>% 
    # Compute chromosome size
  group_by(CHR) %>% 
  summarise(chr_len=max(BP)) %>% 
  # Calculate cumulative position of each chromosome
  mutate(tot=cumsum(as.numeric(chr_len))-chr_len) %>%
  dplyr::select(-chr_len) %>%
  
  # Add this info to the initial dataset
  left_join(gwasResults, ., by=c("CHR"="CHR")) %>%
  
  # Add a cumulative position of each SNP
  arrange(CHR, BP) %>%
  mutate( BPcum=BP+tot)
##prepare x axis
head(don)
#don <- don[order(don$CHR, decreasing = T),]
axisdf = don %>% group_by(CHR) %>% summarise(center=( max(BPcum) + min(BPcum) ) / 2 )
axisdf$CHR <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,"X")
don['BPcum'] = as.numeric(don[,'BPcum'])
don['color'] = 'wheat3'
#don = don %>% mutate(color = if_else(bias == "Obesity_biased", 'red3','yellow3')) #for obesity
don = don %>% mutate(color = if_else(bias == "Obesity_biased", 'hotpink3','wheat4')) #hotpink3 ##skyblue3

en <- c(1,3,5,7,9,11,13,15,17,19,21)
ra <- c(2,4,6,8,10,12,14,16,18,20,22)
evenN <- don[don$CHR %in% c(1,3,5,7,9,11,13,15,17,19,21), ]
radix<- don[don$CHR %in% c(2,4,6,8,10,12,14,16,18,20,22), ]

data1 <- data.frame(start = rep(NA,11),
                    end = rep(NA,11))

data2 <- data.frame(start = rep(NA,11),
                    end = rep(NA,11))

for (a in en) {
  minN <- min(evenN[evenN$CHR==a, ]$BPcum)
  data1[a,1] <- minN
  maxN <- max(evenN[evenN$CHR==a, ]$BPcum)
  data1[a,2] <- maxN
}

for (b in ra) {
  minN <- min(radix[radix$CHR==b, ]$BPcum)
  data2[b,1] <- minN
  maxN <- max(radix[radix$CHR==b, ]$BPcum)
  data2[b,2] <- maxN
}
data1<-na.omit(data1)
data2<-na.omit(data2)

don$cytoType
don<- don %>%
  mutate(fdr = case_when(cytoType == "am" ~ don$P,
                         cytoType == "del" ~ 1))

don$fdr[is.na(don$fdr)] = 1

pdf(file = "./Figures/Fig4b_ESCA_am.pdf",   
    width=3,
    height=8)

library(ggplot2)
ggplot() +
  geom_rect(data = data1, mapping=aes(xmin = start, xmax = end, ymin = 0, ymax = 2.2),fill="gray90", color = "gray90") +##CHOL1.25
  #geom_rect(data = data2,mapping=aes(xmin = start, xmax = end, ymin = 0, ymax = 1.25),  color = "yellow") + 
  #geom_point( aes(color=as.factor(CHR)), alpha=0.8, size=1.3) +
  geom_bar(data = don, aes(x=don$BPcum, y=-log10(don$fdr)),stat = "identity",color = don$color) +
  scale_color_manual(values = rep(c("black"), 22 )) +
  
  # custom X axis:
  scale_x_continuous( label = axisdf$CHR, breaks= axisdf$center ) +
  scale_y_continuous(expand = c(0, 0) ) +     # remove space between plot area and x axis
  
  # Custom the theme:
  theme_bw() +
  labs(x="", y="-Log10(FDR)") +
  theme( 
    legend.position="none",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()) +
  geom_hline(yintercept=1, linetype="dashed", color = "green") +
  coord_flip() + 
  labs(title = "ESCA Amplifications", y = "-log10 (FDR)", x = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        title = element_text(size=14, color="black"),
        # axis.line = element_line(colour = "black"),
        axis.title=element_text(size=12, color="black"),
        axis.text=element_text(size=12, color="black"),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "grey",
                                    fill = NA,
                                    size = 1))
dev.off()
##FDR=0.1, -log10(0.2)=0.69897

################UCEC
cytoband <- read.table("UCEC_Norvsobesity_result_CybandCNV.txt", sep = "\t", header = T)
head(cytoband)
cytoband$cy <- cytoband$result.feature
#cytoband$cyto <- ifelse(grepl('^[0-9]*$',cytoband$cy), paste0("X",cytoband$cy),cytoband$cy)

#gsub('21p11.2',pattern = "^(\\d+)[p|q]([0-9|\\.]+)",replacement ="\\1")
#gsub('21p11.2',pattern = "^(\\d+)[p|q]([0-9|\\.]+)",replacement ="\\2")

cytoband<- cytoband %>%
  mutate(chr = case_when(grepl("p", cy) ~ sub("\\p.*$", "\\1", cy),
                         grepl("q", cy) ~ sub("\\q.*$", "\\1", cy)
  ))
cytoband<- cytoband %>%
  mutate(cyto = case_when(grepl("p", cy) ~ sub(".*[0-9]p", "p\\1", cy),
                          grepl("q", cy) ~ sub(".*[0-9]q", "q\\1", cy)
  ))

#cytoband<- cytoband %>%
#  mutate(cytoType = case_when(result.mean.0.w <= 0 ~ "del",
 #                             result.mean.0.w > 0 ~ "am"))
cytoband<- cytoband %>% mutate(cytoType = if_else(result.mean.0.w <= 0, 'del','am'))

cytoband<- cytoband %>%
  mutate(bias = case_when(abs(result.mean.0.w) > abs(result.mean.1.w) ~ "Obesity_biased",
                          abs(result.mean.0.w) < abs(result.mean.1.w) ~ "Normal_biased"))


cytoband_new <- cytoband[,c("cy","cyto","chr", "result.fdr", "cytoType", "bias")]
cytoband_new$chr_n <- paste0("chr", cytoband_new$chr)
cytoband_new$chr_cyto <- paste(cytoband_new$chr_n, cytoband_new$cyto, sep = "_")

pos <- read.table("cytoBand.txt", sep = "\t", header = F)

colnames(pos) <- c("chr", "start", "end", "cyto", "type")
pos$chr_cyto <- paste(pos$chr, pos$cyto, sep = "_")
merged <- merge(cytoband_new, pos, by.x = "chr_cyto", by.y = "chr_cyto")

#####manhattan plot
gwasResults <- merged[,c("chr.x", "cy", "result.fdr", "start", "cytoType", "bias")]
gwasResults <- rename(gwasResults, c(CHR=chr.x, SNP=cy,P=result.fdr,BP=start,cytoType=cytoType,bias=bias))
#ggwasResults <- gwasResults[!(gwasResults$CHR=="X"),]
gwasResults$BP[gwasResults$BP == 0] <- 1
gwasResults['CHR'] = as.numeric(gwasResults[,'CHR'])
gwasResults=gwasResults[order(gwasResults['CHR']),]

don <- gwasResults %>% 
  # Compute chromosome size
  group_by(CHR) %>% 
  summarise(chr_len=max(BP)) %>% 
  # Calculate cumulative position of each chromosome
  mutate(tot=cumsum(as.numeric(chr_len))-chr_len) %>%
  dplyr::select(-chr_len) %>%
  
  # Add this info to the initial dataset
  left_join(gwasResults, ., by=c("CHR"="CHR")) %>%
  
  # Add a cumulative position of each SNP
  arrange(CHR, BP) %>%
  mutate( BPcum=BP+tot)
##prepare x axis
head(don)
#don <- don[order(don$CHR, decreasing = T),]
axisdf = don %>% group_by(CHR) %>% summarise(center=( max(BPcum) + min(BPcum) ) / 2 )
axisdf$CHR <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,"X")
don['BPcum'] = as.numeric(don[,'BPcum']
don['color'] = 'wheat3'
#don = don %>% mutate(color = if_else(bias == "Obesity_biased", 'red3','yellow3')) #for obesity
don = don %>% mutate(color = if_else(bias == "Obesity_biased", 'hotpink3','wheat4')) #hotpink3 ##skyblue3

en <- c(1,3,5,7,9,11,13,15,17,19,21)
ra <- c(2,4,6,8,10,12,14,16,18,20,22)
evenN <- don[don$CHR %in% c(1,3,5,7,9,11,13,15,17,19,21), ]
radix<- don[don$CHR %in% c(2,4,6,8,10,12,14,16,18,20,22), ]

data1 <- data.frame(start = rep(NA,11),
                    end = rep(NA,11))

data2 <- data.frame(start = rep(NA,11),
                    end = rep(NA,11))

for (a in en) {
  minN <- min(evenN[evenN$CHR==a, ]$BPcum)
  data1[a,1] <- minN
  maxN <- max(evenN[evenN$CHR==a, ]$BPcum)
  data1[a,2] <- maxN
}

for (b in ra) {
  minN <- min(radix[radix$CHR==b, ]$BPcum)
  data2[b,1] <- minN
  maxN <- max(radix[radix$CHR==b, ]$BPcum)
  data2[b,2] <- maxN
}
data1<-na.omit(data1)
data2<-na.omit(data2)

don$cytoType
don<- don %>%
  mutate(fdr = case_when(cytoType == "am" ~ don$P,
                         cytoType == "del" ~ 1))

#don<- don %>% mutate(fdr = if_else(cytoType == "am", p,1))

don$fdr[is.na(don$fdr)] = 1
pdf(file = "./Figures/Fig4d_UCEC_am.pdf",   # The directory you want to save the file in
    width=3,
    height=8)

ggplot() +
  geom_rect(data = data1, mapping=aes(xmin = start, xmax = end, ymin = 0, ymax = 2.2),fill="gray90", color = "gray90") +##CHOL1.25
  #geom_rect(data = data2,mapping=aes(xmin = start, xmax = end, ymin = 0, ymax = 1.25),  color = "yellow") + 
  #geom_point( aes(color=as.factor(CHR)), alpha=0.8, size=1.3) +
  geom_bar(data = don, aes(x=don$BPcum, y=-log10(don$fdr)),stat = "identity",color = don$color) +
  scale_color_manual(values = rep(c("black"), 22 )) +
  
  # custom X axis:
  scale_x_continuous( label = axisdf$CHR, breaks= axisdf$center ) +
  scale_y_continuous(expand = c(0, 0) ) +     # remove space between plot area and x axis
  
  # Custom the theme:
  theme_bw() +
  labs(x="", y="-Log10(FDR)") +
  theme( 
    legend.position="none",
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()) +
  geom_hline(yintercept=1, linetype="dashed", color = "green") +
  coord_flip() + 
  labs(title = "UCEC Amplifications", y = "-log10 (FDR)", x = "") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        title = element_text(size=14, color="black"),
        # axis.line = element_line(colour = "black"),
        axis.title=element_text(size=12, color="black"),
        axis.text=element_text(size=12, color="black"),
        plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "grey",
                                    fill = NA,
                                    size = 1))
dev.off()
