setwd("./data/processedData/")
library(ggplot2)
library(ggpubr)
library("RColorBrewer")

a=read.csv("UCECnESCA.ob.am.KEGG.txt",sep = '\t')
#a$P.value=-log2(a$Pvalue)
library("RColorBrewer")
#######dot plot 
library("RColorBrewer")
#a$Term=gsub("\\ \\(GO:[0-9]+\\)","",a$Term)
a$GeneRatio <-as.numeric(gsub("\\/.*", "", a$Overlap))/as.numeric(gsub(".*/", "", a$Overlap))
a$GeneN <- as.numeric(gsub("\\/.*", "", a$Overlap))

###plot
p1 <- ggplot(a, aes(x=Cancer,y=Term))+
  geom_point(data = a, aes(x =Cancer ,y=Term,size =GeneRatio, color=Pvalue)) +
  scale_colour_gradient(low = "red", high = "blue",
                        # labels=c("<0.01", "<0.05", "<0.1"), 
                        breaks=c(0.01,0.02, 0.03, 0.04)) +
  labs(size="Rich ratio", color="p value") +
  xlab("") + labs(title = "Obesity biased amplifications", y="Enriched Pathways")+
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",size=0.5),
        # axis.line = element_line(colour = "black",size=0.5),
        axis.title=element_text(size=16,color="black"),
        axis.text.x=element_text(size=15,color="black"),
        axis.text.y=element_text(size=15,color="black"),
        plot.title = element_text(size=16,hjust = 0.5)) 


####UCEC GOMF
UCEC=read.csv("UCEC.all.GOMF.txt",sep = '\t', header = F)
colnames(UCEC) <- c("Term", "Overlap","P.value","Cancer")
UCEC$Term=gsub("\\ \\(GO:[0-9]+\\)","",UCEC$Term)
UCEC$P.value=-log2(UCEC$P.value)

pathwayList <- c("ribosome binding", "melanocortin receptor binding","NAADP-sensitive calcium-release channel activity",
                 "carnitine O-acyltransferase activity", "receptor ligand activity", "5'-3' DNA helicase activity", "acid-amino acid ligase activity",
                 "threonine-type peptidase activity","mannosyl-oligosaccharide 1,2-alpha-mannosidase activity", "mannosyl-oligosaccharide mannosidase activity",
                 "GPI anchor binding", "sulfurtransferase activity", "phosphatidylethanolamine binding", "ionotropic glutamate receptor binding",
                 "voltage-gated chloride channel activity", "retinoid X receptor binding", "ribosomal large subunit binding", "acid-thiol ligase activity (",
                 "CoA-ligase activity", "hydrolase activity, acting on carbon-nitrogen (but not peptide) bonds, in cyclic amidines", 
                 "testosterone dehydrogenase [NAD(P)] activity","serine-type endopeptidase activity", "tRNA-specific adenosine deaminase activity", "estradiol 17-beta-dehydrogenase activity", 
                 "kinesin binding", "nucleoside diphosphate kinase activity", "protein heterodimerization activity",
                 "ribosomal small subunit binding", "RNA binding", "intramolecular oxidoreductase activity, transposing C=C bonds",
                 "oxidoreductase activity, acting on the CH-OH group of donors, NAD or NADP as acceptor", 
                 "steroid dehydrogenase activity, acting on the CH-OH group of donors, NAD or NADP as acceptor", "P-type sodium transporter activity", 
                 "P-type sodium:potassium-exchanging transporter activity", "potassium ion binding", "alkali metal ion binding", 
                 "serine-type exopeptidase activity", "protein kinase A catalytic subunit binding", "phosphatidylcholine transporter activity","TBP-class protein binding", 
                 "inward rectifier potassium channel activity", "general transcription initiation factor binding", 
                 "RNA 2'-O-methyltransferase activity", "voltage-gated potassium channel activity", "U2 snRNA binding",
                 "calcium channel regulator activity", "protein serine/threonine kinase activator activity", 
                 "voltage-gated calcium channel activity","vitamin D receptor binding","P-type potassium transmembrane transporter activity",
                 "nucleobase-containing compound kinase activity","N-acetylglucosamine 6-O-sulfotransferase activity",
                 "androgen receptor binding","mitogen-activated protein kinase binding","phosphatidylinositol-3,5-bisphosphate 3-phosphatase activity",
                 "G protein-coupled receptor binding","caspase binding")

UCEC <- UCEC[!UCEC$Term %in% pathwayList, ]

#######dot plot

UCEC$GeneRatio <-as.numeric(gsub("\\/.*", "", UCEC$Overlap))/as.numeric(gsub(".*/", "", UCEC$Overlap))
UCEC$GeneN <- as.numeric(gsub("\\/.*", "", UCEC$Overlap))

p2<-ggplot(UCEC, aes(x=Cancer,y=Term))+
  geom_point(data = UCEC, aes(x =Cancer ,y=Term,size =GeneRatio, color=P.value)) +
  scale_colour_gradient(low = "blue", high = "red", na.value = NA) +
  labs(size="Rich ratio", color="-log2(P-value)") +
  xlab("") + labs(title = "UCEC", y="GO-MF Terms")+
  scale_x_discrete(labels=c("Nor_am","Nor_del", "Ob_am", "Ob_del")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black",size=0.5),
        # axis.line = element_line(colour = "black",size=0.5),
        axis.title=element_text(size=16,color="black"),
        axis.text.x=element_text(size=15,color="black"),
        axis.text.y=element_text(size=15,color="black"),
        plot.title = element_text(size=16,hjust = 0.5))

pdf(file = "./Figures/Fig4df_enrichedPathway_2.pdf",  
    width=16,
    height=6)

ggarrange(p1, p2, ncol = 2, nrow = 1,widths = c(1, 1.3))

dev.off()
