setwd("./data/processedData/FiguresData/")
library(dplyr)
library(ggplot2)

  table=read.table("all_immune_BMI.label.txt",header=T, sep="\t")
  table=arrange(table,gsub("[0-9]+","",table$Type))
  
  #plot ImmueType
  df<-data.frame(table$TCGA.Study, table$Type, table$Immune.Subtype)
  
  df$table.Type<-gsub("[0-9]+","",df$table.Type)
  df<-df[!is.na(df$table.Immune.Subtype),]
  head(df)
  ImmuType <- df %>% group_by(table.TCGA.Study, table.Type, table.Immune.Subtype)%>%
    summarise(count=n())%>% mutate(perc=count/sum(count))
  ImmuType$table.Type = factor(ImmuType$table.Type, ordered = TRUE, levels = c("normal","overweight","obesity"))
  head(ImmuType)
  
  typeof(ImmuType)
  ImmuType$table.Type
  ImmuType <- ImmuType %>% mutate(BMIType = case_when(table.Type=="normal" ~ "Normal-weight",
                                                       table.Type=="overweight" ~ "Overweight",
                                                       table.Type=="obesity" ~ "Obese"))
  head(ImmuType)
  ImmuType <- ImmuType %>% mutate(Im = case_when(table.Immune.Subtype=="C1" ~ "C1 Wound healing",
                                                 table.Immune.Subtype=="C2" ~ "C2 IFN-g dominant",
                                                 table.Immune.Subtype=="C3" ~ "C3 Inflammatory",
                                                 table.Immune.Subtype=="C4" ~ "C4 Lymphocyte depleted",
                                                 table.Immune.Subtype=="C5" ~ "C5 Immunologically quiet",
                                                 table.Immune.Subtype=="C6" ~ "C6 TGF-b dominant"
                                                 ))
  
  ImmuType <- ImmuType %>% 
    group_by(table.TCGA.Study) %>% #arrange(-type) %>% 
    mutate(pos = cumsum(perc) - perc / 2)
  colnames(ImmuType) <- c("cancer", "Group", "type", "count", "num", "BMIType", "Im", "pos")
  head(ImmuType)
  barwidth = 0.8
  
pdf(file = "./Figures/Fig6f_barplot.pdf",
 width=13.5,height=3.5)
  ggplot() + 
    geom_bar(data = ImmuType, 
             mapping = aes(x = factor(BMIType, level=c("Normal-weight","Overweight", "Obese")), y = num, fill = Im), 
             stat="identity", 
             position='stack', 
             width = barwidth)+facet_grid(~ cancer, ) +
    scale_fill_manual(labels = c("C1 Wound healing", "C2 IFN-g dominant","C3 Inflammatory", 
                                 "C4 Lymphocyte depleted", "C5 Immunologically quiet", 
                                 "C6 TGF-b dominant"),
                      values=c("salmon3", "lightpink3", "lightgoldenrod3", "lightblue3", 
                               "turquoise4", "darkseagreen3")) +
    #guides(color = F) +
    labs(x="", y="Relative Percentage", fill="") +
    #theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    theme(axis.text.x = element_text(size=12, angle = 45, color="black", vjust = 1, hjust=1),
      #axis.text.x = element_blank(),
      axis.text.y = element_text(size=14, color="black", vjust = 0.5, hjust=1),
      axis.title.y = element_text(size=14, color="black"),
      # axis.title.x = element_text(size=18, face="bold", color="black"),
      legend.text = element_text(size=13, color="black"),
      strip.text.x = element_text(size = 13, colour = "black"))
 
dev.off()  
  
  
  
  
  
 
