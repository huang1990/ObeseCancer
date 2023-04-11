setwd("./data/processedData/FiguresData/")

a=read.table("ov.DEG.txt", header = T)
library(ggplot2)
library(cowplot)

#Then I create 9 pie-charts, each one containing one group and showing the quantity of normal-weight vs. overweight.obesity
#(only the name of the group showing; color of each BMI groups is explained seperately in the according text)
p1 <- ggplot(a[a$Tumor=="BLCA",], aes("", y = Number, fill = factor(Type), a$Total[1]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[1])+
  scale_fill_manual(values = c("wheat4", "skyblue3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

p2 <- ggplot(a[a$Tumor=="CESC",], aes("", y = Number, fill = factor(Type), a$Total[2]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[2])+
  scale_fill_manual(values = c("wheat4", "skyblue3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

p3 <- ggplot(a[a$Tumor=="CHOL",], aes("", y = Number, fill = factor(Type), a$Total[3]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[3])+
  scale_fill_manual(values = c("wheat4", "skyblue3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

p4 <- ggplot(a[a$Tumor=="COAD",], aes("", y = Number, fill = factor(Type), a$Total[4]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[4])+
  scale_fill_manual(values = c("wheat4", "skyblue3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

p5 <- ggplot(a[a$Tumor=="DLBC",], aes("", y = Number, fill = factor(Type), a$Total[5]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[5])+
  scale_fill_manual(values = c("wheat4", "skyblue3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

p6 <- ggplot(a[a$Tumor=="ESCA",], aes("", y = Number, fill = factor(Type), a$Total[6]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[6])+
  scale_fill_manual(values = c("wheat4", "skyblue3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

p7 <- ggplot(a[a$Tumor=="KIRP",], aes("", y = Number, fill = factor(Type), a$Total[7]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[7])+
  scale_fill_manual(values = c("wheat4", "skyblue3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

p8 <- ggplot(a[a$Tumor=="LIHC",], aes("", y = Number, fill = factor(Type), a$Total[8]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[8]) +
  scale_fill_manual(values = c("wheat4", "skyblue3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

p9 <- ggplot(a[a$Tumor=="READ",], aes("", y = Number, fill = factor(Type), a$Total[9]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[9])+
  scale_fill_manual(values = c("wheat4", "skyblue3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

p10 <- ggplot(a[a$Tumor=="SKCM",], aes("", y = Number, fill = factor(Type), a$Total[10]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[10])+
  scale_fill_manual(values = c("wheat4", "skyblue3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))


p11 <- ggplot(a[a$Tumor=="THYM",], aes("", y = Number, fill = factor(Type), a$Total[10]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[11])+
  scale_fill_manual(values = c("wheat4", "skyblue3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1,-0.1,-0.1,-0.1), "cm"))

p12 <- ggplot(a[a$Tumor=="UCEC",], aes("", y = Number, fill = factor(Type), a$Total[12]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[12])+
  scale_fill_manual(values = c("wheat4", "skyblue3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))


p13 <- ggplot(a[a$Tumor=="UCS",], aes("", y = Number, fill = factor(Type), a$Total[10]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[13])+
  scale_fill_manual(values = c("wheat4", "skyblue3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))


p14 <- ggplot(a[a$Tumor=="UVM",], aes("", y = Number, fill = factor(Type), a$Total[10]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[14])+
  scale_fill_manual(values = c("wheat4", "skyblue3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

grobs <- ggplotGrob(p11+theme(legend.position = "left"))$grobs
legend <- grobs[[which(sapply(grobs, function(x) a$name) == "guide-box")]]

pdf(file = "./Figures/Fig5a_DEG_piechat.pdf", 
    width=13, height=3)
####
#p<-plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,
plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,
          align="h",ncol=(nrow(a)/2),
          rel_widths = c(log10(a$Total[1]),
                            log10(a$Total[2]),
                            log10(a$Total[3]),
                            log10(a$Total[4]),
                            log10(a$Total[5]),
                            log10(a$Total[6]),
                            log10(a$Total[7]),
                            log10(a$Total[8]),
                            log10(a$Total[9]),
                            log10(a$Total[10]),
                            log10(a$Total[11]),
                            log10(a$Total[12]),
                            log10(a$Total[13]),
                            log10(a$Total[14])))


dev.off()

###############ob
a=read.table("ob.DEG.txt", header = T)

t1 <- ggplot(a[a$Tumor=="BLCA",], aes("", y = Number, fill = factor(Type), a$Total[1]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[1])+
  scale_fill_manual(values = c("wheat4", "hotpink3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

t2 <- ggplot(a[a$Tumor=="CESC",], aes("", y = Number, fill = factor(Type), a$Total[2]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[2])+
  scale_fill_manual(values = c("wheat4", "hotpink3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

t3 <- ggplot(a[a$Tumor=="CHOL",], aes("", y = Number, fill = factor(Type), a$Total[3]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[3])+
  scale_fill_manual(values = c("wheat4", "hotpink3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),
        axis.line=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

t4 <- ggplot(a[a$Tumor=="COAD",], aes("", y = Number, fill = factor(Type), a$Total[4]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[4])+
  scale_fill_manual(values = c("wheat4", "hotpink3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

t5 <- ggplot(a[a$Tumor=="DLBC",], aes("", y = Number, fill = factor(Type), a$Total[5]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[5])+
  scale_fill_manual(values = c("wheat4", "hotpink3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

t6 <- ggplot(a[a$Tumor=="ESCA",], aes("", y = Number, fill = factor(Type), a$Total[6]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[6])+
  scale_fill_manual(values = c("wheat4", "hotpink3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

t7 <- ggplot(a[a$Tumor=="KIRP",], aes("", y = Number, fill = factor(Type), a$Total[7]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[7])+
  scale_fill_manual(values = c("wheat4", "hotpink3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

t8 <- ggplot(a[a$Tumor=="LIHC",], aes("", y = Number, fill = factor(Type), a$Total[8]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[8]) +
  scale_fill_manual(values = c("wheat4", "hotpink3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

t9 <- ggplot(a[a$Tumor=="READ",], aes("", y = Number, fill = factor(Type), a$Total[9]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[9])+
  scale_fill_manual(values = c("wheat4", "hotpink3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

t10 <- ggplot(a[a$Tumor=="SKCM",], aes("", y = Number, fill = factor(Type), a$Total[10]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[10])+
  scale_fill_manual(values = c("wheat4", "hotpink3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))


t11 <- ggplot(a[a$Tumor=="THYM",], aes("", y = Number, fill = factor(Type), a$Total[10]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[11])+
  scale_fill_manual(values = c("wheat4", "hotpink3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=11,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1,-0.1,-0.1,-0.1), "cm"))

t12 <- ggplot(a[a$Tumor=="UCEC",], aes("", y = Number, fill = factor(Type), a$Total[12]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[12])+
  scale_fill_manual(values = c("wheat4", "hotpink3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))


t13 <- ggplot(a[a$Tumor=="UCS",], aes("", y = Number, fill = factor(Type), a$Total[10]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[13])+
  scale_fill_manual(values = c("wheat4", "hotpink3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))


t14 <- ggplot(a[a$Tumor=="UVM",], aes("", y = Number, fill = factor(Type), a$Total[10]))+
  geom_bar(width = 4, stat="identity") + coord_polar("y", start = 0, direction = 1)+
  ggtitle(label=a$Tumor[14])+
  scale_fill_manual(values = c("wheat4", "hotpink3")) +
  theme_classic()+theme(legend.position = "none")+
  geom_text(aes(label = paste0(Number, "")), position = position_stack(vjust = 0.5))+
  theme(axis.title=element_blank(),axis.line=element_blank(),axis.ticks=element_blank(),axis.text=element_blank(),plot.background = element_blank(),
        plot.title=element_text(color="black",size=12,face="plain",hjust=0.5)) +
  theme(plot.margin = unit(c(-0.1, -0.1, -0.1, -0.1), "cm"))

grobs <- ggplotGrob(p11+theme(legend.position = "left"))$grobs
legend <- grobs[[which(sapply(grobs, function(x) a$name) == "guide-box")]]

####
pdf(file = "./Figures/Fig5c_DEG_piechat.pdf", 
    width=13, height=3)
plot_grid(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,
             align="h",ncol=(nrow(a)/2),
             rel_widths = c(log10(a$Total[1]),
                            log10(a$Total[2]),
                            log10(a$Total[3]),
                            log10(a$Total[4]),
                            log10(a$Total[5]),
                            log10(a$Total[6]),
                            log10(a$Total[7]),
                            log10(a$Total[8]),
                            log10(a$Total[9]),
                            log10(a$Total[10]),
                            log10(a$Total[11]),
                            log10(a$Total[12]),
                            log10(a$Total[13]),
                            log10(a$Total[14])))

dev.off()

