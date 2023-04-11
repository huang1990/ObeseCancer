
clinical <- read.table("./data/processedData/FiguresData/all_clinical.txt", sep = "\t", header = T)
disease = unique(clinical[clinical$bmi >=18.5,]$disease)
####### boxplot BMI
sub_id = clinical[clinical$bmi>=18.5,]$submitter_id
sub_bmi = clinical[clinical$bmi>=18.5,]$bmi
sub_disease = clinical[clinical$bmi>=18.5,]$disease
sub_data = data.frame(sub_id,sub_disease,sub_bmi)

sub_data$type[sub_data$sub_bmi>=18.5 & sub_data$sub_bmi<25] ="normal"
sub_data$type[sub_data$sub_bmi>=25 & sub_data$sub_bmi<30] ="overweight"
sub_data$type[sub_data$sub_bmi>=30] ="obesity"

##delete NA
sub_data <- sub_data[rowSums(is.na(sub_data)) != ncol(sub_data), ]
sub_data <- sub_data[complete.cases(sub_data), ]


dise = unique(sub_data$sub_disease)
for (i in dise){sub_data[sub_data$sub_disease==i,5] = median(sub_data[sub_data$sub_disease==i,]$sub_bmi)}
sort_data = sub_data[order(sub_data$V5),]
sort_data$sub_disease = factor(sort_data$sub_disease, ordered = F,levels = unique(sort_data$sub_disease))
sort_data$sub_disease = factor(sort_data$sub_disease, levels = c("BLCA","CESC","CHOL","COAD","DLBC","ESCA",
                                                                 "KIRP","LIHC","READ","SKCM","THYM","UCEC","UCS","UVM"))
##change legend order
sort_data$type <- factor(sort_data$type, levels = c("normal", "overweight", "obesity"))
lengend_title<-""

####save figures
pdf(file = "./Figures/Fig1a.pdf",  
    width=10,
    height=2.5)

ggplot(data=sort_data,aes(x=sub_disease,y=sub_bmi)) +
  scale_color_manual(lengend_title, values = c(normal='wheat4',overweight='skyblue3',obesity='hotpink3'))+
  geom_boxplot(outlier.size = 0.2) +
  geom_jitter(data = sort_data,aes(x=sub_disease,y=sub_bmi,col=type),size=0.8) +
  ylim(10,60) +
  theme_bw() +
  ylab("BMI") +
  theme(axis.text = element_text(size=12,color = "black"), axis.title = element_text(size=12),
        axis.title.x = element_blank(),
        legend.title = element_text(size=12),
        legend.text = element_text(size=12))

dev.off()


