setwd("./data/processedData/FiguresData")
library(TCGAbiolinks)
library(survival)
library(survminer)
library("Cairo")

d="UCEC"

clinical=read.table(paste("./data/clinical_data/select.addPurity.",d,"_clinical.txt", sep = ""), sep = "\t", header=T) 

expression=read.table(paste(d,"_expression", sep = ""),sep = "\t", header =T)

ids=read.table(paste(d,"_norVSob_up_sig", sep = ""), sep = "\t", header = F)

expression$gene_id <- gsub("\\|.*","",expression$gene_id)
expression = expression[!duplicated(expression$gene_id),]
rownames(ids)=ids$V1
rownames(expression)=expression$gene_id

Pvalue=c()
Symbol=c()
High=c()
Low=c()


#genels <- c("CFHR3","GPR171","STAT5B","SLCO1B1","LPIN2","MAD2L1","PNPLA7","SLC22A25","GINS3","GBA3")
#genels <- c("CENPK","C12orf48","TTK","CHTF18","AMY2B","LMF1","ANKRA2","HMGB2","HSP90AA1","CCDC99") ##lIHC ov
#genels <- c("ZDHHC1","ACTL8","RILPL2","SLC47A1","C11orf52","SCGB2A1","SLC25A35","LRRC36","ANAPC4","ASRGL1") #UCEC ov
#genels <- c("LRRC36","ANAPC4","SPATA18","FAM116B","NUCB2","LRP2BP","PPIB","RANGRF","LRRC56","TTC21A") #UCEC ob
### for overweight
#for(i in genels){
 # i="C12orf48"
  i="ZDHHC1"
  symbol=as.character(i)
  name=i
  if(!i %in% rownames(expression)){next}
  gene=data.frame(t(expression[name,])) 
  gene$number=as.numeric(as.character(gene[,1]))
  gene=subset(gene, !is.na(gene$number))
  
  gene$submitter_id= substr(row.names(gene),1,12)
  gene=gene[substr(rownames(gene),14,14)=="0",]
  
  gene$rank= ifelse(gene$number> median(gene$number),"High","Low")
  clinical$submitter_id=gsub("-",'.',clinical$submitter_id)
  gene_clinical=merge(clinical,gene,by="submitter_id")
  gene_clinical$vital_status
  gene_clinical$status[gene_clinical$vital_status=="Alive"]=0
  gene_clinical$status[gene_clinical$vital_status=="Dead"]=1
  gene_clinical$days=ifelse(gene_clinical$vital_status=="Alive",gene_clinical$days_to_last_follow_up,gene_clinical$days_to_death)
  #gene_clinical=gene_clinical[!is.na(gene_clinical$days),]
  gene_clinical <- gene_clinical[,c('days','status','rank')]
  sfit= survfit(Surv(days,status)~rank,data=gene_clinical)
 # Name=c(Name,name)
  Symbol=c(Symbol,symbol)
  
  Pvalue=c(Pvalue,surv_pvalue(sfit)$pval)
  High=c(High, mean(gene_clinical[gene_clinical$rank=="High",][,1], na.rm = T))
  Low=c(Low, mean(gene_clinical[gene_clinical$rank=="Low",][,1], na.rm = T))
  #if(surv_pvalue(sfit)$pval<=0.1){
  custom_theme <- function() {
    theme_survminer() %+replace%
      theme(
        plot.title=element_text(hjust=0.5)
      )
  }
  
  pdf(file="./Figures/Fig5e_UCEC_ob_sur.pdf",
      width = 5, height = 3.5)
  ggsurvplot(sfit,pval=TRUE,
                title=d,ggtheme=custom_theme(),xlim=c(0,4000),
                palette = c("hotpink3", "wheat4"),  ###hotpink3, skyblue3
                legend.title="",
                xlab = "Time (days)",
                legend.labs=c("Obesity_Upregulated","Normalweight_Downregulated"))

  dev.off()
  
###################ov
  i="ANAPC4"
  symbol=as.character(i)
  name=i
  if(!i %in% rownames(expression)){next}
  gene=data.frame(t(expression[name,])) 
  gene$number=as.numeric(as.character(gene[,1]))
  gene=subset(gene, !is.na(gene$number))
  
  gene$submitter_id= substr(row.names(gene),1,12)
  gene=gene[substr(rownames(gene),14,14)=="0",]
  
  gene$rank= ifelse(gene$number> median(gene$number),"High","Low")
  clinical$submitter_id=gsub("-",'.',clinical$submitter_id)
  gene_clinical=merge(clinical,gene,by="submitter_id")
  gene_clinical$vital_status
  gene_clinical$status[gene_clinical$vital_status=="Alive"]=0
  gene_clinical$status[gene_clinical$vital_status=="Dead"]=1
  gene_clinical$days=ifelse(gene_clinical$vital_status=="Alive",gene_clinical$days_to_last_follow_up,gene_clinical$days_to_death)
  #gene_clinical=gene_clinical[!is.na(gene_clinical$days),]
  gene_clinical <- gene_clinical[,c('days','status','rank')]
  sfit= survfit(Surv(days,status)~rank,data=gene_clinical)
  Symbol=c(Symbol,symbol)
  
  Pvalue=c(Pvalue,surv_pvalue(sfit)$pval)
  High=c(High, mean(gene_clinical[gene_clinical$rank=="High",][,1], na.rm = T))
  Low=c(Low, mean(gene_clinical[gene_clinical$rank=="Low",][,1], na.rm = T))
  #if(surv_pvalue(sfit)$pval<=0.1){
  custom_theme <- function() {
    theme_survminer() %+replace%
      theme(
        plot.title=element_text(hjust=0.5)
      )
  }
  
  pdf(file="./Figures/Fig5f_UCEC_ov_sur.pdf",
      width = 5, height = 3.5)
  ggsurvplot(sfit,pval=TRUE,
             title=d,ggtheme=custom_theme(),xlim=c(0,4000),
             palette = c("skyblue3", "wheat4"),  ###hotpink3, skyblue3
             legend.title="",
             xlab = "Time (days)",
             legend.labs=c("Overweight_Upregulated","Normalweight_Downregulated"))
  
  dev.off()


