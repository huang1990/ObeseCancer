##The effect of confounders between normal-weight and overweight/obese patients, 
##wilcox.test used for continous features, fihser.test used for discrete features.
options(stringsAsFactors = F)
library(magrittr)

setwd("./data/clinical_data/")
###14 cancer types we investigated
cancerNames <- c("BLCA", "CESC", "CHOL", "COAD", "DLBC", "ESCA", "KIRP",
                 "LIHC", "READ", "SKCM", "THYM", "UCEC", "UCS", "UVM")
###Two types of features
ContinousFeatures <- c("age_at_index","Purity","cigarettes_per_day")
DiscreteFeatures <- c("race","tumor_stage","primary_diagnosis", "gender", "tissue_or_organ_of_origin", "alcohol_history", "prior_malignancy")
##Statistic analysis
lapply(cancerNames,function(x){
  read.csv(paste("select.addPurity.",x,"_clinical.txt",sep=""),sep="\t") -> data
  data$cigarettes_per_day[is.na(data$cigarettes_per_day)] <- 0
  Continous <- data[,c("type",colnames(data)[colnames(data) %in% ContinousFeatures])]
  Continous[,2:ncol(Continous)] <- apply(Continous[,2:ncol(Continous)],2,function(x){ifelse(x=="#N/A",NA,x)})
  
  ContinousP <- data.frame()
  for(i in colnames(Continous)[-1]){
    ContinousP <- rbind(
      ContinousP,
      data.frame(Feature=i,pvalue= wilcox.test(as.numeric(Continous[Continous$type=="normal",i]),
                                               as.numeric(Continous[Continous$type=="obesity",i]),na.action=na.omit)$p.val)
    )
  }
  if(length(colnames(data)[colnames(data) %in% DiscreteFeatures])>0){
    Discrete <- data[,c("type",colnames(data)[colnames(data) %in% DiscreteFeatures])]
    DiscreteP <- data.frame()
    for(i in colnames(Discrete)[-1]){
      print(table(Discrete[,c("type",i)]))
      tab1 = table(Discrete[,c("type",i)])
      print(tab1)
      if(ncol(tab1)>1){
        if(length(which(!colMeans(tab1) == 0))>1){
          DiscreteP <- rbind(DiscreteP,data.frame(Feature=i,pvalue= fisher.test(tab1,simulate.p.value = TRUE)$p.value))
        }else{
          DiscreteP = rbind(DiscreteP,data.frame(Feature=i,pvalue=1))
        }
      }else{
        DiscreteP = rbind(DiscreteP,data.frame(Feature=i,pvalue=1))
      }
    }
    FeatureP <- rbind(ContinousP,DiscreteP)
    FeatureP$cancer_types <- rep(x,length(colnames(data)[colnames(data) %in% c(DiscreteFeatures,ContinousFeatures)]))
    return(FeatureP)
  }else{
    ContinousP$cancer_types <- rep(x,length(colnames(data)[colnames(data) %in% c(ContinousFeatures)]))
    return(ContinousP)
  }
}) %>%
dplyr::bind_rows() -> FeaturesBias
write.table(FeaturesBias,file="CounfounderAffectsBetweenNorObesity.tab",quote = F,row.names = F,sep="\t")

###heatmap plot
FeaturesBias <- merge(FeaturesBias,data.frame(cancer_types=rep(cancerNames,times=10),Feature=rep(c(DiscreteFeatures,ContinousFeatures),each=14)),by=c("Feature","cancer_types"),all=T)
FeaturesBias$tmp <- FeaturesBias$pvalue
FeaturesBias[,"tmp"][is.na(FeaturesBias[,"tmp"])] <- 1
FeaturesBias[,"tmp"][FeaturesBias[,"tmp"] <= 0.05]  <- 0
FeaturesBias[,"tmp"][FeaturesBias[,"tmp"] > 0.05 & FeaturesBias[,"tmp"] <1]  <- 0.5
FeaturesBias$tmp <- factor(FeaturesBias$tmp)
#pdf("./ProcessedClinicalData/Feature.bias.pdf",width=6,height = 2.5)#  order by immune cell abundance
ggplot(FeaturesBias[FeaturesBias$tmp %in% c(0,0.5),],aes(x=cancer_types,y=Feature))+
  geom_point(aes(color=tmp),size=4)+
  scale_color_manual(limits=c../data/clinical_data/(0,0.5),values=c("#E41A1C","darkgray"),guide=F)+
  scale_y_discrete(limit=c(ContinousFeatures,DiscreteFeatures),labels=c("Age","Purity","Smoking","Race","Stage","Histological type","Gender","tissue_or_organ_of_origin", "alcohol_history", "prior_malignancy"))+
  geom_point(data=FeaturesBias[FeaturesBias$tmp ==1,],aes(x=cancer_types,y=Feature),shape=4,size=4)+
  theme(panel.background=element_rect(colour="white",fill="white"), 
        #panel.grid.major=element_line(color="black"),
        #panel.grid.minor=element_blank(),
        plot.title = element_text(size = 16, colour = "black", hjust = 0.5),
        axis.title=element_blank(),
        axis.text.x=element_text(size=12,colour = "black"),#,angle=90,vjust=0.5,hjust=1),
        axis.text.y=element_text(size=12,colour = "black"),
        axis.ticks=element_blank(),
        legend.text=element_text(size=14),
        axis.line=element_blank())+
  geom_tile(data=FeaturesBias,aes(x=cancer_types,y=Feature),fill=NA,color="black") + 
  ggtitle("Normal-weight vs Obese tumors")
  

##################################Nor VS overweight
cancerNames <- c("BLCA", "CESC", "CHOL", "COAD", "DLBC", "ESCA", "KIRP",
                 "LIHC", "READ", "SKCM", "THYM", "UCEC", "UCS", "UVM")
###Two types of features
ContinousFeatures <- c("age_at_index","Purity","cigarettes_per_day")
DiscreteFeatures <- c("race","tumor_stage","primary_diagnosis", "gender", "tissue_or_organ_of_origin", "alcohol_history", "prior_malignancy")
##Statistic analysis
lapply(cancerNames,function(x){
  read.csv(paste("select.addPurity.",x,"_clinical.txt",sep=""),sep="\t") -> data
  data$cigarettes_per_day[is.na(data$cigarettes_per_day)] <- 0
  Continous <- data[,c("type",colnames(data)[colnames(data) %in% ContinousFeatures])]
  Continous[,2:ncol(Continous)] <- apply(Continous[,2:ncol(Continous)],2,function(x){ifelse(x=="#N/A",NA,x)})
  
  ContinousP <- data.frame()
  for(i in colnames(Continous)[-1]){
    ContinousP <- rbind(
      ContinousP,
      data.frame(Feature=i,pvalue= wilcox.test(as.numeric(Continous[Continous$type=="normal",i]),
                                               as.numeric(Continous[Continous$type=="overweight",i]),na.action=na.omit)$p.val)
    )
  }
  if(length(colnames(data)[colnames(data) %in% DiscreteFeatures])>0){
    Discrete <- data[,c("type",colnames(data)[colnames(data) %in% DiscreteFeatures])]
    DiscreteP <- data.frame()
    for(i in colnames(Discrete)[-1]){
      print(table(Discrete[,c("type",i)]))
      tab1 = table(Discrete[,c("type",i)])
      print(tab1)
      if(ncol(tab1)>1){
        if(length(which(!colMeans(tab1) == 0))>1){
          DiscreteP <- rbind(DiscreteP,data.frame(Feature=i,pvalue= fisher.test(tab1,simulate.p.value = TRUE)$p.value))
        }else{
          DiscreteP = rbind(DiscreteP,data.frame(Feature=i,pvalue=1))
        }
      }else{
        DiscreteP = rbind(DiscreteP,data.frame(Feature=i,pvalue=1))
      }
    }
    FeatureP <- rbind(ContinousP,DiscreteP)
    FeatureP$cancer_types <- rep(x,length(colnames(data)[colnames(data) %in% c(DiscreteFeatures,ContinousFeatures)]))
    return(FeatureP)
  }else{
    ContinousP$cancer_types <- rep(x,length(colnames(data)[colnames(data) %in% c(ContinousFeatures)]))
    return(ContinousP)
  }
}) %>%
  dplyr::bind_rows() -> FeaturesBias
write.table(FeaturesBias,file="CounfounderAffectsBetweenNorOverweight.tab",quote = F,row.names = F,sep="\t")

###heatmap plot
FeaturesBias <- merge(FeaturesBias,data.frame(cancer_types=rep(cancerNames,times=10),Feature=rep(c(DiscreteFeatures,ContinousFeatures),each=14)),by=c("Feature","cancer_types"),all=T)
FeaturesBias$tmp <- FeaturesBias$pvalue
FeaturesBias[,"tmp"][is.na(FeaturesBias[,"tmp"])] <- 1
FeaturesBias[,"tmp"][FeaturesBias[,"tmp"] <= 0.05]  <- 0
FeaturesBias[,"tmp"][FeaturesBias[,"tmp"] > 0.05 & FeaturesBias[,"tmp"] <1]  <- 0.5
FeaturesBias$tmp <- factor(FeaturesBias$tmp)
#pdf("./ProcessedClinicalData/Feature.bias.pdf",width=6,height = 2.5)#  order by immune cell abundance
ggplot(FeaturesBias[FeaturesBias$tmp %in% c(0,0.5),],aes(x=cancer_types,y=Feature))+
  geom_point(aes(color=tmp),size=4)+
  scale_color_manual(limits=c(0,0.5),values=c("#E41A1C","darkgray"),guide=F)+
  scale_y_discrete(limit=c(ContinousFeatures,DiscreteFeatures),labels=c("Age","Purity","Smoking","Race","Stage","Histological type","Gender","tissue_or_organ_of_origin", "alcohol_history", "prior_malignancy"))+
  geom_point(data=FeaturesBias[FeaturesBias$tmp ==1,],aes(x=cancer_types,y=Feature),shape=4,size=4)+
  theme(panel.background=element_rect(colour="white",fill="white"), 
        #panel.grid.major=element_line(color="black"),
        #panel.grid.minor=element_blank(),
        plot.title = element_text(size = 16, colour = "black", hjust = 0.5),
        axis.title=element_blank(),
        axis.text.x=element_text(size=12,colour = "black"),#,angle=90,vjust=0.5,hjust=1),
        axis.text.y=element_text(size=12,colour = "black"),
        axis.ticks=element_blank(),
        legend.text=element_text(size=14),
        axis.line=element_blank())+
  geom_tile(data=FeaturesBias,aes(x=cancer_types,y=Feature),fill=NA,color="black") + 
  ggtitle("Normal-weight vs Overweight tumors")


