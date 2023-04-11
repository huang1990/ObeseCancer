setwd("./data/processedData/mutationFre/")
##R version 3.5, platform x86_64-redhat-linux-gnu
options(stringsAsFactors = F)
#install.packages("dummies")
library(dummies)
library(dplyr)
#install.packages("formula.tools")
#load propensity score algorithm function.
source("./code/cal.R")

##setup new folder, deposit result in this folder
folder <- "PSM_Output"
if (!file.exists(folder)) { dir.create(folder) }
scripts.dir <- folder
setwd(scripts.dir)

disease=c("BLCA","CESC","CHOL","COAD","DLBC","ESCA","KIRP","LIHC","READ","SKCM","THYM","UCEC","UCS","UVM")
group <- c("overweight", "obesity")
d<-"UVM"
i <- "overweight"
for (d in disease) {
  for (i in group) {
    analysis <- paste("Norvs",i,sep = "")
    sum.ImmFeatureAll <- data.frame()
    
    data <- read.csv(paste("./data/processedData/clinical_data/",d,"_clinicalData.csv",sep = ""),header=T,stringsAsFactors=F)
    head(data)
    data <- data[!is.na(data$age_at_index),]
    data$age_at_index <- as.numeric(data$age_at_index)
    if (d =="LIHC" || d =="SKCM") {
      data$age_at_index <- NULL  ##for LIHC,SKCM age is NA
    }  
    if (d =="READ" || d =="SKCM") {
      data$prior_malignancy <- NULL  ##for SKCM, READ, malignancy is not available
    }
    data$Purity <- as.numeric(data$Purity)
    
    data <- unique(data) 
    rownames(data) <- data[,1]
    data <- data[,-1]
    
    data <- data[data$type %in% c("normal", i),] 
    head(data)
    
    if(analysis==paste("Norvs",i,sep = "")){
      # convert female and male to numeric 1,0 to suppress the warning message in lm
      data$type <- ifelse(data$type=="normal",1,0)
      colnames(data)[which(colnames(data)=="type")] <- "Z"
    }
    head(data)
    
    # convert to dummy
    
    dummy.feature <- setdiff(colnames(data),c("Z","age_at_index","Purity"))#,"pathologic_stage"))
    if(length(dummy.feature)>0){
      data.dum <- dummy.data.frame(data, names=dummy.feature)
      dummy.list <- attr(data.dum,"dummies")
      rm.col <- c() 
      #  for (i in 1:length(dummy.list))
      # {
      #  rm.col <- c(rm.col, dummy.list[[i]][length(dummy.list[[i]])])
      # }
      # data.dum <- data.dum[,-rm.col]
      data.dum$X0 <- rep(1, nrow(data.dum))
      colnames(data.dum) <- gsub(" ", ".", colnames(data.dum))
      if (d == "THYM") {
        data.dum <- subset(data.dum, select = -c(raceTypeOthers)) #for THYM
      }
      if (d == "COAD") {
        if (i == "obesity") {
          data.dum <- subset(data.dum, select = -c(raceTypeOthers)) #for COAD obesity
        }
      }
      if (d == "SKCM") {
        data.dum <- subset(data.dum, select = -c(tumor_stagenot.reported,tumor_stagestge.i,tumor_stagestge.iv, raceTypeblackOrAfricanAmerican, raceTypeOthers)) #SKCM
      }
      if (d == "LIHC") {
        if (i == "obesity") {
          data.dum <- subset(data.dum, select = -c(tumor_stagestge.iv))  #LIHC
        }
      }
      if (d == "ESCA") {
        data.dum <- subset(data.dum, select = -c(raceTypeblackOrAfricanAmerican, tumor_stagestge.i,alcohol_historyNot.Reported)) #ESCA
      }
      if (d == "BLCA") {
        data.dum <- subset(data.dum, select = -c(tumor_stagenot.reported, tumor_stagestge.i))  #BLCA
      }
      if (d == "CHOL") {
        if (i == "overweight") { 
          data.dum <- subset(data.dum, select = -c(tumor_stagestge.iii,raceTypeASIAN, raceTypeblackOrAfricanAmerican, raceTypeOthers)) #CHOL overweight
        } else {
          data.dum <- subset(data.dum, select = -c(tumor_stagestge.iv,raceTypeASIAN, raceTypeblackOrAfricanAmerican, raceTypeOthers, prior_malignancyyes)) #CHOL obesity
        }
      }
      if (d == "DLBC") {
        if (i == "overweight") {
          data.dum <- subset(data.dum, select = -c(genderfemale,gendermale,raceTypeblackOrAfricanAmerican)) #DLBC overwegiht
        } else {
          data.dum <- subset(data.dum, select = -c(raceTypeASIAN,raceTypeblackOrAfricanAmerican,raceTypeWHITE)) #DLBC obesity
        }
      }
      if (d == "KIRP") {
        data.dum <- subset(data.dum, select = -c(raceTypeASIAN)) #KRP
      }
      if (d == "READ") {
        if (i =="overweight") {
          data.dum <- subset(data.dum, select = -c(raceTypeblackOrAfricanAmerican,raceTypeWHITE)) #READ ov
        } else {
          data.dum <- subset(data.dum, select = -c(genderfemale,gendermale,tumor_stagenot.reported)) #READ ob
        }
      }
      if (d == "UVM") {
        if (i == "obesity") {
          data.dum <- subset(data.dum, select = -c(prior_malignancyyes,tumor_stagenot.reported)) #UVM obesity
        } else {
          data.dum <- subset(data.dum, select = -c(prior_malignancyyes)) #UVM overweight
        }
      }
      if (d == "UCS") {
        if (i == "overweight") { 
          data.dum <- subset(data.dum, select = -c(raceTypeASIAN)) #for UCS overwegiht 
        } else {
          data.dum <- subset(data.dum, select = -c(raceTypeASIAN,raceTypeOthers)) #for UCS obesity 
        }
      }
      # data.dum <- subset(data.dum, select = -c(raceTypeASIAN)) #for UCS
      
      # data.dum <- subset(data.dum, select = -c(tumor_stagenot.reported,tumor_stagestge.ii,tumor_stagestge.iii, prior_malignancyyes, raceTypeblackOrAfricanAmerican, raceTypeWHITE,genderfemale,gendermale)) #READ
      # data.dum <- subset(data.dum, select = -c(tumor_stagestge.iv))  #LIHC
      # data.dum <- subset(data.dum, select = -c(raceTypeASIAN))  #KIRP
      # data.dum <- subset(data.dum, select = -c(raceTypeblackOrAfricanAmerican, tumor_stagestge.i,alcohol_historyNot.Reported)) #ESCA
      # data.dum <- subset(data.dum, select = -c(tumor_stagenot.reported, tumor_stagestge.i))  #BLCA
      # data.dum <- subset(data.dum, select = -c(tumor_stagestge.i,tumor_stagestge.iv,tumor_stagestge.iii,raceTypeASIAN, raceTypeblackOrAfricanAmerican, raceTypeOthers, prior_malignancyyes)) #CHOL
      # data.dum <- subset(data.dum, select = -c(raceTypeblackOrAfricanAmerican)) #DLBC
      # data.dum <- subset(data.dum, select = -c(prior_malignancyyes,tumor_stagenot.reported)) #UVM
      #  UCEC,CESC 
      #form <- as.formula("Z~.") # should exclude X0
      exclude.col <- match(c("Z","X0"), colnames(data.dum)) 
      form <- as.formula(paste("Z~",paste(colnames(data.dum)[-exclude.col],collapse="+"),sep=""))
    }else{
      data.dum <- data
      data.dum$X0 <- rep(1, nrow(data.dum))
      colnames(data.dum) <- gsub(" ", ".", colnames(data.dum))
      if (d == "THYM") {
        data.dum <- subset(data.dum, select = -c(raceTypeOthers)) #for THYM
      }
      if (d == "COAD") {
        if (i == "obesity") {
          data.dum <- subset(data.dum, select = -c(raceTypeOthers)) #for COAD obesity
        }
      }
      if (d == "SKCM") {
        data.dum <- subset(data.dum, select = -c(tumor_stagenot.reported,tumor_stagestge.i,tumor_stagestge.iv, raceTypeblackOrAfricanAmerican, raceTypeOthers)) #SKCM
      }
      if (d == "LIHC") {
        if (i == "obesity") {
          data.dum <- subset(data.dum, select = -c(tumor_stagestge.iv))  #LIHC
        }
      }
      if (d == "ESCA") {
        data.dum <- subset(data.dum, select = -c(raceTypeblackOrAfricanAmerican, tumor_stagestge.i,alcohol_historyNot.Reported)) #ESCA
      }
      if (d == "BLCA") {
        data.dum <- subset(data.dum, select = -c(tumor_stagenot.reported, tumor_stagestge.i))  #BLCA
      }
      if (d == "CHOL") {
        if (i == "overweight") { 
          data.dum <- subset(data.dum, select = -c(tumor_stagestge.iii,raceTypeASIAN, raceTypeblackOrAfricanAmerican, raceTypeOthers)) #CHOL overweight
        } else {
          data.dum <- subset(data.dum, select = -c(tumor_stagestge.iv,raceTypeASIAN, raceTypeblackOrAfricanAmerican, raceTypeOthers, prior_malignancyyes)) #CHOL obesity
        }
      }
      if (d == "DLBC") {
        if (i == "overweight") {
          data.dum <- subset(data.dum, select = -c(genderfemale,gendermale,raceTypeblackOrAfricanAmerican)) #DLBC overwegiht
        } else {
          data.dum <- subset(data.dum, select = -c(raceTypeASIAN,raceTypeblackOrAfricanAmerican,raceTypeWHITE)) #DLBC obesity
        }
      }
      if (d == "KIRP") {
        data.dum <- subset(data.dum, select = -c(raceTypeASIAN)) #KRP
      }
      if (d == "READ") {
        if (i =="overweight") {
          data.dum <- subset(data.dum, select = -c(raceTypeblackOrAfricanAmerican,raceTypeWHITE)) #READ ov
        } else {
          data.dum <- subset(data.dum, select = -c(genderfemale,gendermale,tumor_stagenot.reported)) #READ ob
        }
      }
      if (d == "UVM") {
        if (i == "obesity") {
          data.dum <- subset(data.dum, select = -c(prior_malignancyyes,tumor_stagenot.reported)) #UVM obesity
        } else {
          data.dum <- subset(data.dum, select = -c(prior_malignancyyes)) #UVM overweight
        }
      }
      if (d == "UCS") {
        if (i == "overweight") { 
          data.dum <- subset(data.dum, select = -c(raceTypeASIAN)) #for UCS overwegiht 
        } else {
          data.dum <- subset(data.dum, select = -c(raceTypeASIAN,raceTypeOthers)) #for UCS obesity 
        }
      }
      # data.dum <- subset(data.dum, select = -c(raceTypeOthers)) #for THYM, COAD
      # data.dum <- subset(data.dum, select = -c(raceTypeASIAN)) #for UCS
      # data.dum <- subset(data.dum, select = -c(tumor_stagenot.reported,tumor_stagestge.i,tumor_stagestge.iv, raceTypeblackOrAfricanAmerican, raceTypeOthers)) #SKCM: 
      # data.dum <- subset(data.dum, select = -c(tumor_stagenot.reported,tumor_stagestge.ii,tumor_stagestge.iii, prior_malignancyyes, raceTypeblackOrAfricanAmerican, raceTypeWHITE,genderfemale,gendermale)) #READ
      # data.dum <- subset(data.dum, select = -c(tumor_stagestge.iv))  #LIHC
      # data.dum <- subset(data.dum, select = -c(raceTypeASIAN))  #KIRP
      # data.dum <- subset(data.dum, select = -c(raceTypeblackOrAfricanAmerican, tumor_stagestge.i,alcohol_historyNot.Reported)) #ESCA
      # data.dum <- subset(data.dum, select = -c(tumor_stagenot.reported, tumor_stagestge.i))  #BLCA
      # data.dum <- subset(data.dum, select = -c(tumor_stagestge.i,tumor_stagestge.iv,tumor_stagestge.iii,raceTypeASIAN, raceTypeblackOrAfricanAmerican, raceTypeOthers, prior_malignancyyes)) #CHOL
      # data.dum <- subset(data.dum, select = -c(raceTypeblackOrAfricanAmerican)) #DLBC
      # data.dum <- subset(data.dum, select = -c(prior_malignancyyes,tumor_stagenot.reported)) #UVM
      #  UCEC,CESC 
      #form <- as.formula("Z~.") # should exclude X0
      exclude.col <- match(c("Z","X0"), colnames(data.dum)) 
      form <- as.formula(paste("Z~",paste(colnames(data.dum)[-exclude.col],collapse="+"),sep=""))
    }
    
    # perform calculation
    
    # mutationFre
    mutation <- read.table(paste("./data/processedData/mutationFre/", d,".mutation.count.txt", sep = ""), sep = "\t", header = T)
    if (d == "UVM") {
    mutation$Nonstop <- NULL ##only for UVM(no Nonstop mutation)
    }
    mutationFeature <- mutation[substr(mutation$sample,14,15) %in% c("01"),] %>% dplyr::mutate(sample=substr(sample,1,12))
    mutationFeature <- mutationFeature[!duplicated( mutationFeature$sample),]
    Feature.pri <- mutationFeature[5:ncol(mutationFeature)]
    colnames(Feature.pri) <- colnames(mutationFeature)[5:ncol(mutationFeature)]
    rownames(Feature.pri) <- mutationFeature$sample
    head(Feature.pri)
    Feature.pri <- rm.zero.col(Feature.pri)
    #Feature.pri <- apply(Feature.pri,2,function(x){log2(x+1)})
    folder <- paste(d,"_",analysis,sep="")
    
    if (!file.exists(folder)) { dir.create(folder) }
    ###change the directary of 'weight.test'
    #trace(weight.test, edit = T)
    Feature.result <- weight.test(data.dum, form, Feature.pri, is.continuous=TRUE,weight=ifelse(analysis==paste("Norvs",i,sep = ""),"MW","ATT"),mirror.plot=FALSE, d, data.type= "Feature", outdir=paste(scripts.dir, "/",cancer,"_",analysis,sep=""),perm=FALSE)
    ###organize and save into table 
    sum.Immune <- summarize.p(Feature.pri, Feature.result, print=TRUE)
    summarize.p(Feature.pri, Feature.result, print=TRUE, cutoff=0.05)
    write.summary(sum.Immune, d, analysis,"MutationFre")
    write.result(Feature.result, d, analysis,"MutationFre")
    save(Feature.result, file=paste(d,"_", analysis,"_MutationFre_result.RData",sep=""))
  }
}




