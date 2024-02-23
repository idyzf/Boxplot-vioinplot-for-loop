library("ggpubr")
library("openxlsx")
library("corrplot")
library("ggplot2")
library("dplyr")

rm(list=ls())

rawdata <- read.xlsx("ruvcorrected.xlsx")
rownames(rawdata)<- rawdata$Name
rawdata<- rawdata[,-1]
print(names(rawdata))
####### selection only the DEG genes
deg<- read.xlsx("statistic_p0.05_fc_2.0.xlsx")
names_deg <- deg$.y.
genes_rui <- list("MGMT", "TP53", "RB1", "ATM" , "ATR", "WEE1",  "RAD51", "RAD51B","RAD51C","RAD51D", "PARP1", "PARP2", "PARP3", "PARP4")
names_deg <- append(names_deg, genes_rui)
rawdata<- t(rawdata)
data<- subset(rawdata, rownames(rawdata) %in% names_deg)

data<- as.data.frame(t(data))

#################### dados clinicos #####################
clinical<- read.xlsx("clinico_141.xlsx")
clinical<- subset(clinical, clinical$ID %in% rownames(data))
group<- as.data.frame(clinical$IDH1)
rownames(group)<- clinical$ID
group$ID <- rownames(group)
data$ID <- rownames(data)
data2 <- left_join(group, data)
rownames(data2)<- data2$ID
data2<- data2[,-2]
genes<- names(data2[,-1])
#######for loop #####
for (i in 1:length(genes)) { #i=1
  formula <- paste(genes[i], " ~ clinical$IDH1") %>% formula()
  file_name = paste("Boxplot", genes[i], ".tiff", sep="")
  tiff(file_name)
  p<- boxplot(formula,
              data = data2,
              title = genes[i], 
              xlab="Groups", 
              ylab= genes[i], 
              # Set box filling to transparent:
              fill = NA,
              # Set box border color based on group:
              col = c("purple", "orange"))
  
                             
 print(p)
 dev.off()
}
i=1
for (i in 1:length(genes)) {
  formula <- paste(genes[i], " ~ clinical$IDH1") %>% formula()
  file_name = paste("Violin", genes[i], ".tiff", sep="")
  tiff(file_name)
  teste1 <- compare_means(formula = formula, 
                          data = data2, method = "t.test",p.adjust.method = "fdr")
  p_adj<-teste1$p.adj
  q<- vioplot(formula,
              data = data2,
              title = genes[i], 
              xlab="Groups", 
              ylab= genes[i], 
              # Set box filling to transparent:
              fill = NA,
              # Set box border color based on group:
              col = c("purple", "orange")
              + geom_text( # Additional layer for p-values
                aes(x = clinical$IDH1, y = pos + max(pos) * 0.1, label = p_adj, 3)))
 
  print(q)
 dev.off()
}

 
