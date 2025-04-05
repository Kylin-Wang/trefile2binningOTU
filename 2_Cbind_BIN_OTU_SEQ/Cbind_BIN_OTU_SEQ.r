rm(list=ls())


setwd("C:/tmp") #设置工作路径

#以下将OTU和序列分成两行
df <- read.table("input_OTUseq_template.csv", header = F, sep = ",") #读入数据
otuseq <- data.frame(OTU = c(), Seq = c())
tmp <-data.frame(OTU = c("OTU"), Seq = c("SEQ"))

for (i in 1:nrow(df)) {
  if (i %% 2 == 1) {
    tmp$OTU[1] <- df$V1[i]
  } 
  else {
    tmp$Seq[1] <- df$V1[i]
    otuseq <- rbind(otuseq, tmp)
  }
}
#write.csv(otuseq,"otuseq.csv",row.names = F)

#以下将Bin分配到每个OTU
otubin <- read.table("input_otu_bin_template.csv", header = T, sep = ",") #读入数据

otuabund <- read.table("input_otu_abund_template.csv", header = T, sep = ",") #读入数据

result <- cbind(Bin = rep(0,nrow(otuabund)), Seq = rep("",nrow(otuabund)), otuabund)

for (i in 1:nrow(result)) {
  result$Bin[i] <- otubin$Bin[which(otubin$OTU == result$OTU[i])]
  result$Seq[i] <- otuseq$Seq[which(otuseq$OTU == result$OTU[i])]
}

result <- result[order(result$Bin),]

write.csv(result,"output_BinSeqOTUAbund_template.csv",row.names = F)