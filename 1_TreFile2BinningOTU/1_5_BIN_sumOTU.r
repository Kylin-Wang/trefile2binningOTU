rm(list=ls())
library(stringr)

#Input data
setwd("C:/tmp") #Set the work path
df <- read.table("output_bin_template.csv", header = T, sep = ",") #Input data
#binning
hash <- matrix(FALSE,nrow(df),nrow(df)) #hash[i,j]=TRUE means i is the ancestor node of j
for (i in 1:nrow(df)) {
  p <- i
  while (p != 0) {
    hash[p,i] <- TRUE
    p <- df$Prt[p]
  }
}

NNdist <- function(i,j){
  p <- j #p represents an ancestor node of j
  while (!hash[p,i]) { #If node p is not an ancestor of node i
    p <- df$Prt[p] #p is updated to the ancestor node of p
  }
  return(df$Lth[i]+df$Lth[j]-2*df$Lth[p]) #Returns the distance between i and j
}

ds <- 24 #Merge until each bin contains at least ds leaf nodes
flag <- TRUE
while (flag) { #Flag determines whether there is still a bin with less leaf nodes than ds
  flag <- FALSE
  subbin <- unique(df$hBin[which(df$hBin != 0)]) #Extract the representative node of the current bin (hBin[i]==i)
  for(i in rev(subbin)) {
    if (df$hBin[i] == i && df$nBin[df$bin[i]] < ds) { #If the i node is still the representative node and the number of OTUs in the bin is less than ds
      flag <- TRUE #This round of loop performs the merge operation
      dL <- df$lf2rt[1] * 2 #dL is the shortest distance from each representative node to the i node
      tp <- 0 #The representative node with the shortest distance to the i node
      for(j in subbin) { 
        if (df$hBin[j] == j && i != j) { #If the j node is still the representative node and is not the i node
          tL <- NNdist(i, j) #tL is the distance between i and j
        }
        if (tL < dL) { #If a representative node j with a shorter distance to i is found
          dL <- tL #dL replaced by tL
          tp <- j #tp is denoted by j
        }
      }
      df$nBin[df$bin[tp]] <- df$nBin[df$bin[tp]] + df$nBin[df$bin[i]] #The total number of OTUs in the bin where tp is located plus the number of OTUs in the bin where i is located
      df$nBin[df$bin[i]] <- 0 #The original bin of i is empty
      df$hBin[which(df$hBin == i)] <- tp #The nodes belonging to the i bin are changed to belong to the tp bin.
      df$bin[which(df$bin == df$bin[i])] <- df$bin[tp]
    }
  }
}

result <- data.frame(OTU = df$Name, Bin = df$hBin)

for (i in nrow(result):1) {
  if (!str_detect(result$OTU[i],"OTU")){
    result <- result[-i,]
  }
}

outbin <- unique(result$Bin)

for (i in 1:length(outbin)) {
  result$Bin[which(result$Bin == outbin[i])] <- -i
}

result$Bin <- -result$Bin

result <- result[order(result$Bin),]

write.csv(result,"output_bin_sumOTU_template.csv",row.names = F)
