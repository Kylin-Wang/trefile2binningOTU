rm(list=ls())
#Input data
setwd("C:/tmp") #Set the work path
df <- read.table("output_dist_leaf2leaf_template.csv", header = T, sep = ",") #Input data

#Determine the phylogenetic distance L, ensuring that after L is cut off, the distance between all connected species is less than the threshold
L <- 0.2
dL <- 0
for (i in 1:nrow(df)) {
  if (df$lf2lf[i] > L && dL < df$Lth[i]){
    dL <- df$Lth[i]
  }
}

#Binning
mark_bin <- function(x, bp, p){ #The bin number of the xth node is bp, and the representative node is p
  if(x != 0){
    bin[x] <<- bp
    hBin[x] <<- p
    mark_bin(df$Chd1[x], bp, p)
    mark_bin(df$Chd2[x], bp, p)
    mark_bin(df$Chd3[x], bp, p)
  }
}

bin <- rep(0,nrow(df)) #The bin number of the i-th OTU
hBin <- rep(0,nrow(df)) #The root of the bin corresponding to the i-th node (hBin[i]==i)
nBin <- rep(0,nrow(df)) #The number of species in the i-th bin
bp <- 1 #Points to unassigned bin numbers
for (i in 1:nrow(df)) {
  if (df$Lth[i] > dL && bin[i] == 0){
    nBin[bp] <- df$nNode[i]
    hBin[i] <- i
    mark_bin(i,bp,i)
    bp <- bp+1
  } else if (df$nNode[i] == 1 && bin[i] == 0) {
    nBin[bp] <- df$nNode[i]
    hBin[i] <- i
    bin[i] <- bp
    bp <- bp+1
  }
}

df <- cbind(df,bin,nBin,hBin)
write.csv(df,"output_bin_template.csv",row.names = F)