rm(list=ls())
library(stringr)

#Input data
setwd("C:/tmp") #Set the work path
df <- read.table("output_tree.csv", header = T, sep = ",") #Input data

#Create a traversal stack S
S <- c(1) #S is used as a stack to store the tree structure of the breadth-first search
p <- 1 #Points to the current node in the stack
q <- 2 #Points to the empty space at the top of the stack
while (p < q){ #Until there are no untraversed nodes
  if (df$Chd1[S[p]] != 0){ #If there are child nodes, they are stored in the stack
    S[q] <- df$Chd1[S[p]]
    q <- q+1
  }
  if (df$Chd2[S[p]] != 0){ #If there are child nodes, they are stored in the stack
    S[q] <- df$Chd2[S[p]]
    q <- q+1
  }
  if (df$Chd3[S[p]] != 0){ #If there are child nodes, they are stored in the stack
    S[q] <- df$Chd3[S[p]]
    q <- q+1
  }
  p <- p+1 #The current node has been processed, accessing unprocessed nodes
}

#Calculate the maximum value of each node derived from the leaf node to the root node
lf2rt <- rep(-1,q-1) #The maximum value from the leaf node to the root node of the i-th node
while (q > 1) { #Count the maximum distance from the leaf nodes to the root node contained in each node, and q points to the empty site/processed site of the stack
  q <- q-1
  if (df$nNode[S[q]] == 1) { #If S[q] is a leaf node
    lf2rt[S[q]] <- df$Lth[S[q]] #The distance from the current node to the root node is the distance from the leaf node to the root node
  } else { #S[q] is not a leaf node
    if (df$Chd1[S[q]] != 0){ 
      lf2rt[S[q]] <- max(lf2rt[S[q]], lf2rt[df$Chd1[S[q]]])
    }
    if (df$Chd2[S[q]] != 0){ #If there is a leaf node in the child node, it is counted in the total number of leaf nodes
      lf2rt[S[q]] <- max(lf2rt[S[q]], lf2rt[df$Chd2[S[q]]])
    }
    if (df$Chd3[S[q]] != 0){ #If there is a leaf node in the child node, it is counted in the total number of leaf nodes
      lf2rt[S[q]] <- max(lf2rt[S[q]], lf2rt[df$Chd3[S[q]]])
    }
  }
}
df <- cbind(df,lf2rt)
write.csv(df,"output_dist_leaf2root_template.csv",row.names = F)