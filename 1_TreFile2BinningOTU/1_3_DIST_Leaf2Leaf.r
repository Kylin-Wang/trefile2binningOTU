rm(list=ls())

#Input data
setwd("C:/tmp") #Set the work path
df <- read.table("output_dist_leaf2root_template.csv", header = T, sep = ",") #Input data

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

#Calculate the maximum distance between leaf nodes within each node
ds <- rep(-1,nrow(df)) #The maximum distance between leaf nodes within the i-th node
while (q > 1) { #Count the maximum distance from the leaf nodes to the root node contained in each node, and q points to the empty site/processed site of the stack
  q <- q-1
  if (df$nNode[S[q]] == 1) {
    ds[S[q]] <- 0
  } else {
    t <- c(0,0,0) #Stores the maximum distance from leaf to root
    ts <- c(0,0,0) #Maximum distance between leaf nodes within a child node
    if (df$Chd1[S[q]] != 0){ #If the child node exists, store the maximum distance from the leaf node to which the child node belongs to the root node
      t[1] <- df$lf2rt[df$Chd1[S[q]]]
      ts[1] <- ds[df$Chd1[S[q]]]
    }
    if (df$Chd2[S[q]] != 0){ #If the child node exists, store the maximum distance from the leaf node to which the child node belongs to the root node
      t[2] <- df$lf2rt[df$Chd2[S[q]]]
      ts[2] <- ds[df$Chd2[S[q]]]
    }
    if (df$Chd3[S[q]] != 0){ #If the child node exists, store the maximum distance from the leaf node to which the child node belongs to the root node
      t[3] <- df$lf2rt[df$Chd3[S[q]]]
      ts[3] <- ds[df$Chd3[S[q]]]
    }
    ds[S[q]] <- max(sum(t)-min(t)-2*df$Lth[S[q]], ts)
  }
}
lf2lf <- ds
df <- cbind(df,lf2lf)
write.csv(df,"output_dist_leaf2leaf_template.csv",row.names = F)