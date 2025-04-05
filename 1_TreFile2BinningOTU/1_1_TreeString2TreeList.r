rm(list=ls())
library(stringr)

#Input data
setwd("C:/tmp") #Set the work path
input <- read.table("input.tre", header = F, sep = ";") #Input data

tre <- input$V1

df <- data.frame(Name = c("1"), #Node Name
                 Dist = c(0), #The distance from the current node to the parent node
                 Lth = c(0.00), #The distance from the current node to the root node
                 Prt = c(0), #The ID of the parent node of the current node
                 nNode = c(0), #The number of leaf nodes owned by the current node
                 nChd = c(0), #The number of child nodes of the current node
                 Chd1 = c(0), #The ID of the first child node of the current node
                 Chd2 = c(0), #The number of the second child node of the current node
                 Chd3 = c(0) #The number of the third child node of the current node
)

#Convert a one-dimensional phylogenetic tree to a data frame
i <- 2 #Current string character position
pst <- 1 #The number of the current presenting node
n <- 1 #Number of nodes in the tree

while(i < str_length(tre)) { #Process the one-dimensional phylogenetic tree tre character by character
  
  ch <- substr(tre, i, i) #Extract the i-th character
  
  if (ch == '(') { #Construct new non-leaf nodes (all leaf nodes are OTUs)
    n = n+1 #Total number of nodes +1
    tdf <- data.frame(Name = c(as.character(n)), 
                      Dist = c(0), 
                      Lth = c(0.00),
                      Prt = c(pst),
                      nNode = c(0),
                      nChd = c(0), 
                      Chd1 = c(0), 
                      Chd2 = c(0), 
                      Chd3 = c(0)
    ) #Create a new non-leaf node tdf, the parent node of tdf is the current node pst
    df <- rbind(df,tdf) #Add tdf to the df node list
    df$nChd[pst] <- df$nChd[pst] + 1 #Total number of child nodes of pst + 1
    if (df$nChd[pst] == 1){ 
      df$Chd1[pst] = n
    } else if (df$nChd[pst] == 2){
      df$Chd2[pst] = n
    } else {
      df$Chd3[pst] = n
    } #Add the position of tdf in df to the child node position corresponding to pst
    pst <- n #Current node update
    i <- i+1
  } else if (ch == ",") { #Returns the parent node
    pst <- df$Prt[pst]
    i <- i+1
  } else if (ch == ")") { #Remove the bootstrap value and return to the parent node
    repeat {
      i <- i+1
      if (substr(tre, i, i) == ":"){
        break
      }
    } #Skip bootstrap value
    pst <- df$Prt[pst] #Returns the parent node
  } else if (ch == ":") { #Processing distance data
    pt <- i+1 #Record the starting position of the distance data in the string
    while (substr(tre, i, i) != "," && substr(tre, i, i) != ")") {
      i <- i+1
    } #Find the end position of the distance data in the string backward
    df$Dist[pst] <- as.numeric(substr(tre, pt, i-1))
  } else { #Create a new leaf node
    pt <- i #Record the starting position of the leaf node name in the string
    while (substr(tre, i, i) != ":"){ #Look backwards for the end position of the leaf node name in the string
      i <- i+1
    }
    n = n+1 #Total number of nodes +1
    tdf <- data.frame(Name = c(substr(tre, pt, i-1)), 
                      Dist = c(0), 
                      Lth = c(0.00),
                      Prt = c(pst),
                      nNode = c(1),
                      nChd = c(0), 
                      Chd1 = c(0), 
                      Chd2 = c(0), 
                      Chd3 = c(0)
    ) #Create a new leaf node tdf, the parent node of tdf is the current node pst
    df <- rbind(df,tdf) #Add tdf to the df node list
    df$nChd[pst] <- df$nChd[pst] + 1 #Total number of child nodes of pst + 1
    if (df$nChd[pst] == 1){ 
      df$Chd1[pst] = n
    } else if (df$nChd[pst] == 2){
      df$Chd2[pst] = n
    } else {
      df$Chd3[pst] = n
    } #Add the position of tdf in df to the child node position corresponding to pst
    pst <- n #Current node update
  }
}

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

#Count the number of OTUs contained in each node
while (q > 1) { #Count the number of leaf nodes contained in each node, and q points to the empty site/processed site of the stack
  q <- q-1
  if (df$Chd1[S[q]] != 0){ #If there is a leaf node in the child node, it is counted in the total number of leaf nodes
    df$nNode[S[q]] <- df$nNode[S[q]]+df$nNode[df$Chd1[S[q]]]
  }
  if (df$Chd2[S[q]] != 0){ #If there is a leaf node in the child node, it is counted in the total number of leaf nodes
    df$nNode[S[q]] <- df$nNode[S[q]]+df$nNode[df$Chd2[S[q]]]
  }
  if (df$Chd3[S[q]] != 0){ #If there is a leaf node in the child node, it is counted in the total number of leaf nodes
    df$nNode[S[q]] <- df$nNode[S[q]]+df$nNode[df$Chd3[S[q]]]
  }
}

#Calculate the distance from each node to the root node
for (i in 2:nrow(df)) { #Calculate the distance from each node to the root node
  df$Lth[i] <- df$Lth[df$Prt[i]] + df$Dist[i]
}

write.csv(df,"output_tree_template.csv",row.names = F)