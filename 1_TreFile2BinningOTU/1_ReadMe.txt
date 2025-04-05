TreFile2BinningOTU.r:
	Input tree file input_tre_template.tre,output dataframe output_BIN_sumOTU_template.csv.
	Output file dataframe includes two columns: 
		OTU: leaf node of evolutionary tree
		Bin: divided bin

------------------------------------------------------------------------------------------------------------------------------
TreFile2BinningOTU.r is divided into five programs according to the functional modules. The following five programs and their intermediate output files can be used to understand the operation process.

1_1_TreeString2TreeList.r：
	Input tree file input_tre_template.tre, output dataframe output_tree_template.csv.
	Output file dataframe output_tree_template.csv includes:
		Name: the name of the current node (leaf node is OTU number, intermediate node is the row number)
		Dist: evolutionary distance from the current node to the parent node
		Lth: evolutionary distance from the current node to the root node
		Prt: parent node number of the current node
		nNode: number of leaf nodes contained in the current node
		nChd: number of child nodes of the current node (between 0-3)
		Chd1: row where child node 1 is located (0 means there is no such child node)
		Chd2: row where child node 2 is located (0 means there is no such child node)
		Chd3: row where child node 3 is located (0 means there is no such child node)

1_2_DIST_Leaf2Root.r：
	Input file dataframe output_tree_template.csv, output dataframe output_dist_leaf2root_template.csv.
	Output file dataframe output_dist_leaf2root_template.csv has one more column than output_tree_template.csv:
		lf2rt: The maximum evolutionary distance from the leaf node to the root node contained in the current node.

1_3_DIST_Leaf2Leaf.r：
	Input file dataframe output_dist_leaf2root_template.csv, output dataframe output_dist_leaf2leaf_template.csv.
	Output file dataframe output_dist_leaf2leaf_template.csv has one more column than output_dist_leaf2root_template.csv:
		lf2lf: The maximum evolutionary distance between the leaf nodes contained in the current node.

1_4_BIN.r：
	Input file dataframe output_dist_leaf2leaf_template.csv, output dataframe output_bin_template.csv.
	Output file dataframe output_bin_template.csv has three more columns than output_dist_leaf2leaf_template.csv:
		bin: bin number.
		nBin: number of leaf nodes contained in bin numbered n.
		hBin: representative node of bin.

1_5_BIN_sumOTU.r：
	Input file dataframe output_bin_template.csv, output dataframe output_BIN_sumOTU_template.csv.
	Output file dataframe includes two columns: 
		OTU: leaf node of evolutionary tree
		Bin: divided bin