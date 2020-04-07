# This section compiles and loads the Source2Root and Source2Mother maps
# for the Initial Estimate which are used in the compilation of the final IE tables

# Load Source2Root concordances: 


Source2Root <-list("WSA" = as.matrix( read.csv(paste0(path$Concordance,"/WSA/WSA_Source2Root_Product.csv"),header = FALSE) ))

Source2Root$WSA <- Source2Root$WSA[Code$WSA,]  # Select only finished steel

Source2Root$WSA <- Source2Root$WSA / rowSums(Source2Root$WSA)  # Create map (normalized concordance)

Source2Root[["Cullen"]] <- list("Product" = as.matrix( read.csv(paste0(path$Concordance,"/Cullen/Cullen_Source2Root_Product.csv"),header = FALSE) ),
                                "EndUse" = as.matrix( read.csv(paste0(path$Concordance,"/Cullen/Cullen_Source2Root_EndUse.csv"),header = FALSE) ))

# Compile Source to mother concordance for finished steel products

Source2Mother <- list("WSA" = Source2Root$WSA %*% as.matrix(ProductAggregator[,Code$base$Finished]) )

# Load WSA Source2Source map for estimate final use of hot rolled coil-sheet-strip

Source2Source <- list("WSA" = as.matrix(read.csv(paste0(path$Concordance,"/WSA/WSA_Source2Source.csv"),header = FALSE)) )

Source2Source$WSA <- Source2Source$WSA[Code$WSA,Code$WSA]


# Create map to allocate mother to source:

Mother2Source_Map <- list("WSA" = t(Source2Mother$WSA) / colSums(Source2Mother$WSA) ) 

Mother2Source_Map$WSA[is.na(Mother2Source_Map$WSA)] <- 0 # Because forgings is not form WSA, set NA to 0

# Create mother2mother concordance = parent to child processes

Mother2Mother <-  list("WSA" = Mother2Source_Map$WSA %*% Source2Source$WSA %*% Source2Mother$WSA )

colnames(Mother2Mother$WSA) <- rownames(Mother2Mother$WSA) <- Code$base$Finished


# Load products to end-use map from Cullen et al 2012

Products2EndUse <- read.csv(paste0(path$IE_Processed,"/Cullen/ProductsToEndUse.csv"))

rownames(Products2EndUse) <- Products2EndUse$X  # Write products names to row names

Products2EndUse$X <- NULL  # Delete product name column 

Products2EndUse <- as.matrix(Products2EndUse)

# Create Source to Root and Source to mother maps:

Source2Root_Map <- list("Cullen" = list("EndUse" = (Source2Root$Cullen$EndUse / rowSums(Source2Root$Cullen$EndUse) ),
                                        "Product" = (Source2Root$Cullen$Product / rowSums(Source2Root$Cullen$Product) )
)
)

Source2Mother[["Cullen"]] <- list("EndUse" =  Source2Root_Map$Cullen$EndUse %*% as.matrix(ProductAggregator[,Code$base$Final]),
                                  "Product" = Source2Root_Map$Cullen$Product %*% as.matrix(ProductAggregator[,Code$base$Finished])
)

Source2Mother$Cullen$Yields <- Source2Mother$Cullen$EndUse  # Add concordance for yields

Source2Mother$Cullen$Yields[Source2Mother$Cullen$Yields > 0] <- 1  # Set shares to 1 for yields


# Add mother sector names to maps:

colnames(Source2Mother$Cullen$EndUse) <- base$product$Name[Code$base$Final]

colnames(Source2Mother$Cullen$Product) <- base$product$Name[Code$base$Finished]

# Translate product to end-use map to base classification of mother table: 

Products2EndUse <- t(Source2Mother$Cullen$Product)  %*% Products2EndUse %*% Source2Mother$Cullen$EndUse


