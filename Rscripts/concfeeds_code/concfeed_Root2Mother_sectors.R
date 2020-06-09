################################################################################
# This code creates the Root2Mother product and industry concordance by
# merging Source2Root concodances of primary data sources 
# that are expicit in the mother table (e.g. WSA)
# Date: 04.20.2020
# hanspeter.wieland@wu.c.at

IEdatafeed_name <- "Ind30Pro39v1" 

print(paste0("Start of ",IEdatafeed_name," Root2Mother sector concfeed."))

# Set library path when running on suphys server
if(Sys.info()[1] == "Linux"){
  .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")
  # Define location for root directory
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"}else{
  root_folder <- "C:/Users/hwieland/Github workspace/PIOLab/"}

# Initializing R script (load R packages and set paths to folders etc.)
source(paste0(root_folder,"Rscripts/Subroutines/InitializationR.R"))

# Read base regions, products and codes from mat-file if available
source(paste0(path$root,"Rscripts/Subroutines/Read_BaseClassification.R"))

# Load function to write tables to file
source(paste0(path$root,"Rscripts/Subroutines/Numbers2File.R"))

# set <- read.xlsx(paste0(path$Settings,"/Base/IE_settings.xlsx"),sheet = 2)

# Load Source to root sector aggregator of (i) WSA and (ii) IRP accounts 
# and (iii) additionally pseudo S2R where no raw data is available 
# Note that all tables are transposed (i.e. in root to source format):

path_wsa <- paste0(path$Concordance,"/WSA/WSA_Source2Root_WithLabels.xlsx")
path_IRP <- paste0(path$Concordance,"/IRP/20200426_IRP_Extraction_SecConc.xlsx")
path_add <- paste0(path$Concordance,"/Additional/20200426_Additional_Root2Mother_Sec_Ind30Pro39v1.xlsx")
  
S2R <- list("WSA" = list("process" =  read.xlsx(path_wsa, sheet = 1),
                         "flow" = read.xlsx(path_wsa, sheet = 2)
                         ),
            "IRP" = list("process" =  read.xlsx(path_IRP, sheet = 1),
                         "flow" = read.xlsx(path_IRP, sheet = 2)
                        ),
            "Add" = list("process" =  read.xlsx(path_add, sheet = 1),
                         "flow" = read.xlsx(path_add, sheet = 2)
                        )
            )


R2M[["process"]] <- matrix( 0, nrow = nrow(root$process), ncol = num$process )

## 1. Flow concordance

# Create empty R2M sector aggregators
R2M[["flow"]] <- matrix( 0, nrow = nrow(root$flow), ncol = num$flow )

# Select flows (by name or code) that are included in datafeeds (or the pseudo S2R = Additional) and the mother classification
Code <- list("WSA" = base$flow[base$flow$feed %in% colnames(S2R$WSA$flow), ],
             "IRP" = base$flow[base$flow$feed == "IRPextraction", ],
             "Add" = base$flow[base$flow$feed == "Additional", ]
             )
            
# Extract Source to root concordances (just to make sure the right subset is selected)
Conco <- list("WSA" = as.matrix( S2R$WSA$flow[1:nrow(root$flow), Code$WSA$feed] ),
              "IRP" = S2R$IRP$flow$binary,
              "Add" = as.matrix( S2R$Add$flow[1:nrow(root$flow), as.character( Code$Add$Code )] )
              )
 
# Write WSA S2R concordance into R2M flow concordance
R2M$flow[, Code$WSA$Code ] <- Conco$WSA[,Code$WSA$feed]

# Write IRP S2R into R2M flow concordance
R2M$flow[, Code$IRP$Code ] <- Conco$IRP 

# Write additional pseudo source 2 root i.e. root 2 mother into R2M flow concordance
R2M$flow[, Code$Add$Code ] <- Conco$Add[ , as.character( Code$Add$Code ) ]

# Set filename and path to R2M flow concordance 
filename <- paste0(path$Concordance,"/Sector Aggregators/",num$flow,"Products_SectorAggregator.csv")

if( sum(R2M$flow) > nrow(root$flow) ) print("Warning: Sum of R2M-flow conco is more than in root deifned.")

Numbers2File(R2M$flow,filename)  # Write R2M for flows to folder in concordance library

## 2. Process concordance

# Create empty R2M sector aggregators
R2M[["process"]] <- matrix( 0, nrow = nrow(root$process), ncol = num$process )

# 2.1 Select flows (by name or code) that correspond to only one feed
base_sel <- base$process[is.na(base$process$feed_2),]

Code <- list("WSA" = base_sel[base_sel$feed_1 %in% colnames(S2R$WSA$process), ],
             "IRP" = base_sel[base_sel$feed_1 == "IRPextraction", ],
             "Add" = base_sel[base_sel$feed_1 == "Additional", ]
             )

# Extract Source to root concordances (just to make sure the right subset is selected)
Conco <- list("WSA" = as.matrix( S2R$WSA$process[1:nrow(root$process), Code$WSA$feed_1] ),
              "IRP" = S2R$IRP$process$binary,
              "Add" = as.matrix( S2R$Add$process[1:nrow(root$process), as.character( Code$Add$Code )] )
              )

# Write all S2R concordance into R2M process concordance that correspond 1:1
R2M$process[, Code$WSA$Code ] <- Conco$WSA
R2M$process[, Code$IRP$Code ] <- Conco$IRP 
R2M$process[, Code$Add$Code ] <- Conco$Add

# 2.2 Select flows that correspond to two feeds
base_sel <- base$process[!is.na(base$process$feed_2) & is.na(base$process$feed_3),]

# Loop over rows
for(i in 1:nrow(base_sel))
{
  # Extract Source to root concordances for each feed
  Conco <- data.frame("feed_1" = S2R$WSA$process[1:nrow(root$process), base_sel$feed_1[i] ] ,
                      "feed_2" = S2R$WSA$process[1:nrow(root$process), base_sel$feed_2[i] ]
                      )

  R2M$process[, base_sel$Code[i] ] <- rowSums(Conco)  # Write concordance into R2M
}

# 2.3 Select flows that correspond to three feeds
base_sel <- base$process[!is.na(base$process$feed_3) & is.na(base$process$feed_4),]

# Loop over rows
for(i in 1:nrow(base_sel))
{
  # Extract Source to root concordances for each feed
  Conco <- data.frame("feed_1" = S2R$WSA$process[1:nrow(root$process), base_sel$feed_1[i] ],
                      "feed_2" = S2R$WSA$process[1:nrow(root$process), base_sel$feed_2[i] ],
                      "feed_3" = S2R$WSA$process[1:nrow(root$process), base_sel$feed_3[i] ]
                      )
  
  R2M$process[, base_sel$Code[i] ] <- rowSums(Conco)  # Write concordance into R2M
}

# 2.3 Select flows that correspond to four feeds
base_sel <- base$process[!is.na(base$process$feed_4),]

# Loop over rows
for(i in 1:nrow(base_sel))
{
  # Extract Source to root concordances for each feed
  Conco <- data.frame("feed_1" = S2R$WSA$process[1:nrow(root$process), base_sel$feed_1[i] ],
                      "feed_2" = S2R$WSA$process[1:nrow(root$process), base_sel$feed_2[i] ],
                      "feed_3" = S2R$WSA$process[1:nrow(root$process), base_sel$feed_3[i] ],
                      "feed_4" = S2R$WSA$process[1:nrow(root$process), base_sel$feed_4[i] ]
                      )
  
  R2M$process[, base_sel$Code[i] ] <- rowSums(Conco)  # Write concordance into R2M
}

# Set filename and path to R2M process concordance 
filename <- paste0(path$Concordance,"/Sector Aggregators/",num$process,"Industries_SectorAggregator.csv")

if( sum(R2M$process) > nrow(root$process) )
{
  print("Warning: Sum of R2M-process conco is more than in root deifned -> Normalizing conco !")
  R2M$process <- R2M$process / rowSums(R2M$process)
  
}
  

Numbers2File(R2M$process,filename)  # Write R2M for flows to folder in concordance library

