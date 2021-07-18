################################################################################
# This concfeed code constructs a region root-2-mother (R2M) aggregator for 
# a 40 region mother classification. It builds on the 49 region R2M aggregator
# and a given mother-2-mother aggregator.

# Author: hanspeter.wieland@wu.ac.at
# Date : 07.22.2020

print("Start of 35 region R2M concfeed.")

# Set library path when running on suphys server
if(Sys.info()[1] == "Linux"){
  .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")
  # Define location for root directory
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"}else{
    root_folder <- "C:/Users/hwieland/Github workspace/PIOLab/"}

# Initializing R script (load R packages and set paths to folders etc.)
source(paste0(root_folder,"Rscripts/Subroutines/InitializationR.R"))

# Load function to write tables to file
source(paste0(path$root,"Rscripts/Subroutines/Numbers2File.R"))

# Load R2M aggregator (221 to 49 regions)
R2M_49 <- read.csv( paste0(path$Concordance,'/Region Aggregators/049_RegionAggregator.csv'),header = FALSE )
R2M_49 <- as.matrix(R2M_49)

# Load M2M aggregator (49 to 35 regions)
M2M <- read.xlsx( paste0(path$Concordance,'/Region Aggregators/RegionAggregator_49_to_35.xlsx') )
M2M <- as.matrix( M2M[,5:39] )


R2M_40 <- R2M_49 %*% M2M   # Create new R2M aggregator


# Check the sums of the aggregators and print out
print( paste( 'Check sum of R2M aggregator (221 -> 49):',sum(R2M_49) ) )
print( paste( 'Check sum of M2M aggregator (49 -> 40):',sum(M2M) ) )
print( paste( 'Check sum of new R2M aggregator (221 -> 40):',sum(R2M_40) ) )

# Write new aggregator into concordance library folder
Numbers2File( R2M_40, paste0(path$Concordance,'/Region Aggregators/035_RegionAggregator.csv') )

