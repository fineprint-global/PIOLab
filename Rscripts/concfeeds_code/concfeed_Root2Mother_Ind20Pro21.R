################################################################################
# This concfeed code constructs a sectoral root-2-mother (R2M) aggregator for 
# the 20 processes and 21 flows IE. It builds on the 30by39 sector R2M aggregator


# Author: hanspeter.wieland@wu.ac.at
# Date : 25.01.2022

print("Start of 20by21 sector R2M concfeed.")

################################################################################
# Set library path depending on whether data feed runs on Uni Sydney server or local
if(Sys.info()[1] == "Linux")
{
  # Define location of root directory on the Uni Sydney server:
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"
  
  if(dir.exists(root_folder))
  {
    # Setting the R package library folder on Uni Sydney server
    .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")  
  } else{
    # Define location of root directory on the WU Vienna server:
    root_folder <- "/data/WULab/Roots/PIOLab/"
  }
  
  
} else{
  
  library(tidyr)
  # Locating folder where the present script is stored locally to derive the root folder 
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  
  if(length(this_file)==0) this_file <- rstudioapi::getSourceEditorContext()$path
  
  root_folder <- substr(dirname(this_file),1,nchar(dirname(this_file))-23)
  remove(this_file)
}
################################################################################

# Initializing R script (load R packages and set paths to folders etc.)
source(paste0(root_folder,"Rscripts/Subroutines/InitializationR.R"))

# Load function to write tables to file
source(paste0(path$root,"Rscripts/Subroutines/Numbers2File.R"))

# Load R2M aggregator (221 to 49 regions)
R2M_Ind30 <- read.csv( paste0(path$Concordance,'/Sector Aggregators/30Industries_SectorAggregator.csv'),header = FALSE )
R2M_Ind30 <- as.matrix(R2M_Ind30)
R2M_Pro39 <- read.csv( paste0(path$Concordance,'/Sector Aggregators/39Products_SectorAggregator.csv'),header = FALSE )
R2M_Pro39 <- as.matrix(R2M_Pro39)

# Load M2M aggregator 
M2M_Ind <- read.xlsx( paste0(path$Concordance,'/Sector Aggregators/Aggregator_30by39_to_20by21.xlsx'), sheet = 1, rowNames = TRUE )
M2M_Ind <- as.matrix( M2M_Ind )
M2M_Pro <- read.xlsx( paste0(path$Concordance,'/Sector Aggregators/Aggregator_30by39_to_20by21.xlsx'), sheet = 2,  rowNames = TRUE )
M2M_Pro <- as.matrix( M2M_Pro )

# Create new R2M aggregators
R2M_Ind_new <- R2M_Ind30 %*% M2M_Ind   
R2M_Pro_new <- R2M_Pro39 %*% M2M_Pro   

# Check the sums of the aggregators and print out
print( paste( 'Check sum of R2M_Ind aggregator (76 -> 20):',sum(R2M_Ind_new) ) )
print( paste( 'Check sum of R2M_Pro aggregator (266 -> 21):',sum(R2M_Pro_new) ) )

# Write new aggregator into concordance library folder
Numbers2File( R2M_Ind_new, paste0(path$Concordance,'/Sector Aggregators/20Industries_SectorAggregator.csv') )
Numbers2File( R2M_Pro_new, paste0(path$Concordance,'/Sector Aggregators/21Products_SectorAggregator.csv') )

