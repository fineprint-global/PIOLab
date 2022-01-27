################################################################################
# Data feed: Production values of hot rolled products 
# Source: Statistical Yearbooks of World Steel Association
# Author: hanspeter.wieland@wu.ac.at
# Date: 19.03.2020 


datafeed_name <- "HotRolledProducts"  # Set name of feed 

# Determine loaction of root folder
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

# Initializing R script (R packages, folders etc.):
source(paste0(root_folder,"Rscripts/Subroutines/InitializationR.R"))

# Set path to processed data folder and data feed subroutines
path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name)  # Add datafeed specific path for output data
path["df_Subroutines"] <- paste0(path$Rscripts,"/datafeeds_code/datafeed_subroutines/") 
path$ALANG <- paste0(path$ALANG,"/",datafeed_name)

# Call script to clear ALANG and processed data folders of the present data feed
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/ClearFolders.R"))

# Loop over time series and create ALANG files ans raw data vectors in root classification
for(year in 2008:2017) source( paste0(path$df_Subroutines,"CreateALANGforWSAdata.R") ) # Run syntax

rm(list = ls()) # clear workspace

