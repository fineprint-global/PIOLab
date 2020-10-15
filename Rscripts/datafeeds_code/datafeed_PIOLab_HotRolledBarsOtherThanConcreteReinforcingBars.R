################################################################################
# Data feed: Production values of other hot rolled bars
# Source: Statistical Yearbooks of World Steel Association
# Author: hanspeter.wieland@wu.ac.at
# Date: 19.03.2020 

datafeed_name <- "HotRolledBarsOtherThanConcreteReinforcingBars"  # Set name of feed 

library(tidyverse)
library(tidyr)

# Determine loaction of root folder
################################################################################

# Set library path depending on whether data feed runs on Uni Sydney server or local
if(Sys.info()[1] == "Linux")
{
  # Setting the R package library folder on Uni Sydney server
  .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")
  
  # Define location of root directory on the Uni Sydney server:
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"
  
} else{
  
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

# Set paths to folders
root_folder1 <- root_folder
path <- list("Processed" = paste0(root_folder,"ProcessedData"))   # create path list
path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name)  # Add datafeed specific path for output data

# Call script to clear ALANG and processed data folders of the present data feed
source(paste0(root_folder,"Rscripts/datafeeds_code/datafeed_subroutines/ClearFolders.R"))

for (year in 2008:2017){
root_folder <- root_folder1  

# Location of the WSA syntax in the lab

subfun <- "Rscripts/datafeeds_code/datafeed_subroutines/CreateALANGforWSAdata.R"

source(paste0(root_folder,subfun)) # Run syntax
}
rm(list = ls()) # clear workspace

  