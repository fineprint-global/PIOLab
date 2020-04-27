################################################################################
# Data feed: Total production of ingots 
# Source: Statistical Yearbooks of World Steel Association
# Author: hanspeter.wieland@wu.ac.at
# Date: 19.03.2020 

datafeed_name <- "Ingots"

# Determine loaction of root folder
################################################################################
# Set library path when running on suphys server
if(Sys.info()[1] == "Linux"){
  .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")
  # Define location for root directory
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"}else{
    root_folder <- "C:/Users/hwieland/Github workspace/PIOLab/"}
################################################################################

# Location of the WSA syntax in the lab

subfun <- "Rscripts/datafeeds_code/datafeed_subroutines/CreateALANGforWSAdata.R"

source(paste0(root_folder,subfun)) # Run syntax

rm(list = ls()) # clear workspace