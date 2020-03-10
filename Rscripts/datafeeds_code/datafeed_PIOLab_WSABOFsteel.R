################################################################################

datafeed_name <- "WSABOFsteel"
print(paste0("datafeed_PIOLab_",datafeed_name," initiated."))

################################################################################
# Set library path when running on suphys server
if(Sys.info()[1] == "Linux"){
  .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")
  # Define location for root directory
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"}else{
  root_folder <- "C:/Users/hwieland/Github workspace/PIOLab/"}
################################################################################

# Initializing R script (load R packages and set paths to folders etc.)
source(paste0(root_folder,"Rscripts/Subroutines/InitializationR.R"))

path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name) # Add path for processed data

# Run code for ALANG commands
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/CreateALANGforWSAdata.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))
  