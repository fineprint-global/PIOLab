################################################################################
# datafeed_PIOLab_BOFsteel
# 
#
datafeed_name <- "WSAFlatRolledProducts"
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

# Long rolled products have the item code 7 in WSA data
item_id <- 8
# Set relative standard error for smallest and largest values in the data set
RSE <- list("small" = 0.2,"large" = 0.05)
# Set range of products and industries to be adressed by this feed
Grandchild <- list("RoW" = "43-58","Column" = "45-125")

# Load function and create ALANG commands
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/CreateALANGforWSAdata.R"))
ALANG <- CreateALANGforWSAdata(item_id,RSE,Grandchild,datafeed_name)

# Call script that writes the ALANG file to the respective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))
  