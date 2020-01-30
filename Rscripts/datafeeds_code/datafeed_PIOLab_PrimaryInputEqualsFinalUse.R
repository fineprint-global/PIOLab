########################################################################
# datafeed_PIOLab_PrimaryInputEqualsFinalUse
#
# This data feed formulates ALANG commands so that the sum of the primary inputs 
# (inputs from nature and EoL scrap) equals the sum of the final use (incl. flows to the environment) 

datafeed_name <- "PrimaryInputEqualsFinalUse"
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

# Read global extraction and EOL values
source(paste0(path$Subroutines,"/Read_ExtractionIRP.R"))
DE <- sum(DE$Quantity)
# For now just this (assume per ton of iron ore 1.3 ton of flux, air and coke are required)
# Set SE to 5%
tot <-  DE + DE*1.3 
error <- 0.05
SE <- as.character(round(tot*error))
# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))
# Extend table with additional columns
ALANG <- ALANG[,c(1:19,11:19)]

ALANG <- add_row(ALANG,'1' = "Sum of primary inputs equals sum of final use",
                 Coef1 = "1",'Row parent' = "1-e",'Row child' = "3",'Row grandchild' = "1-e",
                 'Column parent' = "1-e",'Column child' = "1",'Column grandchild' = "1-e",
                 'Coef1.1' = "-1",'Row parent.1' = "1-e",'Row child.1' = "1-e",'Row grandchild.1' = "1-e",
                 'Column parent.1' = "1-e",'Column child.1' = "3",'Column grandchild.1' = "1-e")

# Add other variables
ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$Incl <- "Y"
ALANG$Parts <- "2"
ALANG$Value <- "0"
ALANG$S.E. <- SE
ALANG$Years <- ALANG$Years.1 <- "1"
ALANG$Margin <- ALANG$Margin.1 <- "1"
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""

# Call script that writes the ALANG file to the repsective folder in the root
source(paste0(root_folder,"Rscripts/datafeeds_code/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

