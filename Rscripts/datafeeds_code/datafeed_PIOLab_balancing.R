####################################
# datafeed_PIOLab_balancing
#
# This data feed formulates the ALANG commands for the balancing condition
# for both industries and products
#
datafeed_name <- "balancing"
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


source(paste0(path$Subroutines,"/makeALANGheadline.R"))  # Create ALANG header

ALANG <- ALANG[,c(1:19,11:19)]  # Extend table with additional columns

# Balancing industries
ALANG <- add_row(ALANG,'1' = "Balancing industries",
                 Coef1 = "1",'Row parent' = "1-e",'Row child' = "1-e",'Row grandchild' = "1-e",
                 'Column parent' = "1:e",'Column child' = "1",'Column grandchild' = "1:e",
                 'Coef1.1' = "-1",'Row parent.1' = "1:e",'Row child.1' = "1",'Row grandchild.1' = "1:e",
                 'Column parent.1' = "1-e",'Column child.1' = "1-e",'Column grandchild.1' = "1-e")
  
# Balancing products
ALANG <- add_row(ALANG,'1' = "Balancing product markets",
                 Coef1 = "1",'Row parent' = "1:e",'Row child' = "1",'Row grandchild' = "1-e",
                 'Column parent' = "1:e~3",'Column child' = "2",'Column grandchild' = "1:e",
                 'Coef1.1' = "-1",'Row parent.1' = "1:e",'Row child.1' = "2",'Row grandchild.1' = "1:e",
                 'Column parent.1' = "1-e",'Column child.1' = "1-e",'Column grandchild.1' = "1-e")  


# Add other variables
ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$Incl <- "Y"
ALANG$Parts <- "2"
ALANG$Value <- "0"
ALANG$S.E. <- "0"
ALANG$Years <- ALANG$Years.1 <- "1"
ALANG$Margin <- ALANG$Margin.1 <- "1"
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
  
# Call script that writes the ALANG file to the respective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

rm(list = ls()) # clear workspace