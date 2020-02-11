################################################################################
#
datafeed_name <- "PauliukEoL"
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
  
# Load Pauliuk data in root classification 
source(paste0(path$Subroutines,"/Load_PauliukEoL.R"))
# Check whether NA exist and delete if so
data <- data[!is.na(data$Code),]

# Loading function for estimating SE with linear regression
source(paste0(path$Subroutines,"/SE_LogRegression.R"))
data <- SE_LogRegression(data,0.50,0.1)

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))
# Extend table with additional columns
  
for(i in 1:nrow(data))
{ 
  # Get root_code of region 
  reg_num <- data$Code[i]
  reg_name <- as.character(root$region$Name[reg_num])
  reg_num <- as.character(reg_num)
  # Read extraction value
  value <- as.character(data$Quantity[i])
  # Set SE
  SE <- as.character(data$SE[i]) 
  
  # Add command for domestic Use table
  ALANG <- add_row(ALANG,'1' = paste0("DataFeed Pauliuk EoL ",reg_name),
                   Value = value,'Row parent' = reg_num,'Row child' = "3",'Row grandchild' = "2",
                   'Column parent' = reg_num,'Column child' = "1",'Column grandchild' = "64-65",S.E. = SE)
}
# Add other variables
ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$Incl <- "Y"
ALANG$Parts <- "1"
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
ALANG$Years <- "1"
ALANG$Margin <- "1"
ALANG$Coef1 <- "1"
  
# Call script that writes the ALANG file to the repsective folder in the root
source(paste0(root_folder,"Rscripts/datafeeds_code/WriteALANG2Folder.R"))
  
print(paste0("datafeed_PIOLab_",datafeed_name," finished."))
  