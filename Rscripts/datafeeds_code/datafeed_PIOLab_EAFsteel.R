################################################################################
# datafeed_PIOLab_BOFsteel
# 
#
datafeed_name <- "EAFsteel"
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
  
# Loading raw data
# Load specific yearbook
source(paste0(path$Subroutines,"/Load_YearbookWSA.R"))
# Load function to align data with root classification
source(paste0(path$Subroutines,"/Read_ProductionWSA.R"))
# Select EAF steel production
item_page <- items[[5]]$page
# Read values and align with root classification
data <- Read_ProductionWSA(path,year,item_page,yb,concord)

# Loading function for estimating SE with linear regression
source(paste0(path$Subroutines,"/SE_LogRegression.R"))
data <- SE_LogRegression(data,0.2,0.03)

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
  # Set SE to 10%
  SE <- as.character(data$SE[i])
  
  # Add command for domestic supply table
  ALANG <- add_row(ALANG,'1' = paste0("DataFeed WSA Liquid steel from EAF ",reg_name),
                   Value = value,'Row parent' = reg_num,'Row child' = "1",'Row grandchild' = "22",
                   'Column parent' = reg_num,'Column child' = "2",'Column grandchild' = "31",S.E. = SE)
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
  