########################################################################
# 
# This data feed contains ALANG commands for different ratios in iron and steelmaking 
#

datafeed_name <- "RatioConstraints"
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

# Load ratio information from file
ratios <- read.xlsx(paste0(path$Raw,"/WSA/Yields in the steel industry.xlsx"),sheet = 1) %>%
  select(Process,Average,Maximum,Minimum)
# Select slag flow
slag <- ratios$Average[ratios$Process == "BF slag rate"]

# Load specific yearbook
source(paste0(path$Subroutines,"/Load_YearbookWSA.R"))
# Load function to align data with root classification
source(paste0(path$Subroutines,"/Read_ProductionWSA.R"))
# Select Pig Iron production
item_page <- items[[2]]$page
# Read values and align with root classification
data <- Read_ProductionWSA(path,year,item_page,yb,concord)

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))

for(r in data$Code)
{
  reg_name <- root$region$Name[r]
  reg_num <- as.character(r)
  
  # Add command for domestic Use table
  ALANG <- add_row(ALANG,'1' = paste0("Ratio Slag-PigIron: ",reg_name),
                  'Row parent' = reg_num,'Column parent' = reg_num)
  
  
}

ALANG$`Column child` <- "2:3~8"
ALANG$`Column grandchild` <- "11;2"
ALANG$Coef1 <- paste0("R [1000;",slag,"]")  
ALANG$S.E. <- "0.1"
ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$`Row child` <- "1"
ALANG$`Row grandchild` <- "6"
ALANG$Value <- "0"
ALANG$Incl <- "Y"
ALANG$Parts <- "1"
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
ALANG$Years <- "1"
ALANG$Margin <- "1"

# Call script that writes the ALANG file to the repsective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))
