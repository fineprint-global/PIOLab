################################################################################
#
datafeed_name <- "KrausmannTotalsEoL"
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
  
# Load data 
data <- read.csv(paste0(path$Raw,"/KrausmannTotalsEoL/GECKrausmann2018 iron steel results.csv"))
data <- data[data$Year == year,"Outputs.from.stock.and.use..EoL"]
# Krausmann is in 1000 tons, convert to tons 
data <- data * 1000

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))

# Load RSE settings
RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)

# Set SE
SE <- data*RSE$Minimum
  
# Add command for domestic Use table
ALANG <- add_row(ALANG,'1' = paste0("Krausmann total EoL-Scrap ",year))

ALANG$Value <- as.character(data)
ALANG$S.E. <- as.character(SE)

ALANG$`Row parent` <- "1-e"
ALANG$`Row child` <- "3"
ALANG$`Row grandchild` <- "2"

ALANG$`Column parent` <- "1-e"
ALANG$`Column child` <- "1"
ALANG$`Column grandchild` <- "1-e"

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
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))
  
print(paste0("datafeed_PIOLab_",datafeed_name," finished."))
  