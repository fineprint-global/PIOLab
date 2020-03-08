################################################################################

datafeed_name <- "IRPextraction"
print(paste0("datafeed_PIOLab_",datafeed_name," initiated."))

# Create wrapper for writing clean data 
WriteFile <- function(d,t)
{
  write.table(d,file = t,row.names = FALSE,col.names = FALSE,sep = ",",na = "NaN")
}

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
source(paste0(path$Subroutines,"/Read_ExtractionIRP.R"))

# Loading function for estimating SE with linear regression
source(paste0(path$Subroutines,"/SE_LogRegression.R"))

RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)

data <- SE_LogRegression(data,RSE$Minimum,RSE$Maximum)

n_reg <- nrow(root$region) # Number of root regions

data_new <- matrix(NaN,nrow = n_reg,ncol = 2)

data_new[data$Code,1] <- data$Quantity
data_new[data$Code,2] <- data$SE
data <- as.data.frame(data_new)
colnames(data) <- c("RHS","SE")
remove(data_new)

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))

# Check if folder with processed data exists, in case delete and create empty one
path_set <- paste0(path$root,"ProcessedData/",datafeed_name)
if(dir.exists(path_set)) unlink(path_set,recursive = TRUE) 
dir.create(path_set)

# Set path to Sector Concordance
filename <- list("IndAgg" ="Sector Aggregators/20IndV1_SectorAggregatorIndustries.csv")

IndAgg <- read.csv(paste0(path$Concordance,"/",filename$IndAgg),header = FALSE) 
IndAgg <- as.matrix(IndAgg)

# Create Root2Root region concordance (identity )
RegAgg <- diag(length(root$region$Code)) # Create region aggregator

# Set name of concordance
filename["RegAgg"] <- "Root2Root_Reg_Concordance.csv"

# Write to folder 
WriteFile(RegAgg,paste0(path$Concordance,"/",filename$RegAgg))

# Define section of path to processed data file
filename["RHS"] <- paste0(path$Processed,"/",datafeed_name,"/",year,"_")

for(i in root$region$Code) 
{
  data_sel <- data.frame("RSE" = rep(NaN,n_reg),"SE" = rep(NaN,n_reg))
  
  data_sel$RSE[i] <- data$RHS[i]
  data_sel$SE[i] <- data$SE[i]
  
  # Write RHS and SE data to folder
  WriteFile(data_sel$RSE,paste0(filename$RHS,"RHS_",root$region$RootCountryAbbreviation[i],"_.csv")) 
  WriteFile(data_sel$SE[i],paste0(filename$RHS,"SE_",root$region$RootCountryAbbreviation[i],"_.csv")) 
  
  # Add command for domestic Use table
  ALANG <- add_row(ALANG,'#' = i)
}

ALANG$`1` <- paste0("DataFeed IRP Extraction ",year," ",root$region$Name)
ALANG$Value <- paste0(filename$RHS,"RHS_",root$region$RootCountryAbbreviation,"_.csv")                   
ALANG$S.E. <- paste0(filename$RHS,"SE_",root$region$RootCountryAbbreviation,"_.csv")

ALANG$Coef1 <- "1"
ALANG$Years <- "1"
ALANG$Margin <- "1"

ALANG$`Row parent` <- "1:e"
ALANG$`Row child` <- "3"
ALANG$`Row grandchild` <- "1"

ALANG$`Column parent` <- "1:e~3"
ALANG$`Column child` <- "1"
ALANG$`Column grandchild` <- "1-4"

ALANG$Incl <- "Y"
ALANG$Parts <- "1"
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
  
# Call script that writes the ALANG file to the repsective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))
  
print(paste0("datafeed_PIOLab_",datafeed_name," finished."))
 
