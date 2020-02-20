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

# Issues regarding Pauliuk EoL data:
# A number of countries are only available as aggregated regions, these are
# the former USSR (root code: 178), Former Yugoslavia (242), Czechoslovakia (56)
# Belgium-Luxembbourg. Furthermore the data set does not include Taiwan (212).
# Therefore write ALANG commands that specifically address the sum of these countries
# and add NaN for Taiwan. 

# Loading function for estimating SE with linear regression
source(paste0(path$Subroutines,"/SE_LogRegression.R"))
data <- SE_LogRegression(data,0.50,0.1)

data <- select(data,Code,Quantity,SE)
data[nrow(data)+1,] <- c(212,NA,NA)

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))
# Extend table with additional columns
  
for(r in data$Code)
{ 
  reg_name <- as.character(root$region$Name[r])
  # Read value
  value <- as.character(data$Quantity[data$Code == r])
  # Set SE
  SE <- as.character(data$SE[data$Code == r]) 
  
  if(r == 178) # Former USSR
  {
    # Codes of former Soviet countries:
    aggreg <- c(178,19,10,223,68,227,14,112,114,137,108,213,120,80,234)
    reg_num <- paste(aggreg,collapse = ",")
    
    ALANG <- add_row(ALANG,'1' = paste0("Pauliuk EoL ",reg_name),Value = value,
                     'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
    
  } else if (r == 242) # Former Yugoslavia
  {
    # Codes of countries of former Yugoslavia:
    aggreg <- c(27,52,140,123,190,196)
    reg_num <- paste(aggreg,collapse = ",")
    
    ALANG <- add_row(ALANG,'1' = paste0("Pauliuk EoL ",reg_name),Value = value,
                     'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
     
  } else if (r == 56) # Czechoslovakia
  {
    # Codes of Czechia and SLovakia:
    aggreg <- c(55,195)
    reg_num <- paste(aggreg,collapse = ",")
    
    ALANG <- add_row(ALANG,'1' = paste0("Pauliuk EoL ",reg_name),Value = value,
                     'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
    
  } else if (r == 21) # Belgium-Luxembourg
  {
    
    aggreg <- c(20,121)
    reg_num <- paste(aggreg,collapse = ",")
    
    ALANG <- add_row(ALANG,'1' = paste0("Pauliuk EoL ",reg_name),Value = value,
                     'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
    
  } else if (r == 212) # Missing Taiwan
  {
    # Get root_code of region 
    reg_num <- as.character(r)
    
    ALANG <- add_row(ALANG,'1' = paste0("Pauliuk EoL ",reg_name),Value = value,
                     'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
    
  } else
  {
    reg_num <- as.character(r)
    ALANG <- add_row(ALANG,'1' = paste0("Pauliuk EoL ",reg_name),Value = value,
                     'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)  
  }
  
  
}

ALANG$`Row child` <- "3"
ALANG$`Row grandchild` <- "2"
ALANG$`Column child` <- "1"
ALANG$`Column grandchild` <- "64-65"

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
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))
  
print(paste0("datafeed_PIOLab_",datafeed_name," finished."))
  