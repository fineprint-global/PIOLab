################################################################################
#
datafeed_name <- "BACI"
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
path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name)

source(paste0(path$Subroutines,"/Numbers2File.R"))  # Load fun. to write arrays to files

# Check if folder with processed data exists, in case delete and create empty one
path[["set"]] <- paste0(path$root,"ProcessedData/",datafeed_name)
if(dir.exists(path$set)) unlink(path$set,recursive = TRUE) 
dir.create(path$set)

source(paste0(path$Subroutines,"/Load_BACI.R")) # Loading raw data

# Load function to calculate standard errors 

source(paste0(path$Subroutines,"/SE_LogRegression.R"))

RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)
data <- SE_LogRegression(data,RSE$Minimum,RSE$Maximum)
data <- select(data,-value)

# Set variables
reg_max <- nrow(root$region)
pro_max <- nrow(root$flow)
n_yea <- "1"
n_she <- "1"


### 1. Create ALANG sheets

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))

# Set length of ALANG file
ALANG_new <- as.data.frame( matrix(NA,nrow = (reg_max*reg_max) ,ncol = ncol(ALANG) ) )
colnames(ALANG_new) <- colnames(ALANG)
ALANG <- ALANG_new
remove(ALANG_new)

# Prepare ALANG sheet for adding raw data:

ALANG$`Row parent` <- rep( 1:reg_max, reg_max )
ALANG$`Column parent` <- rep( 1:reg_max, each = reg_max )

# Add all variables that are identical for all trade pairs
ALANG$`Row child` <- 2
ALANG$`Row grandchild` <- "1:e"
ALANG$`Column child` <- 1
ALANG$`Column grandchild` <- "1-e"

ALANG$Coef1 <- 1
ALANG$Incl <- "Y"

ALANG$Parts <- 1
ALANG$Years <- n_yea
ALANG$Margin <- n_she

ALANG$`Pre-map` <- ""
ALANG$`Post-map` <-  ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""

# Separate commands for SE and RHS:
ALANG <- list("RHS" = ALANG, "SE" = ALANG )
ALANG$RHS$S.E. <- ALANG$SE$Value <- ""

ALANG$RHS$`1` <- paste( year,"BACI_RHS for", rep(root$region$Name, reg_max),"to", rep(root$region$Name, each = reg_max) )
ALANG$SE$`1` <- paste( year,"BACI_SE for", rep(root$region$Name, reg_max),"to", rep(root$region$Name, each = reg_max) )

# Remove "own-trade" entries:
ALANG$RHS <- ALANG$RHS[ ALANG$RHS$`Row parent` != ALANG$RHS$`Column parent`, ]
ALANG$SE <- ALANG$SE[ ALANG$SE$`Row parent` != ALANG$SE$`Column parent`, ]

### 2. Write commands by looping over trade pairs:

for( i in 1:nrow(ALANG$RHS) ) 
{
  # Select data for trade pair
  data_sel <- filter(data,
                     From == ALANG$RHS$`Row parent`[i],
                     To == ALANG$RHS$`Column parent`[i]) 
  
  if( nrow(data_sel) > 0 )
  {
    # Create empty matrix for writing values to file in folder:
    mat <- as.data.frame( matrix(0,nrow = pro_max,ncol = 2) )  
    colnames(mat) <- c("RHS","SE")
    
    # Write raw data and standard errors: 
    mat$RHS[ data_sel$Product ] <- data_sel$Quantity
    mat$SE[ data_sel$Product ] <- data_sel$SE
    
    # Set filenames:
    filename <- list("RHS" = paste0(path$set,"/",ALANG$RHS$`1`[i],".csv"),
                     "SE" = paste0(path$set,"/",ALANG$SE$`1`[i],".csv") )
    
    # Write values to file:
    Numbers2File(table = mat$RHS, filename$RHS )
    Numbers2File(table = mat$SE, filename$SE )
    
    # Write path into AISHA command:
    ALANG$RHS$Value[i] <- paste0("DATAPATH/",datafeed_name,"/",ALANG$RHS$`1`[i],".csv")
    ALANG$SE$S.E.[i] <- paste0("DATAPATH/",datafeed_name,"/",ALANG$SE$`1`[i],".csv")
  }
}

### 3. Supply missing trade pairs with vectors containing only zeros: 



mat <- matrix(0,nrow = pro_max,ncol = 1) # Vector with zeros

filename <- paste0(path$set,"/Zero values.csv") # Set filenames

Numbers2File(table = mat, filename ) # Write values to file:

# Write path into AISHA command:
ALANG$RHS$Value[is.na(ALANG$RHS$Value)] <- paste0("DATAPATH/",datafeed_name,"/Zero values.csv")
ALANG$SE$S.E.[is.na(ALANG$SE$S.E.)] <- paste0("DATAPATH/",datafeed_name,"/Zero values.csv")

ALANG <- rbind(ALANG$RHS,ALANG$SE)  # Combine SE and RHS commands
ALANG$`#` <- 1:nrow(ALANG) # Add index
ALANG[] <- lapply(ALANG,as.character)  # Convert all entries to character


### 4. Call script that writes the ALANG file to the repsective folder in the root

source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

rm(list = ls()) # clear workspace
