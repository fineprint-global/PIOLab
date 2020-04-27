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

# Read trade pairs

index <- select(data,From,To,Quantity) %>% group_by(From,To) %>% 
  summarise(Q = sum(Quantity)) %>% ungroup(From,To) %>% select(From,To)

### 1. Create ALANG sheets

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))

# Set length of ALANG file
ALANG_new <- as.data.frame( matrix(NA,nrow = nrow(index) ,ncol = ncol(ALANG) ) )
colnames(ALANG_new) <- colnames(ALANG)
ALANG <- ALANG_new
remove(ALANG_new)

# Prepare ALANG sheet for adding raw data:

ALANG$`Row parent` <- index$From
ALANG$`Column parent` <- index$To
ALANG$`Row child` <- 2
ALANG$`Row grandchild` <- "1:e t2 CONCPATH/Root2Root_Pro_Concordance.csv"
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

ALANG$RHS$`1` <- ALANG$SE$`1` <- paste0( year," BACI RHS_", root$region$Name[ALANG$RHS$`Row parent`],
                                         " to ", root$region$Name[ALANG$RHS$`Column parent`] )

# Write datapath into AISHA command:
ALANG$RHS$Value <- paste0("DATAPATH/",datafeed_name,"/",ALANG$RHS$`1`,".csv")
ALANG$SE$S.E. <- paste0("DATAPATH/",datafeed_name,"/",ALANG$SE$`1`,".csv")

vec <- data.frame("pro" = 1:pro_max,
                  "RHS" = 0,
                  "SE" = 0)

vec[256:266,2:3] <- NA

### 2. Write commands by looping over export regions:

for( i in 1:nrow(index) ) 
{
  sel <- filter(data, From == index$From[i], To == index$To[i] ) # Select exporter
  
  v <- vec  # Create empty object for data storage
    
  v$RHS[sel$Product] <- sel$Quantity
  v$SE[sel$Product] <- sel$SE
    
  # Set filenames:
  filename <- list("RHS" = paste0(path$set,"/",ALANG$RHS$`1`[i],".csv"),
                   "SE" = paste0(path$set,"/",ALANG$SE$`1`[i],".csv") )
 
  # Write values as row vectors to file:
  Numbers2File( table = t(v$RHS), filename$RHS )
  Numbers2File( table = t(v$SE), filename$SE )
  
}
  

### 3. Merge ALANG sheets and complete missing entries 

ALANG <- rbind(ALANG$RHS,ALANG$SE)     # Combine SE and RHS commands
ALANG$`#` <- 1:nrow(ALANG)             # Add index
ALANG[] <- lapply(ALANG,as.character)  # Convert all entries to character

### 4. Call script that writes the ALANG file to the repsective folder in the root

source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

rm(list = ls()) # clear workspace
