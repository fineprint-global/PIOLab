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
ALANG_new <- as.data.frame( matrix(NA,nrow = reg_max ,ncol = ncol(ALANG) ) )
colnames(ALANG_new) <- colnames(ALANG)
ALANG <- ALANG_new
remove(ALANG_new)

# Prepare ALANG sheet for adding raw data:

ALANG$`Row parent` <- 1:reg_max
ALANG$`Column parent` <- "1:e"
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

ALANG$RHS$`1` <- paste( year,"BACI_RHS for exports of", root$region$Name )
ALANG$SE$`1` <- paste( year,"BACI_SE for exports of", root$region$Name )

# Remove "own-trade" entries:
#ALANG$RHS <- ALANG$RHS[ ALANG$RHS$`Row parent` != ALANG$RHS$`Column parent`, ]
#ALANG$SE <- ALANG$SE[ ALANG$SE$`Row parent` != ALANG$SE$`Column parent`, ]

# vec <- data.frame("reg" = rep(1:reg_max,each = pro_max),
#                   "pro" = rep(1:pro_max, reg_max),
#                   "RHS" = 0,
#                   "SE" = 0 )

mat <- matrix( NA, nrow = pro_max, ncol = reg_max)

### 2. Write commands by looping over export regions:

for( r in 1:reg_max ) 
{
  sel <- filter(data, From == r) %>% select(Product, To, Quantity, SE)  # Select import data of region r
  
  sel <- as.matrix(sel) # Make matrix for indexing
  
  # Create empty object for data storage:
  m <- list("RHS" = mat,
            "SE" = mat )
  
  # m$RHS[,r] <- NA  # Set RHS of products of region r to NA 
  # m$SE[,r] <- NA   # Set SE of products of region r to NA
  # m$RHS[256:266,] <- NA # Set RHS of manufacturing trade NA
  # m$SE[256:266,] <- NA  # Set SE of manufacturing trade NA
  
  # v$RHS[v$reg == r] <- NA  # Set RHS of products of region r to NA 
  # v$SE[v$reg == r] <- NA   # Set SE of products of region r to NA
  # v$RHS[v$pro %in% 256:266] <- NA # Set RHS of manufacturing trade NA
  # v$SE[v$pro %in% 256:266] <- NA # Set SE of manufacturing trade NA
  
  # index <- apply(sel[,1:2], 1, indexing)  # Read indices of root region-product pairs
  
  m$RHS[sel[,1:2]] <- sel[,3]  # Write RHS into matrix
  m$SE[sel[,1:2]] <- sel[,4]  # Write SE into matrix
  
  # v$RHS[index] <- sel$Quantity   # Write RHS into vector
  # v$SE[index] <- sel$SE          # Write SE into vector 
  
  # Set filenames:
  filename <- list("RHS" = paste0(path$set,"/",ALANG$RHS$`1`[r],".csv"),
                   "SE" = paste0(path$set,"/",ALANG$SE$`1`[r],".csv") )
    
  # Write values to file:
  Numbers2File( table = m$RHS, filename$RHS )
  Numbers2File( table = m$SE, filename$SE )
    
  # Write path into AISHA command:
  ALANG$RHS$Value[r] <- paste0("DATAPATH/",datafeed_name,"/",ALANG$RHS$`1`[r],".csv")
  ALANG$SE$S.E.[r] <- paste0("DATAPATH/",datafeed_name,"/",ALANG$SE$`1`[r],".csv")
  
}

### 3. Merge ALANG sheets and complete missing entries 

ALANG <- rbind(ALANG$RHS,ALANG$SE)  # Combine SE and RHS commands
ALANG$`#` <- 1:nrow(ALANG) # Add index
ALANG[] <- lapply(ALANG,as.character)  # Convert all entries to character

### 4. Call script that writes the ALANG file to the repsective folder in the root

source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

rm(list = ls()) # clear workspace
