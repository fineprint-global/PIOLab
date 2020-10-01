########################################################################
# This data feed contains ALANG commands for different ratios in iron and steelmaking 
#

datafeed_name <- "RatiosBlastFurnace"

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

source(paste0(path$Subroutines,"/Numbers2File.R"))  # Load fun. to write arrays to files

# Check if folder with processed data exists, in case delete and create empty one
path[["set"]] <- paste0(path$root,"ProcessedData/",datafeed_name)
if(dir.exists(path$set)) unlink(path$set,recursive = TRUE) 
dir.create(path$set)

# Set path to specific ALANG folder
path$ALANG <- paste0(path$ALANG,"/",datafeed_name)

# Check if folder with ALANG files exists and delte it 
if( dir.exists(path$ALANG) ) unlink( path$ALANG,recursive = TRUE ) 
dir.create( path$ALANG )


# Load RSE settings
RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)

# Load ratios for blast furnace
set <- read.xlsx(xlsxFile = paste0(path$Settings,"/Base/IE_settings.xlsx"), sheet = 1 )

# Data frame for storing all commands that differ across ratios
com <- list("ind" = 1:4,
            "item" = c("PigIronAndResiduals",
                       "ResidualAndResidual",
                       "InputAndProduct",
                       "InputAndInput"),
            "RowChild" = c(NA,NA,NA,NA),
            "RowGrandChild" = c(NA,NA,NA,NA),
            "ColChild" =  c(NA,NA,NA,NA),
            "ColGrandChild" = c(NA,NA,NA,NA),
            "Value" = list(),
            "Filename" = c(NA,NA,NA,NA),
            "orientation" = c(NA,NA,NA,NA),
            stringsAsFactors = FALSE)

com$Filename <- paste0("/",datafeed_name,"/",com$item,".csv")

### Write commands for sum of residuals per pig iron product
com$RowChild[1] <- "1"
com$RowGrandChild[1] <- "6"
com$ColChild[1] <- "2;3"
com$ColGrandChild[1] <- "1-e"

# Read ratios from file
com$Value[[1]] <- c(1,sum( set$value[set$item %in% c("TopGasPerPigIron","SlagPerPigIron")] ) )

# Set orientation of vector
com$orientation[1] <- "r"

### Write commands for sum of residuals per pig iron product
com$RowChild[2] <- "1"
com$RowGrandChild[2] <- "6"
com$ColChild[2] <- "3"
com$ColGrandChild[2] <- "7;8"

# Read ratios from file
com$Value[[2]] <- c(set$value[set$item == "SlagPerPigIron"], set$value[set$item == "TopGasPerPigIron"] )  

# Set orientation of vector
com$orientation[2] <- "r"


### Write commands for sum of inputs per pig iron product
com$RowChild[3] <- "2;3"
com$RowGrandChild[3] <- "1-e"
com$ColChild[3] <- "1"
com$ColGrandChild[3] <- "6"

# Read ratios from file
com$Value[[3]] <- c( set$value[set$item == "OrePerPigIron"],
                     sum( set$value[set$item %in% c("CokePerPigIron", "FluxPerPigIron", "AirPerPigIron")] ) )  

# Set orientation of vector
com$orientation[3] <- "c"


### Write commands for inputs per inputs
com$RowChild[4] <- "3"
com$RowGrandChild[4] <- "[2;3;5]"
com$ColChild[4] <- "1"
com$ColGrandChild[4] <- "6"

# Read ratios from file
com$Value[[4]] <- c( set$value[set$item == "FluxPerPigIron"],
                     set$value[set$item == "CokePerPigIron"],
                     set$value[set$item == "AirPerPigIron"] )  

# Set orientation of vector
com$orientation[4] <- "c"



select <- 4


# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))

# Set length of ALANG file
ALANG_new <- as.data.frame( matrix(NA,nrow = select*nrow(root$region) ,ncol = ncol(ALANG) ) )
colnames(ALANG_new) <- colnames(ALANG)
ALANG <- ALANG_new
remove(ALANG_new)

# Add command for domestic Use table
ALANG$`1` <- paste("Ratio",rep(com$item[1:select],each = nrow(root$region) ), root$region$RootCountryAbbreviation)

ALANG$S.E. <- RSE$Maximum

ALANG$`Row parent` <- rep( root$region$Code, select )
ALANG$`Row child` <- rep( com$RowChild[1:select], each = nrow(root$region) )
ALANG$`Row grandchild` <- rep( com$RowGrandChild[1:select], each = nrow(root$region) )

ALANG$`Column parent` <- rep( root$region$Code, select )
ALANG$`Column child` <- rep( com$ColChild[1:select], each = nrow(root$region) )
ALANG$`Column grandchild` <- rep( com$ColGrandChild[1:select], each = nrow(root$region) )


ALANG$Coef1 <- paste0(rep(com$orientation[1:select],each = nrow(root$region) ),
                     " DATAPATH",rep(com$Filename[1:select],each = nrow(root$region) ) )

# Adjust for colsums
ALANG$`Row parent`[ALANG$`Row child` == "2;3"] <- "1-e"

# Write vectors in different orientation dependign on the adress space
for(i in 1:select) 
{
  if(com$RowChild[i] == "1")
  {
    Numbers2File( t( com$Value[[i]] ) , paste0(path$Processed, com$Filename[[i]] ) )  
  }else
  {
    Numbers2File( com$Value[[i]] , paste0(path$Processed, com$Filename[[i]] ) )
  }
}


ALANG$Value <- "0"
ALANG$Incl <- "Y"
ALANG$Parts <- "1"
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
ALANG$Years <- "1"
ALANG$Margin <- "1"

ALANG$`#` <- as.character(1:nrow(ALANG))

ALANG[] <- lapply(ALANG,as.character)  # Convert all entries to character

# Call script that writes the ALANG file to the repsective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))
