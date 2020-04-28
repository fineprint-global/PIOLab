################################################################################
# This is a subfunction for the data feeds 
# that process the World Steel Association data
# Author: hanspeter.wieland@wu.ac.at
# Date: 13.03.2020


# Initializing R script (R packages, folders etc.):
source(paste0(root_folder,"Rscripts/Subroutines/InitializationR.R"))
path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name) # Add path for processed data

print(paste0("datafeed_PIOLab_",datafeed_name," initiated."))

source(paste0(path$Subroutines,"/Numbers2File.R"))  # Load fun. to write arrays to files
source(paste0(path$Subroutines,"/Read_ProductionWSA.R")) # Fun. to align data with root classification


# Check if folder with processed data exists and in case delete:
if(dir.exists(path$df_Processed)) unlink(path$df_Processed,recursive = TRUE) 
dir.create(path$df_Processed) # Create new (empty) folder
  

# Get relative standard error for smallest and largest values in the data set:
RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)

  
# Read settings (e.g. page in the specific yearbook) for WSA data feeds:
Settings <- read.xlsx(paste0(path$Settings,"/datafeeds_settings/WSA_settings.xlsx"))
Settings <- Settings[Settings$FeedName == datafeed_name,] # Select which feed
  

# Load Yearbook R-object depending on the selected year:

if(year >= 2008)
{
  
  load(file = paste0(path$Raw,"/WSA/Worldsteeldata 2018.RData"))
  
  years <- 2008:2017  # Years covered by the yearbook
  
  pages <- Settings$Yearbook_2018   # Read page number
  
}else
{
  
  load(file = paste0(path$Raw,"/WSA/Worldsteeldata 2005.RData"))
  
  years <- 1995:2004
  
}

# When more than one number in string, split in 2:

if(nchar(pages) > 2) pages <- unlist( strsplit( pages ,",") ) 

pages <- as.integer(pages) # Transform to numeric 


# load correspondence to match WSA with 3digitISO codes

concord <- read.xlsx(xlsxFile = paste0(path$Concordance,"/WSA/WSA_RegionConcordance.xlsx"),
                     sheet = 1)


# Load concordance for sourc2root industries and products

set <- read.xlsx(xlsxFile = paste0(path$Settings,"/Base/IE_settings.xlsx"),sheet = 2)

path_sel <- list("flow" = paste0(path$Concordance,"/WSA/",
                                 set$date[set$aggregator == "sector"],"_WSA_Source2Root_Product.csv"),
                 "process" = paste0(path$Concordance,"/WSA/",
                                    set$date[set$aggregator == "sector"],"_WSA_Source2Root_Industry.csv")
                 )

ConcoInd <- read.table(path_sel$process,sep = ",")

ConcoInd <- list("One" = paste( which(ConcoInd[Settings$id,] == 1) , collapse = "," ),
                 "Zero" = paste( which(ConcoInd[Settings$id,] == 0) , collapse = "," ) )
                 


ConcoPro <- read.table(path_sel$flow,sep = ",")

ConcoPro <- paste( which(ConcoPro[Settings$id,] == 1) , collapse = "," )


# Read values and align with root classification

data <- Read_ProductionWSA(path,year,pages,out,concord)

n_reg <- nrow(root$region) # Number of regions in root

RHS <- matrix(0,nrow = 1,ncol = n_reg) # vector for RHS values all root regions

RHS[1,data$Code] <- data$Quantity  # Write right-hand-side in vector

# ConcoReg <- diag(nrow(root$region)) # Create pseudo aggregator


# Set names and paths to data and concordances:

filename <- list("RHS" = paste0("/",datafeed_name,"/",datafeed_name,"_RHS_",year,".csv"),
                 "ConcoReg" = "/Root2Root_Reg_Concordance.csv",
                 "ConcoInd" = paste0("/WSA_Source2Root_Industry_",datafeed_name,".csv"),
                 "ConcoPro" = paste0("/WSA_Source2Root_Product_",datafeed_name,".csv"))

# Numbers2File( ConcoReg, paste0(path$Concordance,filename$ConcoReg)) # Save region aggregator 

Numbers2File( RHS, paste0(path$Processed, filename$RHS)) # Write data to folder 



source(paste0(path$Subroutines,"/makeALANGheadline.R")) # Create ALANG header

ALANG <- add_row(ALANG,'1' = paste(datafeed_name,year)) # Create entry

# Write command for elements that are not zero:
                 
ALANG$Value <- paste0("DATAPATH",filename$RHS)
ALANG$S.E. <- paste0("E MX",RSE$Maximum,"; MN",RSE$Minimum,";")
ALANG$`Row grandchild` <- ConcoInd$One
ALANG$`Row parent` <- "1-e"
ALANG$`Column parent` <- paste0("1:e t2 CONCPATH",filename$ConcoReg)


# Write command for elements that are zero:

ALANG <- add_row(ALANG,'1' = paste(datafeed_name,year, "Zero elements")) # Create entry

ALANG$Value[2] <- paste0("0")
ALANG$S.E.[2] <- paste0("0")
ALANG$`Row grandchild`[2] <- ConcoInd$Zero
ALANG$`Row parent`[2] <- "1:e"
ALANG$`Column parent`[2] <- paste0("1:e~3")

# Write other variables


ALANG$`Row child` <- "1"
ALANG$`Column child` <- "2"
ALANG$`Column grandchild` <- ConcoPro


ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$Incl <- "Y"
ALANG$Parts <- "1"

ALANG$Years <- as.character(year-2007)
ALANG$Margin <- "1"
ALANG$Coef1 <- "1"

ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
  
# Call script that writes the ALANG file to the respective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))
