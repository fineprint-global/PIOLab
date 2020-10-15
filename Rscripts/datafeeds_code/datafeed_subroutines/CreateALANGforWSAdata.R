################################################################################
# This is a subfunction for the data feeds 
# that process the World Steel Association data
# Author: hanspeter.wieland@wu.ac.at
# Date: 13.03.2020


source(paste0(path$Subroutines,"/Numbers2File.R"))  # Load fun. to write arrays to files
source(paste0(path$Subroutines,"/Read_ProductionWSA.R")) # Fun. to align data with root classification

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


# load correspondence to match WSA with 3digitISO region codes

concord <- read.xlsx(xlsxFile = paste0(path$Concordance,"/WSA/WSA_RegionConcordance.xlsx"),
                     sheet = 1)


### Load concordance for sourc2root industries and products

# Read settings and define path

set <- read.xlsx(xlsxFile = paste0(path$Settings,"/Base/IE_settings.xlsx"),sheet = 2)

path_sel <- list("flow" = paste0(path$Concordance,"/WSA/","WSA_Source2Root_Product.csv"),
                 "process" = paste0(path$Concordance,"/WSA/","WSA_Source2Root_Industry.csv") )

# Import concordances for industries and products

ConcoInd <- read.table(path_sel$process,sep = ",")
ConcoPro <- read.table(path_sel$flow,sep = ",")

# Create two concordances, for zeros and ones

ConcoInd <- list("One" = ConcoInd[Settings$id,],
                 "Zero" = ConcoInd[Settings$id,]
                 )

ConcoInd$Zero <- -1 * (ConcoInd$Zero -1)  # Binary invertion of conco for zeros 

# Transform into ALANG comand

ConcoInd$One <- paste( which(ConcoInd$One == 1) , collapse = "," )
ConcoInd$Zero <- paste( which(ConcoInd$Zero == 1) , collapse = "," )
ConcoPro <- paste( which(ConcoPro[Settings$id,] == 1) , collapse = "," )


# Read RHS values and align with root classification

data <- Read_ProductionWSA(path,year,pages,out,concord)

n_reg <- nrow(root$region) # Number of regions in root

RHS <- matrix(NA,nrow = 1,ncol = n_reg) # vector for RHS values all root regions

RHS[1,data$Code] <- data$Quantity  # Write right-hand-side in vector


# Set names and paths to data and concordances:

filename <- list("RHS" = paste0("/",datafeed_name,"/",datafeed_name,"_RHS_",year,".csv") )


Numbers2File( t( RHS ), paste0(path$Processed, filename$RHS)) # Write data to folder 



source(paste0(path$Subroutines,"/makeALANGheadline.R")) # Create ALANG header

ALANG <- add_row(ALANG,'1' = paste(datafeed_name,year)) # Create entry

# Write command for elements that are not zero:

ALANG$Value <- paste0("DATAPATH",filename$RHS)
ALANG$S.E. <- paste0("E MX",RSE$Maximum,"; MN",RSE$Minimum,";")
ALANG$`Row grandchild` <- ConcoInd$One
ALANG$Coef1 <- RSE$Coef

# Write command for elements that are zero:

ALANG <- add_row(ALANG,'1' = paste(datafeed_name,year, "Zero elements")) # Create entry

ALANG$Value[2] <- 0
ALANG$S.E.[2] <- 0
ALANG$`Row grandchild`[2] <- ConcoInd$Zero
ALANG$Coef1[2] <- 1

# Write other variables
ALANG$`Row parent` <- "1:e"
ALANG$`Column parent` <- "1:e~3"
ALANG$`Row child` <- 1
ALANG$`Column child` <- 2
ALANG$`Column grandchild` <- ConcoPro

ALANG$`#` <- 1:nrow(ALANG)
ALANG$Incl <- "Y"
ALANG$Parts <- 1

ALANG$Years <- 1
ALANG$Margin <- 1

ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""

ALANG[] <- lapply(ALANG,as.character)  # Convert all entries to character

# Call script that writes the ALANG file to the respective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))


             