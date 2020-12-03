################################################################################
# This is a subfunction for the data feeds 
# that process the World Steel Association data
# Author: hanspeter.wieland@wu.ac.at
# Date: 13.03.2020

# Remove processed data folder for WSA feeds, because values are stored directly in ALANG sheet
if( dir.exists(path$df_Processed) ) unlink(path$df_Processed, recursive = TRUE)

source(paste0(path$Subroutines,"/Numbers2File.R"))  # Load fun. to write arrays to files
source(paste0(path$Subroutines,"/Read_ProductionWSA.R")) # Fun. to align data with root classification

# Call function for estimating standard errors
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/standev.R"))

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

data <- standev(data, RSE$Minimum, RSE$Maximum)  # Add standard errors

source(paste0(path$Subroutines,"/makeALANGheadline.R")) # Create ALANG header

ALANG <- ALANG[1:nrow(data),]  # Create entries for non-zero
ALANG_zero <- ALANG[1:nrow(data),]  # Create entries for zeros


# Write command for elements that are not zero:
ALANG$`1` <- paste(datafeed_name,year,root$region$RootCountryAbbreviation[data$Code]) 
ALANG$Value <- round( data$Quantity, digits = 2)
ALANG$S.E. <- round( data$SD, digits = 2)
ALANG$`Row grandchild` <- ConcoInd$One
ALANG$Coef1 <- RSE$Coef

ALANG$`Row parent` <- data$Code
ALANG$`Column parent` <- data$Code

# Write command for elements that are zero:

ALANG_zero$`1` <- paste(datafeed_name,year, "Zero elements", root$region$RootCountryAbbreviation[data$Code] ) 

ALANG_zero$Value <- 0
ALANG_zero$S.E. <- 0
ALANG_zero$`Row grandchild` <- ConcoInd$Zero
ALANG_zero$Coef1 <- 1

ALANG_zero$`Row parent` <- data$Code
ALANG_zero$`Column parent` <- data$Code

# Merge zero and non-zero entries into one array
ALANG <- rbind(ALANG, ALANG_zero)
remove(ALANG_zero)

# Write other variables
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


             