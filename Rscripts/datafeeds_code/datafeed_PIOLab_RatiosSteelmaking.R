########################################################################
# This data feed contains ALANG commands for different ratios in iron and steelmaking 
#

datafeed_name <- "RatiosSteelmaking"

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

Row <- read.xlsx( paste0(path$Concordance,"/Additional/20200426_Additional_Root2Mother_Sec_Ind30Pro39v1.xlsx"), sheet = 2 )

Row <- rowSums( Row[1:nrow(root$flow), colnames(Row) %in% 37:38] )

Row_scrap <- paste0( which( Row != 0 ), collapse = "," )

Row_iron <- paste0( which( Row == 0 ), collapse = "," )


# Load ratios for blast furnace
set <- read.xlsx(xlsxFile = paste0(path$Settings,"/Base/IE_settings.xlsx"), sheet = 1 )

# Data frame for storing all commands that differ across ratios
com <- list("ind" = 1:2,
            "item" = c("ScrapRateBOF",
                       "ScrapRateEAF"),
            "ColGrandChild" = c("20","22,23,24"),
            "Value" = c(0.2,0.9),
            "Filename" = c(NA,NA),
            stringsAsFactors = FALSE)

com$Filename <- paste0("/",datafeed_name,"/",com$item,".csv")



# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))

# Set length of ALANG file
ALANG_new <- as.data.frame( matrix(NA,nrow = 2*nrow(root$region) ,ncol = ncol(ALANG) ) )
colnames(ALANG_new) <- colnames(ALANG)
ALANG <- ALANG_new
remove(ALANG_new)

# Add command for domestic Use table
ALANG$`1` <- paste("Ratio",rep(com$item[1:2],each = nrow(root$region) ), root$region$RootCountryAbbreviation)

ALANG$S.E. <- 0.1

ALANG$`Row parent` <- rep( root$region$Code, 2 )
ALANG$`Row child` <- 2
ALANG$`Row grandchild` <- paste0("[",Row_iron,";",Row_scrap,"]")

ALANG$`Column parent` <- rep( root$region$Code, 2 )
ALANG$`Column child` <- 1
ALANG$`Column grandchild` <- rep( com$ColGrandChild, each = nrow(root$region) )

ALANG$Coef1 <- paste0("c DATAPATH",rep(com$Filename,each = nrow(root$region) ) )


for(i in 1:2) Numbers2File( c( 1 - com$Value[[i]], com$Value[[i]] ) , paste0(path$Processed, com$Filename[[i]] ) )  


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
