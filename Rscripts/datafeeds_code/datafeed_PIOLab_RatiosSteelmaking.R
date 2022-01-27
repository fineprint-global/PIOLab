########################################################################
# This data feed contains ALANG commands for different ratios in iron and steelmaking 
#

datafeed_name <- "RatiosSteelmaking"

# Determine loaction of root folder
################################################################################
# Set library path depending on whether data feed runs on Uni Sydney server or local
if(Sys.info()[1] == "Linux")
{
  # Define location of root directory on the Uni Sydney server:
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"
  
  if(dir.exists(root_folder))
  {
    # Setting the R package library folder on Uni Sydney server
    .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")  
  } else{
    # Define location of root directory on the WU Vienna server:
    root_folder <- "/data/WULab/Roots/PIOLab/"
  }
  
  
} else{
  
  library(tidyr)
  # Locating folder where the present script is stored locally to derive the root folder 
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  
  if(length(this_file)==0) this_file <- rstudioapi::getSourceEditorContext()$path
  
  root_folder <- substr(dirname(this_file),1,nchar(dirname(this_file))-23)
  remove(this_file)
}
################################################################################

# Initializing R script (load R packages and set paths to folders etc.)
source(paste0(root_folder,"Rscripts/Subroutines/InitializationR.R"))

source(paste0(path$Subroutines,"/Numbers2File.R"))  # Load fun. to write arrays to files

path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name)  # Add datafeed specific path for output data
path["df_Subroutines"] <- paste0(path$Rscripts,"/datafeeds_code/datafeed_subroutines/")
path$ALANG <- paste0(path$ALANG,"/",datafeed_name)  # Set path to specific ALANG folder

# Call script to clear ALANG and processed data folders of the present data feed
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/ClearFolders.R"))


### Create concordance and store in folder

Conco <- matrix(0, nrow = 2, ncol = nrow(root$flow))

# Read flow indices of scrap  
Scrap <- read.xlsx( paste0(path$Concordance,"/Additional/20200426_Additional_Root2Mother_Sec_Ind30Pro39v1.xlsx"), sheet = 2 )
Scrap <- rowSums( Scrap[1:nrow(root$flow), colnames(Scrap) %in% 37:38] )
Conco[2,] <- Scrap 

# Add other indices
Other <- Scrap
Other[c(1:7,30:nrow(root$flow))] <- 1
Conco[1,which( Other == 0)] <- 1

# Set path and create folder for concordance
path$Concordance <- paste0( path$Concordance,"/",datafeed_name,"/")


Numbers2File(Conco, paste0(path$Concordance,"Source2Root_Flows.csv") )

# Row_scrap <- paste0( which( Row != 0 ), collapse = "," )
# Row_iron <- "2,4,5,6"

# Load ratios for blast furnace
set <- read.xlsx(xlsxFile = paste0(path$Settings,"/Base/IE_settings.xlsx"), sheet = 1 )

# Data frame for storing all commands that differ across ratios
com <- list("ind" = 1:2,
            "item" = c("ScrapRateBOF",
                       "ScrapRateEAF"),
            "ColGrandChild" = c("20","[22,23,24]"),
            "Value" = c(0.2,0.9),
            "Filename" = c(NA,NA),
            stringsAsFactors = FALSE)

com$Filename <- paste0("/",com$item,".csv")




for(year in 1970:2017)
{
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
  
  ALANG$`Row parent` <- "1-221"
  ALANG$`Row child` <- 2
  # ALANG$`Row grandchild` <- paste0("[",Row_iron,"];[",Row_scrap,"]")
  ALANG$`Row grandchild` <- paste0("1:e a CONCPATH/",datafeed_name,"/Source2Root_Flows.csv")
  
  ALANG$`Column parent` <- rep( root$region$Code, 2 )
  ALANG$`Column child` <- 1
  ALANG$`Column grandchild` <- rep( com$ColGrandChild, each = nrow(root$region) )
  
  ALANG$Coef1 <- paste0("c DATAPATH/",datafeed_name,rep(com$Filename,each = nrow(root$region) ) )
  
  
  for(i in 1:2) Numbers2File( c( 1 - com$Value[[i]], com$Value[[i]] ) , paste0(path$df_Processed, com$Filename[[i]] ) )  
  
  
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

}

