########################################################################
# 
# This data feed formulates ALANG commands so that the sum of the primary inputs 
# (inputs from nature and EoL scrap) equals the sum of the final use (incl. flows to the environment) 

datafeed_name <- "PrimaryInputEqualsFinalUse"

print(paste0("datafeed_PIOLab_",datafeed_name," initiated."))

library(tidyr)

# Determine loaction of root folder
################################################################################

# Set library path depending on whether data feed runs on Uni Sydney server or local
if(Sys.info()[1] == "Linux")
{
  # Setting the R package library folder on Uni Sydney server
  .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")
  
  # Define location of root directory on the Uni Sydney server:
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"
  
} else{
  
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

# Set path to processed data and ALANG folder 
path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name)  # Add datafeed specific path for output data
path$ALANG <- paste0(path$ALANG,"/",datafeed_name)
path["df_Subroutines"] <- paste0(path$Rscripts,"/datafeeds_code/datafeed_subroutines/") 

# Call script to clear ALANG and processed data folders of the present data feed
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/ClearFolders.R"))

# Remove processed data folder cause not needed for this feed
unlink(path$df_Processed, recursive = TRUE)  


for(year in 1970:2014)
{
  # Read global extraction and EOL values
  source(paste0(path$Subroutines,"/Read_ExtractionIRP.R"))
  DE <- sum(data$Quantity)
  
  # For now just this (assume per ton of iron ore 1.3 ton of flux, air and coke are required)
  # Set SE
  RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)
  
  tot <-  DE + DE*1.3 
  SE <- as.character(round(tot*RSE$Minimum))
  
  # Create empty ALANG table with header
  source(paste0(path$Subroutines,"/makeALANGheadline.R"))
  
  # Extend table with additional columns
  ALANG <- ALANG[,c(1:19,11:19)]
  
  ALANG <- add_row(ALANG,'1' = "Sum of primary inputs equals sum of final use")
  
  
  # Part 1: Sum over primary inputs 
  ALANG$Coef1 <- "1"
  ALANG$`Row parent` <- "1-e"
  ALANG$`Row child` <- "3" 
  ALANG$`Row grandchild` <- "1-e"
  ALANG$`Column parent` <- "1-e"
  ALANG$`Column child` <- "1"
  ALANG$`Column grandchild` <- "1-e"
  
  # Part 2: Sum over final use
  ALANG$Coef1.1 <- "-1"
  ALANG$`Row parent.1` <- "1-e"
  ALANG$`Row child.1` <- "1-2"                 
  ALANG$`Row grandchild.1` <- "1-e"                 
  ALANG$`Column parent.1` <- "1-e"
  ALANG$`Column child.1` <- "3"                 
  ALANG$`Column grandchild.1` <- "1-e"
                   
  # Add other variables
  ALANG$`#` <- "1"
  ALANG$Incl <- "Y"
  ALANG$Parts <- "2"
  ALANG$Value <- "0"
  ALANG$S.E. <- SE
  ALANG$Years <- ALANG$Years.1 <- "1"
  ALANG$Margin <- ALANG$Margin.1 <- "1"
  ALANG$`Pre-map` <- ""
  ALANG$`Post-map` <- ""
  ALANG$`Pre-Map` <- ""
  ALANG$`Post-Map` <- ""
  
  # Call script that writes the ALANG file to the repsective folder in the root
  source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))
}
  
print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

