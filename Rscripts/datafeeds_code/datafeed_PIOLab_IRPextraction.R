################################################################################

datafeed_name <- "IRPextraction"
print(paste0("datafeed_PIOLab_",datafeed_name," initiated."))


################################################################################
# Set library path depending on whether data feed runs on Uni Sydney or WU Vienna server or local
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

path$ALANG <- paste0(path$ALANG,"/",datafeed_name)
path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name)  # Add datafeed specific path for output data
path["df_conco"] <- paste0(path$Concordance,"/IRP/")  # Add datafeed specific path for S2R concordance

# Call script to clear ALANG and processed data folders of the present data feed
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/ClearFolders.R"))

# Load function to write arrays to files
source( paste0(path$Subroutines,"/Numbers2File.R") )

# Call function for estimating standard errors
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/standev.R"))

# Read date of concordance for sourc2root industries and products
set <- read.xlsx(xlsxFile = paste0(path$Settings,"/datafeeds_settings/IRP_settings.xlsx"),sheet = 1)

Conco <- list("process" = read.xlsx(xlsxFile = paste0(path$df_conco,set$date,"_IRP_Extraction_SecConc.xlsx"),sheet = 1),
              "flow" =  read.xlsx(xlsxFile = paste0(path$df_conco,set$date,"_IRP_Extraction_SecConc.xlsx"),sheet = 2) )

# read upper and lower error bounds (RSE = relative standard error) from settings file
RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)

n_reg <- nrow(root$region) # Number of root regions



for(year in 1970:2014)
{
  # Loading raw data
  source(paste0(path$Subroutines,"/Read_ExtractionIRP.R"))
  
  data <- standev(data, RSE$Minimum, RSE$Maximum)  # Add standard errors to RHS
  
  # Create empty ALANG table with header
  source(paste0(path$Subroutines,"/makeALANGheadline.R"))
  
  ALANG <- ALANG[1:nrow(data),]
  
  # Write year-specific commands
  ALANG$`1` <- paste(datafeed_name,year, root$region$RootCountryAbbreviation[data$Code] )
  ALANG$Value <- round( data$Quantity, digits = 2)
  ALANG$S.E. <- round( data$SD, digits = 2)
  
  ALANG$Years <- 1
  
  # Add all other non-year-specific commands
  ALANG$`#` <- 1:nrow(ALANG)
  ALANG$Coef1 <- 1
  ALANG$Margin <- 1
  
  ALANG$`Row parent` <- data$Code
  ALANG$`Row child` <- 3
  ALANG$`Row grandchild` <- root$input$Code[ root$input$Name == "Hematite & Magnetite" ]
  
  ALANG$`Column parent` <- data$Code
  ALANG$`Column child` <- 1
  ALANG$`Column grandchild` <- paste( Conco$process$Code[Conco$process$binary == 1] , collapse = "," )
  
  ALANG$Incl <- "Y"
  ALANG$Parts <- "1"
  ALANG$`Pre-map` <- ""
  ALANG$`Post-map` <- ""
  ALANG$`Pre-Map` <- ""
  ALANG$`Post-Map` <- ""
  
  ALANG[] <- lapply(ALANG,as.character)  # Convert all entries to character
  
  # Call script that writes the ALANG file to the repsective folder in the root
  source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))
}

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

