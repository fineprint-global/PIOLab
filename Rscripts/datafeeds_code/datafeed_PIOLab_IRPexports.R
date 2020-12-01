################################################################################

datafeed_name <- "IRPexports"
print(paste0("datafeed_PIOLab_",datafeed_name," initiated."))

################################################################################
# Set library path depending on whether data feed runs on Uni Sydney server or local
if(Sys.info()[1] == "Linux")
{
  # Setting the R package library folder on Uni Sydney server
  .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")
  
  # Define location of root directory on the Uni Sydney server:
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"
  
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

path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name)

source(paste0(path$Subroutines,"/Numbers2File.R")) # Load fun. to write arrays to files


# Check if folder with processed data exists, in case delete and create empty one

if(dir.exists(path$df_Processed)) unlink(path$df_Processed,recursive = TRUE) 

dir.create(path$df_Processed)

# Loading raw data
rawdata <- read.csv(paste0(path$Raw,"/IRP/all_CCC_Exp_ResearchDB.csv"),stringsAsFactors=FALSE)
colnames(rawdata)[6:50] <- 1970:2014

for (year in 1970:2014) 
{
  data <- rawdata %>% filter(CCC_IEATrade_Name == "Iron ores") %>% select(Country,AlphaNumISO,as.character(year)) 
  
  data <- data[!is.na(data[,3]),]  # Remove NA
  
  data <- data %>% separate(AlphaNumISO, into = c('ISOCode', 'num'), sep = 3) %>% 
    select(-num)  
  
  colnames(data)[ncol(data)] <- "Quantity"
  data <- filter(data,Quantity > 0)
  
  # Remove Yugoslavia and USSR from data
  data <- data %>% filter(!ISOCode %in% c("YUG","SUN")) 
  
  # Look up root codes
  data <- left_join(data,root$region[,c("Code","RootCountryAbbreviation")],
                    by = c("ISOCode" = "RootCountryAbbreviation"),copy = FALSE) %>% 
    select(Code,Quantity)
  
  data <- data[!is.na(data$Code),] # Mayotte and Farour Isl. are not in root (tbc)
  
  n_reg <- nrow(root$region) # Read number of root regions
  
  data_new <- data.frame("code" = 1:n_reg,"RHS" = 0)
  
  data_new$RHS[data$Code] <- data$Quantity
  
  data <- data_new
  
  remove(data_new)
  
  # Set names and paths to data and concordances:
  
  filename <- paste0(path$Processed,"/",datafeed_name,"/",datafeed_name,"_RHS_",year,".csv")
  
  print(filename)
  
  Numbers2File( data$RHS, filename ) # Save S2R reg aggregator 
  
}

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

rm(list = ls()) # clear workspace

