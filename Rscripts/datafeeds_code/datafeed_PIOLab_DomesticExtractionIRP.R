################################################################################
# Based on the IRP MFA database, the data feed calculates the 
# Domestic Material Consumption (DMC) Indicator for all base regions. 
# DMC is used as a constraint for the PIOT.
#

datafeed_PIOLab_DomesticExtractionIRP <- function(path,year)
{
  # Set library path when running on suphys server
  if(Sys.info()[1] == "Linux") .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")
  
  print("datafeed_PIOLab_balancing initiated.")
  year <- 2008
  
  library(dplyr)
  library(tidyr)
  # Because the data.table package is not available for the R version currently running on the USYD server
  # use the forerunner version of it, that is the reshape2 package, to use its functionalities
  #library(data.table)
  library(reshape2)
  library(openxlsx)
  library(stringr)
  library(R.matlab)
  
  # Define location for root directory
  root_folder <- "C:/Users/hwieland/Github workspace/PIOLab/"
  # when working on the IELab server root folder will be set to the following
  if(!dir.exists(root_folder)) root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"
  # Note for HP: Insert code to read root folder from HANDLER variable here
  
  # Read current export (aka working or mother) directory  
  mother <- readMat(paste0(root_folder,"IEfeeds_code/WorkingDirectory4R.mat"))
  mother <- c(mother$out)
  
  path <- list("Subroutines" = paste0(root_folder,"IEfeeds_code/Rscript/StandardPIOT_IE_subroutines"),
               "Raw" = paste0(root_folder,"RawDataRepository"),
               "Processed" = paste0(root_folder,"ProcessedData/StandardPIOT"),
               "Concordance" = paste0(root_folder,"ConcordanceLibrary"),
               "root" = root_folder,
               "mother" = mother)
  
  base_regions <- read.xlsx(paste0(path$Concordance,"/StandardPIOT_RootClassification.xlsx"),sheet = 9)
  n_reg <- nrow(base_regions)  
  
  # Create empty ALANG table with header
  source(paste0(path$Subroutines,"/makeALANGheadline.R"))
  ALANG <- makeALANGheadline()
  # Extend table with additional columns
  ALANG <- ALANG[,c(1:19,11:19)]
  
  
  
  
}


