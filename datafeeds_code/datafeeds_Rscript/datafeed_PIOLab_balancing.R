####################################
# datafeed_PIOLab_balancing
#
# This data feed formulates the ALANG commands for the balancing condition
# for both industries and products
#
#
#
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
  
path <- list("Subroutines" = paste0(root_folder,"IEfeeds_code/Rscript/StandardPIOT_IE_subroutines"),
               "Raw" = paste0(root_folder,"RawDataRepository"),
               "Processed" = paste0(root_folder,"ProcessedData/StandardPIOT"),
               "Concordance" = paste0(root_folder,"ConcordanceLibrary"),
               "root" = root_folder)

base_regions <- read.xlsx(paste0(path$Concordance,"/StandardPIOT_RootClassification.xlsx"),sheet = 9)
n_reg <- nrow(base_regions)  

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))
ALANG <- makeALANGheadline()
# Extend table with additional columns
ALANG <- ALANG[,c(1:19,11:19)]

for(r in 1:n_reg)  
{
  # Balancing industries
  ALANG <- add_row(ALANG,'1' = paste0("Balancing industries of ",base_regions$BaseRegionName[r]),
                   Coef1 = 1,'Row parent' = r,'Row child' = 1,'Row grandchild' = "1:e",
                   'Column parent' = "1-e",'Column child' = "1-e",'Column grandchild' = "1-e",
                   'Coef1.1' = -1,'Row parent.1' = "1-e",'Row child.1' = "1-e",'Row grandchild.1' = "1-e",
                   'Column parent.1' = r,'Column child.1' = 1,'Column grandchild.1' = "1:e")
  
  # Balancing products
  ALANG <- add_row(ALANG,'1' = paste0("Balancing products of ",base_regions$BaseRegionName[r]),
                   Coef1 = 1,'Row parent' = r,'Row child' = 2,'Row grandchild' = "1:e",
                   'Column parent' = "1-e",'Column child' = "1-e",'Column grandchild' = "1-e",
                   'Coef1.1' = -1,'Row parent.1' = "1-e",'Row child.1' = "1-e",'Row grandchild.1' = "1-e",
                   'Column parent.1' = r,'Column child.1' = 2,'Column grandchild.1' = "1:e")
}

# Add other variables
ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$Incl <- "Y"
ALANG$Parts <- 2
ALANG$Value <- 0
ALANG$S.E. <- 0
ALANG$Years <- ALANG$Years.1 <- 1
ALANG$Margin <- ALANG$Margin.1 <- 1
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
  
# Check if available and creat folder for ALANG storage
path_set <- paste0(path$root,"ALANGfiles/Balancing")
if(dir.exists(path_set)) unlink(path_set,recursive = TRUE) 
# Create new folder  
dir.create(path_set)
  
# Write data frame with ALANG commands as tab-delimited txt-file to root and working directory (mother)
# Note HP: This is probably not the normal procedure, meaning no IE ALANG's in the root
filename <-  paste0(path_set,"/",gsub("-","",Sys.Date()),
                      "_PIOLab_Balancing_000_Constraints-",year,"_000_RoWexcluded.txt")
  
write.table(ALANG,file = filename,row.names = FALSE, quote = F,sep = "\t")
  
# Check if the mother directory really exists
if(Sys.info()[1] == "Linux")
{ 
  # Read current export (aka working or mother) directory  
  mother <- readMat(paste0(root_folder,"IEfeeds_code/WorkingDirectory4R.mat"))
  mother <- c(mother$out)
  
  filename <-  paste0(path$mother,gsub("-","",Sys.Date()),
                      "_PIOLab_Balancing_000_Constraints-",year,"_000_RoWexcluded.txt")
    
  write.table(ALANG,file = filename,row.names = FALSE, quote = F,sep = "\t") 
  
}
  
print("datafeed_PIOLab_balancing finished.")

