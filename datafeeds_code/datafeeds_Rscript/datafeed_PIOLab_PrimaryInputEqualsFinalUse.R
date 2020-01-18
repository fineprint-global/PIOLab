########################################################################
# datafeed_PIOLab_PrimaryInputEqualsFinalUse
#
#
# This data feed formulates ALANG commands so that the sum of the primary inputs 
# (inputs from nature and EoL scrap) equals the sum of the final use (incl. flows to the environment) 

# Set library path when running on suphys server
if(Sys.info()[1] == "Linux") .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")

print("datafeed_PIOLab_PrimaryInputEqualsFinalUse initiated.")
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

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))
ALANG <- makeALANGheadline()
# Extend table with additional columns
ALANG <- ALANG[,c(1:19,11:19)]

ALANG <- add_row(ALANG,'1' = "Sum of primary inputs equals sum of final use",
                 Coef1 = 1,'Row parent' = "1-e",'Row child' = 3,'Row grandchild' = "1-e",
                 'Column parent' = "1-e",'Column child' = 1,'Column grandchild' = "1-e",
                 'Coef1.1' = -1,'Row parent.1' = "1-e",'Row child.1' = 2,'Row grandchild.1' = "1-e",
                 'Column parent.1' = "1-e",'Column child.1' = 3,'Column grandchild.1' = "1-e")

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
path_set <- paste0(path$root,"ALANGfiles/PrimaryInputEqualsFinalUse")
if(dir.exists(path_set)) unlink(path_set,recursive = TRUE) 
# Create new folder  
dir.create(path_set)



# Write data frame with ALANG commands as tab-delimited txt-file to root and working directory (mother)
# Note HP: This is probably not the normal procedure, meaning no IE ALANG's in the root
filename <-  paste0(path_set,"/",gsub("-","",Sys.Date()),
                    "_PIOLab_PrimaryInputEqualsFinalUse_000_Constraints-",year,"_000_RoWexcluded.txt")

write.table(ALANG,file = filename,row.names = FALSE, quote = F,sep = "\t")

# Check if the mother directory really exists
if(dir.exists(path$mother))
{ 
  filename <-  paste0(path$mother,gsub("-","",Sys.Date()),
                      "_PIOLab_PrimaryInputEqualsFinalUse_000_Constraints-",year,"_000_RoWexcluded.txt")
  
  write.table(ALANG,file = filename,row.names = FALSE, quote = F,sep = "\t") 
}

print("datafeed_PIOLab_PrimaryInputEqualsFinalUse finished.")

