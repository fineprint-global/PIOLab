###############################################
#                                             #
#   This is the IE data feed for processing   #
#     raw data of the iron and steel PIOT     #
#                                             #
###############################################

print("Start of StandardPIOT InitialEstimate.")
# 0. Initialize R packages
# Note HP: Insert code to check available R packages here and install if required.
#tryCatch({library(dlyr)},error = function(e) 
#  {install.packages('dplyr')})

# Set library path when running on suphys server
if(Sys.info()[1] == "Linux") .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")

library(dplyr)
library(tidyr)
# Because the data.table package is not available for the R version currently running on the USYD server
# use the forerunner version of it, that is the reshape2 package, to use its functionalities
#library(data.table)
library(reshape2)
library(openxlsx)
library(stringr)
library(R.matlab)

################################################################################
# 1. Set global variables and paths
# Set the year(s)
year <- 2008
# Number of regions
n_reg <- 35

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
             "ALANG" = paste0(root_folder,"ALANGfiles"),
             "root" = root_folder,
             "mother" = mother)


# Check whether output folders for processed data and ALANGs exists, if yes delete them
if(dir.exists(path$Processed)) unlink(path$Processed,recursive = TRUE) 
if(dir.exists(path$ALANG)) unlink(path$ALANG,recursive = TRUE) 
dir.create(path$Processed)
dir.create(path$ALANG)

################################################################################
# 2. Load functions
source(paste0(path$Subroutines,"/DataFeed_PIOLab_BACI.R"))
source(paste0(path$Subroutines,"/DataFeed_PIOLab_IRP.R"))
source(paste0(path$Subroutines,"/DataFeed_PIOLab_Grades.R"))
source(paste0(path$Subroutines,"/DataFeed_PIOLab_WSA.R"))
source(paste0(path$Subroutines,"/DataFeed_PIOLab_SteelIndustryYields.R"))
source(paste0(path$Subroutines,"/DataFeed_PIOLab_EOL.R"))
source(paste0(path$Subroutines,"/DataFeed_PIOLab_IEA.R"))
source(paste0(path$Subroutines,"/DataFeed_PIOLab_EXIOWasteMFAIO.R"))
source(paste0(path$Subroutines,"/DataFeed_PIOLab_Cullen.R"))
source(paste0(path$Subroutines,"/DataProcessing_PIOLab_AligningData.R"))
source(paste0(path$Subroutines,"/DataProcessing_PIOLab_WasteMFAIOExtension.R"))
source(paste0(path$Subroutines,"/DataProcessing_PIOLab_WasteMFAIOModelRun.R"))
source(paste0(path$Subroutines,"/DataProcessing_PIOLab_BuildingDomesticTables.R"))
source(paste0(path$Subroutines,"/DataProcessing_PIOLab_BuildingTradeBlocks.R"))

################################################################################
# 3. Commencing data feeds

# Loading the trade data
DataFeed_PIOLab_BACI(year,path)
# The extraction and ore grade feed
DataFeed_PIOLab_IRP(year,path)
DataFeed_PIOLab_Grades(path)
# Loading production values for semi- and finished steel + information on yields
DataFeed_PIOLab_WSA(year,path)
DataFeed_PIOLab_SteelIndustryYields(path)
# Loading end-of-life steel scrap
DataFeed_PIOLab_EOL(year,path)
# Loading energy data
DataFeed_PIOLab_IEA(year,path)
# Loading and aggregating EXIOBASE Waste-MFA IO version
DataFeed_PIOLab_EXIOWasteMFAIO(year,path)
# Loading fabrication yields taken from Cullen et al 2012
DataFeed_PIOLab_Cullen(path)

################################################################################
# 4. Commencing data processing

# Aligning WSA and IEA data and filling gaps in WSA accounts. 
# Moreover, remove BACI trade flows of iron ores for regions where IRP reports no extraction
DataProcessing_PIOLab_AligningData(year,path)
# Compile extension for the MFA-Waste IO Model and estimate fabrication scrap
DataProcessing_PIOLab_WasteMFAIOExtension(year,path)
# Run Waste-IO Model calculation
DataProcessing_PIOLab_WasteMFAIOModelRun(year,path)
# Compile national SUTs
DataProcessing_PIOLab_BuildingDomesticTables(year,path)
# Compiling trade blocks
DataProcessing_PIOLab_BuildingTradeBlocks(year,path)

################################################################################
# 5. Write ALANG commands
print("Start writing ALANG commands.")
# For the development phase on Windows laptop, set DATAPATH here
#if(Sys.info()[1] == "Windows") DATAPATH <- paste0(root_folder,"ProcessedData")

# Create empty file with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))
ALANG <- makeALANGheadline()

# Standard error for all blocks that don't have a specific S.E.
SE_value <- "E MX1;MN10;CN1e+3;"

# Write commands for the domestic SUTs and the trade blocks

for(i in 1:n_reg)
{  
  # Add command for domestic Use table
  ALANG <- add_row(ALANG,'1' = paste0("InitialEstimate_StandardPIOT_DomesticUse_Region",i),
                   Coef1 = paste0("A ",path$mother,"Data/IE/",year,"_DomesticUse_Region",i,".csv"),
                   'Row parent' = i,'Row child' = 2,'Row grandchild' = "1:e",
                   'Column parent' = i,'Column child' = 1,'Column grandchild' = "1:e",S.E. = SE_value)
  
  # Add command for domestic Supply table
  ALANG <- add_row(ALANG,'1' = paste0("InitialEstimate_StandardPIOT_DomesticSupply_Region",i),
                   Coef1 = paste0("A ",path$mother,"Data/IE/",year,"_DomesticSupply_Region",i,".csv"),
                   'Row parent' = i,'Row child' = 1,'Row grandchild' = "1:e",
                   'Column parent' = i,'Column child' = 2,'Column grandchild' = "1:e",S.E. = SE_value)
  
  # Add command for domestic BoundaryOutput (domestic final use block, that is final demand and wastes to the environment)
  ALANG <- add_row(ALANG,'1' = paste0("InitialEstimate_StandardPIOT_BoundaryOutput_region",i),
                   Coef1 = paste0("A ",path$mother,"Data/IE/",year,"_BoundaryOutput_Region",i,".csv"),
                   'Row parent' = i,'Row child' = 2,'Row grandchild' = "1:e",
                   'Column parent' = i,'Column child' = 3,'Column grandchild' = "1:2",S.E. = SE_value)
  
  # Inputs from nature (primary inputs)
  ALANG <- add_row(ALANG,'1' = paste0("InitialEstimate_StandardPIOT_InputsFromNature_Region",i),
                   Coef1 = paste0("A ",path$mother,"Data/IE/",year,"_InputsFromNature_Region",i,".csv"),
                   'Row parent' = i,'Row child' = 3,'Row grandchild' = 1,
                   'Column parent' = i,'Column child' = 1,'Column grandchild' = "1:e",S.E. = 0)
  
  # EOL scrap (primary inputs)
  ALANG <- add_row(ALANG,'1' = paste0("InitialEstimate_StandardPIOT_EolScrap_Region",i),
                   Coef1 = paste0("A ",path$mother,"Data/IE/",year,"_EolScrap_Region",i,".csv"),
                   'Row parent' = i,'Row child' = 3,'Row grandchild' = 2,
                   'Column parent' = i,'Column child' = 1,'Column grandchild' = "1:e",S.E. = SE_value)
  
  # Write trade blocks
  for(j in (1:n_reg)[-i])
  {
    # For intermediate trade
    ALANG <- add_row(ALANG,'1' = paste0("InitialEstimate_StandardPIOT_IntermediateTrade_region ",i," to ",j),
                     Coef1 = paste0("A ",path$mother,"Data/IE/",year,"_IntermediateTrade_",i,"_",j,".csv"),
                     'Row parent' = i,'Row child' = 2,'Row grandchild' = "1:e",
                     'Column parent' = j,'Column child' = 1,'Column grandchild' = "1:e",S.E. = SE_value)
    
    # For final trade
    ALANG <- add_row(ALANG,'1' = paste0("InitialEstimate_StandardPIOT_FinalTrade_region ",i," to ",j),
                     Coef1 = paste0("A ",path$mother,"Data/IE/",year,"_FinalTrade_",i,"_",j,".csv"),
                     'Row parent' = i,'Row child' = 2,'Row grandchild' = "1:e",
                     'Column parent' = j,'Column child' = 3,'Column grandchild' = "1:2",S.E. = SE_value)
    
  }
}
# Add other variables
ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$Incl <- "Y"
ALANG$Parts <- 1
ALANG$Value <- "I"
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
ALANG$Years <- 1
ALANG$Margin <- 1

#ALANG$S.E. <- "E MX1;MN10;CN1e+3;"
#ALANG$S.E. <- "E LG;CN100;"

# Write data frame with ALANG commands as tab-delimited txt-file to root and working directory (mother)
# Note HP: This is probably not the normal procedure, meaning no IE ALANG's in the root
filename <-  paste0(path$root,"ALANGfiles/",gsub("-","",Sys.Date()),
                    "_PIOLab_SUT_000_InitialEstimate-",year,"_000_DomesticSUT.txt")

write.table(ALANG,file = filename,row.names = FALSE, quote = F,sep = "\t")
# Check if the mother directory really exists
if(dir.exists(path$mother))
{ 
  filename <-  paste0(path$mother,gsub("-","",Sys.Date()),
                      "_PIOLab_SUT_000_InitialEstimate-",
                      year,"_000_DomesticSUT.txt")
  
  write.table(ALANG,file = filename,row.names = FALSE, quote = F,sep = "\t") 
}
  

print("End of StandardPIOT InitialEstimate.")

