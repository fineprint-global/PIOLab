###############################################
#                                             #
#   This is the IE data feed for processing   #
#     raw data of the iron and steel PIOT     #
#                                             #
###############################################

IEdatafeed_name <- "StandardPIOT" 
print(paste0("Start of ",IEdatafeed_name," InitialEstimate."))

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
path[["IE_Subroutines"]] <- paste0(root_folder,"Rscripts/IEfeeds_code/",IEdatafeed_name,"_IE_subroutines")
path[["IE_Processed"]] <- paste0(root_folder,"ProcessedData/",IEdatafeed_name)

# Read base regions, products and codes
base <- list("region" = read.xlsx(paste0(path$Concordance,"/",IEdatafeed_name,"_BaseClassification.xlsx"),sheet = 1),
             "industry" = read.xlsx(paste0(path$Concordance,"/",IEdatafeed_name,"_BaseClassification.xlsx"),sheet = 2),
             "product" = read.xlsx(paste0(path$Concordance,"/",IEdatafeed_name,"_BaseClassification.xlsx"),sheet = 3))

# Check whether output folder for processed data exists, if yes delete it
if(dir.exists(path$IE_Processed)) unlink(path$IE_Processed,recursive = TRUE) 
dir.create(path$IE_Processed)
# Check if ALANG files of old initial estimateexist and delete
source(paste0(path$Subroutines,"/DeleteALANGfilesOfOldIEfeeds.R"))

################################################################################
# 2. Load functions
source(paste0(path$IE_Subroutines,"/IEFeed_PIOLab_WSA.R"))
source(paste0(path$IE_Subroutines,"/IEFeed_PIOLab_SteelIndustryYields.R"))
source(paste0(path$IE_Subroutines,"/IEFeed_PIOLab_BACI.R"))
source(paste0(path$IE_Subroutines,"/IEFeed_PIOLab_IRP.R"))
source(paste0(path$IE_Subroutines,"/IEFeed_PIOLab_Grades.R"))
source(paste0(path$IE_Subroutines,"/IEFeed_PIOLab_EOL.R"))
source(paste0(path$IE_Subroutines,"/IEFeed_PIOLab_IEA.R"))
source(paste0(path$IE_Subroutines,"/IEFeed_PIOLab_EXIOWasteMFAIO.R"))
source(paste0(path$IE_Subroutines,"/IEFeed_PIOLab_Cullen.R"))

source(paste0(path$IE_Subroutines,"/IEDataProcessing_PIOLab_AligningData.R"))
source(paste0(path$IE_Subroutines,"/IEDataProcessing_PIOLab_WasteMFAIOExtension.R"))

source(paste0(path$IE_Subroutines,"/IEDataProcessing_PIOLab_WasteMFAIOModelRun.R"))
source(paste0(path$IE_Subroutines,"/IEDataProcessing_PIOLab_BuildingDomesticTables.R"))
source(paste0(path$IE_Subroutines,"/IEDataProcessing_PIOLab_BuildingTradeBlocks.R"))
source(paste0(path$IE_Subroutines,"/IEDataProcessing_PIOLab_StandardErrorsForTy.R"))

################################################################################
# 3. Commencing data feeds
# Loading production values for semi- and finished steel + information on yields
IEFeed_PIOLab_WSA(year,path)
IEFeed_PIOLab_SteelIndustryYields(path)
# Loading the trade data
IEFeed_PIOLab_BACI(year,path)
# The extraction and ore grade feed
IEFeed_PIOLab_IRP(year,path)
IEFeed_PIOLab_Grades(path)
# Loading end-of-life steel scrap
IEFeed_PIOLab_EOL(year,path)
# Loading energy data
IEFeed_PIOLab_IEA(year,path)
# Loading and aggregating EXIOBASE Waste-MFA IO version
IEFeed_PIOLab_EXIOWasteMFAIO(year,path)
# Loading fabrication yields taken from Cullen et al 2012
IEFeed_PIOLab_Cullen(path)

################################################################################
# 4. Commencing data processing

# Aligning WSA and IEA data and filling gaps in WSA accounts. 
# Moreover, remove BACI trade flows of iron ores for regions where IRP reports no extraction
IEDataProcessing_PIOLab_AligningData(year,path)
# Compile extension for the MFA-Waste IO Model and estimate fabrication scrap
IEDataProcessing_PIOLab_WasteMFAIOExtension(year,path)
# Run Waste-IO Model calculation
IEDataProcessing_PIOLab_WasteMFAIOModelRun(year,path)
# Compile national SUTs
IEDataProcessing_PIOLab_BuildingDomesticTables(year,path)
# Compiling trade blocks
IEDataProcessing_PIOLab_BuildingTradeBlocks(year,path)

IEDataProcessing_PIOLab_StandardErrorsForTy(year,path)

################################################################################
# 5. Write ALANG commands
print("Start writing ALANG commands.")
# For the development phase on Windows laptop, set DATAPATH here
#if(Sys.info()[1] == "Windows") DATAPATH <- paste0(root_folder,"ProcessedData")

n_reg <- nrow(base$region)
# Create empty file with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))

# Standard error for all blocks that don't have a specific S.E.
#SE_value <- "E MX1;MN10;CN1e+3;"
SE_value <- ""

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
                   'Column parent' = i,'Column child' = 1,'Column grandchild' = "1:e",S.E. = 0)
  
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
ALANG$Value <- "I"
ALANG$Years <- 1
ALANG$Margin <- 1

# Write command for the standard errors
ALANG <- add_row(ALANG,'1' = "Standard deviation estimator",
                 Coef1 = paste0("S8 ",path$IE_Processed,"/StandardErrorsForTy/",gsub("-","",Sys.Date()),
                                "_PIOLab_AllCountries_000_StandardErrorsForTy_000_S8FileFor",year,".csv"),
                Value = "S", 'Row parent' = "",'Row child' = "",'Row grandchild' = "",'Column parent' = "",
                'Column child' = "",'Column grandchild' = "",Years = "",Margin = "",S.E. = "E MX100;MN1000;CN1000")

ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$Incl <- "Y"
ALANG$Parts <- 1
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""

#ALANG$S.E. <- "E MX1;MN10;CN1e+3;"
#ALANG$S.E. <- "E LG;CN100;"

# Write data frame with ALANG commands as tab-delimited txt-file to root and working directory (mother)
# Note HP: This is probably not the normal procedure, meaning no IE ALANG's in the root
filename <-  paste0(path$root,"ALANGfiles/",gsub("-","",Sys.Date()),
                    "_PIOLab_SUT_000_InitialEstimate-",year,"_000_DomesticSUT.txt")

write.table(ALANG,file = filename,row.names = FALSE, quote = F,sep = "\t")
# Check if the mother directory really exists
if(file.exists(path$mother))
{ 
  filename <-  paste0(path$mother,gsub("-","",Sys.Date()),
                      "_PIOLab_SUT_000_InitialEstimate-",
                      year,"_000_DomesticSUT.txt")
  
  write.table(ALANG,file = filename,row.names = FALSE, quote = F,sep = "\t") 
}
  

print(paste0("End of ",IEdatafeed_name," InitialEstimate."))


