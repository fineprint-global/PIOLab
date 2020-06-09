###############################################
#                                             #
#   This is the IE data feed for processing   #
#     raw data of the iron and steel PIOT     #
#     it covers 20 processes and 22 products  #
#                                             #
###############################################
# IE feed for 20 industries/processes and 22 products base classifications
# hanspeter.wieland@wu.ac.at (c)
# 04.16.2020 

# In case the code is executed not on the server (and the GUI) for debugging, 
# the user can choose the desired region aggregator by setting the following variable
# either to 5,35 or 49. If test_regagg is not defined it will be set automatically to 
# 5 regions later on in the code.
# test_regagg <- "049"

################################################################################
### 1. Set up workplace for building the initial estimate

IEdatafeed_name <- "Ind30Pro39v1" 

print(paste0("Start of ",IEdatafeed_name," InitialEstimate."))

# Set library path when running on suphys server
if(Sys.info()[1] == "Linux"){
  .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")
  # Define location for root directory
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"}else{
  root_folder <- "C:/Users/hwieland/Github workspace/PIOLab/"}

# Initializing R script (load R packages and set paths to folders etc.)
source(paste0(root_folder,"Rscripts/Subroutines/InitializationR.R"))

# Read base classification settings (region, processe and flow codes) 
source(paste0(path$root,"Rscripts/Subroutines/Read_BaseClassification.R"))

# Read root to mother sector aggregators
source(paste0(path$root,"Rscripts/Subroutines/Load_Root2Mother_sectors.R"))

# Read region aggregation from classification to set the right path for the IE data

if(max(base$region$Code) < 10) 
  {
    regagg <- paste0("00",max(base$region$Code))
  } else
  {
    regagg <- paste0("0",max(base$region$Code))
  }

# Set additional paths that are specific to the present run
path[["IE_Subroutines"]] <- paste0(path$root,"Rscripts/IEfeeds_code/IE_subroutines")
path[["IE_Processed"]] <- paste0(path$root,"ProcessedData/",IEdatafeed_name,"/",regagg)
path[["Agg_Processed"]] <- paste0(path$root,"ProcessedData/",IEdatafeed_name)


# Check whether output folder for processed data for the present initial estimate exists, if not then create it

if(!dir.exists(path$Agg_Processed)) dir.create(path$Agg_Processed)

# Check whether output folder for processed data for the specific aggregation exists, if yes, delete it
if(dir.exists(path$IE_Processed)) unlink(path$IE_Processed,recursive = TRUE) 
dir.create(path$IE_Processed)

# Check if ALANG files of old initial estimate exist and delete
source(paste0(path$Subroutines,"/DeleteALANGfilesOfOldIEfeeds.R"))

# Functions to process raw data for Initial Estimate

IE_fun <- list("/IEFeed_PIOLab_WSA.R",
               "/IEFeed_PIOLab_SteelIndustryYields.R",
               "/IEFeed_PIOLab_BACI.R",
               "/IEFeed_PIOLab_IRP.R",
               "/IEFeed_PIOLab_Grades.R",
               "/IEFeed_PIOLab_EOL.R",
               "/IEFeed_PIOLab_IEA.R",
               "/IEFeed_PIOLab_EXIOWasteMFAIO.R",
               "/IEFeed_PIOLab_Cullen.R",
               "/IEDataProcessing_PIOLab_AligningData.R",
               "/IEDataProcessing_PIOLab_WasteMFAIOExtension.R",
               "/IEDataProcessing_PIOLab_WasteMFAIOModelRun.R",
               "/IEDataProcessing_PIOLab_BuildingDomesticTables.R",
               "/IEDataProcessing_PIOLab_BuildingTradeBlocks.R",
               "/IEDataProcessing_PIOLab_BuildS8fromSupplyUseTables.R",
               "/Check_MassBalances.R")

IE_fun <- paste0(path$IE_Subroutines,IE_fun)  # Add path to functions
lapply(IE_fun,source)                         # Load functions into workspace


### 2. Commencing data pre-processing (from source to root classification)

IEFeed_PIOLab_WSA(year,path)             # WSA production numbers
IEFeed_PIOLab_SteelIndustryYields(path)  # Yield information
IEFeed_PIOLab_BACI(year,path)            # Trade data
IEFeed_PIOLab_IRP(year,path)             # Extraction data
IEFeed_PIOLab_Grades(path)               # Ore grades
IEFeed_PIOLab_EOL(year,path)             # end-of-life i.e. old scrap
IEFeed_PIOLab_IEA(year,path)             # IEA energy data
IEFeed_PIOLab_EXIOWasteMFAIO(year,path)  # Aggregate EXIOBASE Waste-MFA IO model (WIO)
IEFeed_PIOLab_Cullen(path)               # Fabrication yields from Cullen et al. 2012


### 3. Commencing data processing

IEDataProcessing_PIOLab_AligningData(year,path)        # Align WSA, IRP extraction
IEDataProcessing_PIOLab_WasteMFAIOExtension(year,path) # Compile extension for WIO  
IEDataProcessing_PIOLab_WasteMFAIOModelRun(year,path)  # Run WIO Model calculation

### 4. Compile domestic SUTs, trade blocks and S8 files thereof

IEDataProcessing_PIOLab_BuildingDomesticTables(year,path)
IEDataProcessing_PIOLab_BuildingTradeBlocks(year,path)
IEDataProcessing_PIOLab_BuildS8fromSupplyUseTables(year,path)

### 5. Write ALANG commands

# Set up wrapper for adding rows to ALANG

NewALANG <- function(name,SE,ALANG)
{
  file <- paste0("S8 ",path$mother,"Data/IE/",gsub("-","",Sys.Date()),
                 "_PIOLab_AllCountriesS8File_",name,year,".csv")
  
  ALANG <- add_row(ALANG,'1' = name,Coef1 = file, S.E. = SE,
                   Value = "I",Incl = "Y",Parts = "1",'Row parent' = "",'Row child' = "",
                   'Row grandchild' = "",'Column parent' = "",'Column child' = "",
                   'Column grandchild' = "",Years = "",Margin = "",'Pre-map' = "",'Post-map' = "",
                   'Pre-Map' = "",'Post-Map' = "")
  
  return(ALANG)
}

# n_reg <- nrow(base$region)
# Create empty file with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))

RSE <- read.xlsx( paste0(path$Settings,"/Base/IE_settings.xlsx"), sheet = 3 )

# Write ALANG commands

name <- "Supply"
RSE_sel <- RSE[RSE$item == name,]
ALANG <- NewALANG(name,
                  paste0("E MX",RSE_sel$MX,";MN",RSE_sel$MN,";"),
                  ALANG)

name <- "Use"
RSE_sel <- RSE[RSE$item == name,]
ALANG <- NewALANG(name,
                  paste0("E MX",RSE_sel$MX,";MN",RSE_sel$MN,";CN",RSE_sel$CN,";"),
                  ALANG)

name <- "FinalOutput"
RSE_sel <- RSE[RSE$item == name,]
ALANG <- NewALANG(name,
                  paste0("E MX",RSE_sel$MX,";MN",RSE_sel$MN,";"),
                  ALANG)

name <- "Extraction"
RSE_sel <- RSE[RSE$item == name,]
ALANG <- NewALANG(name,
                  paste0("E MX",RSE_sel$MX,";MN",RSE_sel$MN,";"),
                  ALANG)

name <- "EolScrap"
RSE_sel <- RSE[RSE$item == name,]
ALANG <- NewALANG(name,
                  paste0("E MX",RSE_sel$MX,";MN",RSE_sel$MN,";"),
                  ALANG)

name <- "OtherInput"
RSE_sel <- RSE[RSE$item == name,]
ALANG <- NewALANG(name,
                  paste0("E MX",RSE_sel$MX,";MN",RSE_sel$MN,";"),
                  ALANG)

name <- "Waste"
RSE_sel <- RSE[RSE$item == name,]
ALANG <- NewALANG(name,
                  paste0("E MX",RSE_sel$MX,";MN",RSE_sel$MN,";"),
                  ALANG)

name <- "Zero"
RSE_sel <- RSE[RSE$item == name,]
ALANG <- NewALANG(name,
                  paste0("E CN",RSE_sel$CN,";"),
                  ALANG)

ALANG$`#` <- as.character(1:nrow(ALANG))

# Write data frame with ALANG commands as tab-delimited txt-file to root and working directory (mother)
# Note HP: This is probably not the normal procedure, meaning no IE ALANG's in the root
filename <-  paste0(path$root,"ALANGfiles/",gsub("-","",Sys.Date()),
                    "_PIOLab_SUT_000_InitialEstimate-",year,"_000_S8filesForAllRegionsAndFlows.txt")

write.table(ALANG,file = filename,row.names = FALSE, quote = F,sep = "\t")

# Check if the mother directory really exists

if(file.exists(path$mother))
{ 
  filename <-  paste0(path$mother,gsub("-","",Sys.Date()),
                      "_PIOLab_SUT_000_InitialEstimate-",year,"_000_S8filesForAllRegionsAndFlows.txt")
  
  write.table(ALANG,file = filename,row.names = FALSE, quote = F,sep = "\t") 
}

Check_MassBalances()
  
print( paste0("End of ",IEdatafeed_name," InitialEstimate.") )
