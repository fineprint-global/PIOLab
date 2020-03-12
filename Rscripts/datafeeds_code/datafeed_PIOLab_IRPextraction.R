################################################################################

datafeed_name <- "IRPextraction"
print(paste0("datafeed_PIOLab_",datafeed_name," initiated."))

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

path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name)

# Load function to write arrays to files
source(paste0(path$Subroutines,"/Numbers2File.R"))

# Loading raw data
source(paste0(path$Subroutines,"/Read_ExtractionIRP.R"))

# Loading function for estimating SE with linear regression
source(paste0(path$Subroutines,"/SE_LogRegression.R"))

# read upper and lower error bounds from settings file
RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)

# Estimate standard error
data <- SE_LogRegression(data,RSE$Minimum,RSE$Maximum)

n_reg <- nrow(root$region) # Number of root regions

df <- data.frame("RHS" = rep(0,n_reg), "SE" = rep(0,n_reg))
df$RHS[data$Code] <- data$Quantity
df$SE[data$Code] <- data$SE

# Check if folder with processed data exists, in case delete and create empty one
if(dir.exists(path$df_Processed)) unlink(path$df_Processed,recursive = TRUE) 
dir.create(path$df_Processed)

Conco <- diag(nrow(root$region)) # Create pseudo aggregator

filename_RegAgg <- "/Root2Root_Reg_Concordance.csv" # Define name of file

Numbers2File(Conco,paste0(path$Concordance,filename_RegAgg)) # Save aggregator 

filename <- list("RHS" = paste0("/",datafeed_name,"/",datafeed_name,"_RHS_",
                                year,".csv"),
                 "SE" = paste0("/",datafeed_name,"/",datafeed_name,"_SE_",
                               year,".csv"))

Numbers2File( t(df$RHS) , paste0( path$Processed, filename$RHS ) ) 

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))

ALANG <- add_row(ALANG,
                 '1' = paste(datafeed_name,year),
                 Value = paste0("DATAPATH",filename$RHS),
                 S.E. = paste0("E MX",RSE$Maximum,"; MN",RSE$Minimum,";") )
 
# for(i in data$Code) 
# {
#   sel <- df
#   sel[-i,] <- NaN
#   
#   filename <- list("RHS" = paste0("/",datafeed_name,"/",datafeed_name,"_RHS_",year,
#                                   "_",root$region$RootCountryAbbreviation[i],".csv"),
#                    "SE" = paste0("/",datafeed_name,"/",datafeed_name,"_SE_",year,
#                                  "_",root$region$RootCountryAbbreviation[i],".csv"))
#   
#   # Write RHS and SE data to folder
#   Numbers2File(sel$RHS,paste0(path$Processed,filename$RHS)) 
#   Numbers2File(sel$SE,paste0(path$Processed,filename$SE)) 
#   
#   ALANG <- add_row( ALANG,
#                    '1' = paste0("DataFeed IRP Extraction ",year," ",root$region$Name[i]),
#                    'Value' = paste0("DATAPATH",filename$RHS),
#                    'S.E.' = paste0("DATAPATH",filename$SE) )
# }

ALANG$`#` <- 1:nrow(ALANG)
ALANG$Coef1 <- "1"
ALANG$Years <- "1"
ALANG$Margin <- "1"

ALANG$`Row parent` <- "1-e"
ALANG$`Row child` <- "3"
ALANG$`Row grandchild` <- "1"

ALANG$`Column parent` <- paste0("1:e t2 CONCPATH",filename_RegAgg)
ALANG$`Column child` <- "1"
ALANG$`Column grandchild` <- "1-4"

ALANG$Incl <- "Y"
ALANG$Parts <- "1"
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
  
# Call script that writes the ALANG file to the repsective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))
  
print(paste0("datafeed_PIOLab_",datafeed_name," finished."))
