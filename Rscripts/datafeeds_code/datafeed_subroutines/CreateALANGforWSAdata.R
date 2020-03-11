################################################################################
# This is a subfunction for the data feeds that process the World Steel Association data

print(paste0("datafeed_PIOLab_",datafeed_name," initiated."))

# Initializing R script (load R packages and set paths to folders etc.)
source(paste0(root_folder,"Rscripts/Subroutines/InitializationR.R"))

path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name) # Add path for processed data

# Check if folder with processed data exists, in case delete and create empty one
if(dir.exists(path$df_Processed)) unlink(path$df_Processed,recursive = TRUE) 
dir.create(path$df_Processed)
  
# Load function to write arrays to files
source(paste0(path$Subroutines,"/Numbers2File.R"))
  
# Get relative standard error for smallest and largest values in the data set
RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)
  
# Get data feed settings for WSA accounts
Settings <- read.xlsx(paste0(path$Settings,"/datafeeds_settings/WSA_settings.xlsx"))
  
Settings <- Settings[Settings$FeedName == datafeed_name,]
  
# Set range of products and industries to be adressed by this feed
Grandchild <- list("RoW" = as.character(Settings$Row),"Column" = as.character(Settings$Col))
  
# Load specific yearbook
source(paste0(path$Subroutines,"/Load_YearbookWSA.R"))
  
# Load function to align data with root classification
source(paste0(path$Subroutines,"/Read_ProductionWSA.R"))
 
# Read page number (WSA Yearbook) from id 
item_page <- items[[Settings$id]]$page
  
# Read values and align with root classification
data <- Read_ProductionWSA(path,year,item_page,yb,concord)

# Loading function for estimating SE with linear regression
source(paste0(path$Subroutines,"/SE_LogRegression.R"))
data <- SE_LogRegression(data,RSE$Minimum,RSE$Maximum)

n_reg <- nrow(root$region) # Number of regions in root
  
# Create data frame to have complete list of root regions for writing to file
df <- data.frame("RHS" = rep(0,n_reg), "SE" = rep(0,n_reg))
df$RHS[data$Code] <- data$Quantity
df$SE[data$Code] <- data$SE

Conco <- diag(nrow(root$region)) # Create pseudo aggregator

filename_RegAgg <- "/Root2Root_Reg_Concordance.csv" # Define name of file

Numbers2File(Conco,paste0(path$Concordance,filename_RegAgg)) # Save aggregator 

source(paste0(path$Subroutines,"/makeALANGheadline.R")) # Create ALANG header

filename <- list("RHS" = paste0("/",datafeed_name,"/",datafeed_name,"_RHS_",
                                year,".csv"),
                 "SE" = paste0("/",datafeed_name,"/",datafeed_name,"_SE_",
                               year,".csv"))

Numbers2File( t(df$RHS) , paste0( path$Processed, filename$RHS ) ) 

ALANG <- add_row(ALANG,
                 '1' = paste(datafeed_name,year),
                 Value = paste0("DATAPATH",filename$RHS),
                 S.E. = paste0("E MX",RSE$Maximum,"; MN",RSE$Minimum,";") )

# for(i in data$Code)
# { 
#   reg_name <- as.character(root$region$Name[i]) # Name of region
#     
#   sel <- df
#   sel[-i,] <- NaN
#     
#   filename <- list("RHS" = paste0("/",datafeed_name,"/",datafeed_name,"_RHS_",year,
#                                     "_",root$region$RootCountryAbbreviation[i],".csv"),
#                      "SE" = paste0("/",datafeed_name,"/",datafeed_name,"_SE_",year,
#                                    "_",root$region$RootCountryAbbreviation[i],".csv"))
#   
#   # Write RHS and SE data to folder
#   Numbers2File( sel$RHS, paste0(path$Processed,filename$RHS) ) 
#   Numbers2File( sel$SE, paste0(path$Processed,filename$SE) ) 
#     
#   # Add command 
#   ALANG <- add_row(ALANG,'1' = paste( Settings$WSA_name, reg_name, year ),
#                      Value = paste0( "DATAPATH", filename$RHS ),
#                      S.E. = paste0( "DATAPATH", filename$SE ) )
# }
  


# Add other variables
ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$Incl <- "Y"
ALANG$Parts <- "1"
  
ALANG$Years <- "1"
ALANG$Margin <- "1"
ALANG$Coef1 <- "1"
  
ALANG$`Row parent` <- "1-e"
ALANG$`Row child` <- "1"
ALANG$`Row grandchild` <- Grandchild$RoW
  
ALANG$`Column parent` <- paste0("1:e t2 CONCPATH",filename_RegAgg)
ALANG$`Column child` <- "2"
ALANG$`Column grandchild` <- Grandchild$Column
  
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
  
# Call script that writes the ALANG file to the respective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))
