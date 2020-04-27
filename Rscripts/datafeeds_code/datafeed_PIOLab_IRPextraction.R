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
path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name)  # Add datafeed specific path for output data
path["df_conco"] <- paste0(path$Concordance,"/IRP/")  # Add datafeed specific path for S2R concordance

# Load function to write arrays to files
source( paste0(path$Subroutines,"/Numbers2File.R") )

# Read date of concordance for sourc2root industries and products
set <- read.xlsx(xlsxFile = paste0(path$Settings,"/datafeeds_settings/IRP_settings.xlsx"),sheet = 1)
          
Conco <- list("process" = read.xlsx(xlsxFile = paste0(path$df_conco,set$date,"_IRP_Extraction_SecConc.xlsx"),sheet = 1),
              "flow" =  read.xlsx(xlsxFile = paste0(path$df_conco,set$date,"_IRP_Extraction_SecConc.xlsx"),sheet = 2) )

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
 
ALANG$`#` <- 1:nrow(ALANG)
ALANG$Coef1 <- 1
ALANG$Years <- 1
ALANG$Margin <- 1

ALANG$`Row parent` <- "1-e"
ALANG$`Row child` <- 3
ALANG$`Row grandchild` <- root$input$Code[ root$input$Name == "Hematite & Magnetite" ]

ALANG$`Column parent` <- "1:e t2 CONCPATH/Root2Root_Reg_Concordance.csv"
ALANG$`Column child` <- "1"
ALANG$`Column grandchild` <- paste( Conco$process$Code[Conco$process$binary == 1] , collapse = "," )

ALANG$Incl <- "Y"
ALANG$Parts <- "1"
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
  
# Call script that writes the ALANG file to the repsective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))
  
print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

