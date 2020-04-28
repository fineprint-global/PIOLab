################################################################################

datafeed_name <- "IRPimports"
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

source(paste0(path$Subroutines,"/Numbers2File.R")) # Load fun. to write arrays to files


# Check if folder with processed data exists, in case delete and create empty one

if(dir.exists(path$df_Processed)) unlink(path$df_Processed,recursive = TRUE) 

dir.create(path$df_Processed)


# Loading raw data
data <- read.csv(paste0(path$Raw,"/IRP/all_CCC_Imp_ResearchDB.csv"),stringsAsFactors=FALSE)
colnames(data)[6:49] <- 1970:2013
data <- data %>% filter(CCC_IEATrade_Name == "Iron ores") %>% select(Country,AlphaNumISO,as.character(year)) 
# Remove NA
data <- data[!is.na(data$`2008`),]

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

RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name) # Import RSE

# # Add standard errors 
# source(paste0(path$Subroutines,"/SE_LogRegression.R"))
# data <- SE_LogRegression(data,RSE$Minimum,RSE$Maximum)

n_reg <- nrow(root$region) # Read number of root regions

data_new <- data.frame("code" = 1:n_reg,"RHS" = 0)

data_new$RHS[data$Code] <- data$Quantity

data <- data_new

remove(data_new)


ConcoReg <- diag(nrow(root$region)) # Create pseudo aggregator (for columns)

# Set names and paths to data and concordances:

filename <- list("RHS" = paste0("/",datafeed_name,"/",datafeed_name,"_RHS_",year,".csv"),
                 "ConcoReg" = "/Root2Root_Reg_Concordance.csv")

Numbers2File( t(data$RHS), paste0(path$Processed,filename$RHS)) # Save S2R reg aggregator 

Numbers2File( ConcoReg, paste0(path$Concordance,filename$ConcoReg)) # Save S2R reg aggregator 


source(paste0(path$Subroutines,"/makeALANGheadline.R")) # Create ALANG header

ALANG <- ALANG[,c(1:19,11:19)]  # Extend table with additional columns

ALANG <- add_row(ALANG,'1' = "DataFeed IRP imports")  # Create command

ALANG$Value <- paste0("DATAPATH",filename$RHS)

ALANG$S.E. <- paste0("E MX",RSE$Maximum,"; MN",RSE$Minimum,";")

# Write first 8-tuple (sum of domestic use and imports for each region)

ALANG$Coef1 <- "-1"

ALANG$Years <- "1"
ALANG$Margin <- "1"

ALANG$`Row parent` <- paste0("1:e a CONCPATH",filename$ConcoReg)
ALANG$`Row child` <- "2"
ALANG$`Row grandchild` <- "1-e"
 
ALANG$`Column parent` <- "1:e~3"
ALANG$`Column child` <- "1"
ALANG$`Column grandchild` <- "1-e"

# Write second 8-tuple (subtract sum of domestic use table)

ALANG$Coef1.1 <- "1"

ALANG$Years.1 <- "1"
ALANG$Margin.1 <- "1"

ALANG$`Row parent.1` <- "1-e"
ALANG$`Row child.1` <- "2"
ALANG$`Row grandchild.1` <- "1-e"

ALANG$`Column parent.1` <- "1:e"
ALANG$`Column child.1` <- "1"
ALANG$`Column grandchild.1` <- "1-e"

ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$Incl <- "Y"
ALANG$Parts <- "2"

ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""

# Call script that writes the ALANG file to the repsective folder in the root

source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

rm(list = ls()) # clear workspace

