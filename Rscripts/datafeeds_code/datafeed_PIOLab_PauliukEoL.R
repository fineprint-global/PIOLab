################################################################################

datafeed_name <- "PauliukEoL"
print(paste0("datafeed_PIOLab_",datafeed_name," initiated."))

# Determine loaction of root folder
################################################################################

# Set library path depending on whether data feed runs on Uni Sydney server or local
if(Sys.info()[1] == "Linux")
{
  # Setting the R package library folder on Uni Sydney server
  .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")
  
  # Define location of root directory on the Uni Sydney server:
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"
  
} else{
  
  library(tidyr)
  # Locating folder where the present script is stored locally to derive the root folder 
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  
  if(length(this_file)==0) this_file <- rstudioapi::getSourceEditorContext()$path
  
  root_folder <- substr(dirname(this_file),1,nchar(dirname(this_file))-23)
  remove(this_file)
}
################################################################################

# Initializing R script (load R packages and set paths to folders etc.)

source(paste0(root_folder,"Rscripts/Subroutines/InitializationR.R"))

path["df_Processed"] <- paste0(path$Processed,"/",datafeed_name)
path$ALANG <- paste0(path$ALANG,"/",datafeed_name)

# Call script to clear ALANG and processed data folders of the present data feed
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/ClearFolders.R"))

source(paste0(path$Subroutines,"/Numbers2File.R")) # Load fun. to write arrays to files

year <- 2008

# Load Pauliuk data in quasi root classification 

source(paste0(path$Subroutines,"/Load_PauliukEoL.R"))

# Check whether NA exist and add artificial code until later processing in this script

data$Code[is.na(data$Code)] <- 666

data <- select(data,Code,Quantity) # Select only variables that are needed

data <- group_by(data,Code) %>% summarise(Quantity = sum(Quantity)) %>% ungroup(Code)

# Loading function for estimating SE with linear regression

source(paste0(path$Subroutines,"/SE_LogRegression.R"))

RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)

data <- SE_LogRegression(data,RSE$Minimum,RSE$Maximum) # Estimate standard errors
colnames(data)[2:3] <- c("RHS","SE") 

data <- data[order(data$Code),]
index <- data.frame("Source" = 1:nrow(data), "Root" = data$Code)

# Issues regarding Pauliuk EoL data:
# A number of countries are only available as aggregated regions, these are
# the former USSR (source code 86), Former Yugoslavia (106), Czechoslovakia (27), Belgium-Luxembbourg (8). 
# Furthermore the data set does not include Taiwan (implicit in nec regions w. code 666).

# Store source and root region codes for aggregation in one list

Reg <- list("Source" = c(86,106,27,8),
            "Root" = list(c(166,65,119,117,24,205,123,73,10,14,100,209,194,102,193),
                          c(23,86,128,132,178,183),
                          c(52,182),
                          c(16,118) ))

# Create concodance matrix Source2Root for Pauliuk data

Conco <- matrix(0,nrow = nrow(data),ncol = nrow(root$region))

# Remove indices that are not to be aggregated:
index <- index[-nrow(index),] # NEC regions
index <- index[-Reg$Source,] # Aggregated regions

# Write source2region codes for all regions that are not aggregated:

Conco[as.matrix(index)] <- 1

Conco[Reg$Source[1],Reg$Root[[1]]] <- 1 # Former USSR countries 

Conco[Reg$Source[2],Reg$Root[[2]]] <- 1 # Former Yugoslavia

Conco[Reg$Source[3],Reg$Root[[3]]] <- 1 # Czechoslovakia

Conco[Reg$Source[4],Reg$Root[[4]]] <- 1 # Belgium-Luxembourg

# Allocate the remaining root regions to source region NEC
Conco[109,colSums(Conco) == 0] <- 1

# Print sum of matrix which should be 221
print(paste0("Sum of aggregator matrix = ",sum(Conco)))

filename_RegAgg <- "/PauliukEoL_Reg_Source2Root.csv" # Define name of file

Numbers2File(Conco,paste0(path$Concordance,filename_RegAgg)) # Save aggregator 


# Check if folder with processed data exists, in case delete and create empty one

if(dir.exists(path$df_Processed)) unlink(path$df_Processed,recursive = TRUE) 

dir.create(path$df_Processed)



filename <- list("RHS" = paste0("/",datafeed_name,"/",datafeed_name,"_RHS_",
                                year,".csv"),
                 "SE" = paste0("/",datafeed_name,"/",datafeed_name,"_SE_",
                               year,".csv"))

data <- data[,-1] # Remove codes


Numbers2File( t(data$RHS) , paste0( path$Processed, filename$RHS ) ) 


source(paste0(path$Subroutines,"/makeALANGheadline.R")) # Create ALANG header

ALANG <- add_row(ALANG,
                 '1' = paste("Pauliuk EoL",year),
                 Value = paste0("DATAPATH",filename$RHS),
                 S.E. = paste0("E MX",RSE$Maximum,"; MN",RSE$Minimum,";") )

# Add other variables
ALANG$`#` <- as.character(1:nrow(ALANG))

ALANG$Years <- "1"
ALANG$Margin <- "1"
ALANG$Coef1 <- "1"

ALANG$`Row parent` <- "1-e"
ALANG$`Row child` <- "3"
ALANG$`Row grandchild` <- "1"

ALANG$`Column parent` <- paste0("1:e t2 CONCPATH",filename_RegAgg)
ALANG$`Column child` <- "1"
ALANG$`Column grandchild` <- "64-65"

ALANG$Incl <- "Y"
ALANG$Parts <- "1"
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
  
# Call script that writes the ALANG file to the repsective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))
  
print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

rm(list = ls()) # clear workspace
