################################################################################

datafeed_name <- "PauliukEoL"
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
  
# Load Pauliuk data in root classification 
source(paste0(path$Subroutines,"/Load_PauliukEoL.R"))
# Check whether NA exist and delete if so
data <- data[!is.na(data$Code),]

# Issues regarding Pauliuk EoL data:
# A number of countries are only available as aggregated regions, these are
# the former USSR (root code: 178), Former Yugoslavia (242), Czechoslovakia (56)
# Belgium-Luxembbourg. Furthermore the data set does not include Taiwan (212).
# Therefore write ALANG commands that specifically address the sum of these countries
# and add NaN for Taiwan. 

# Loading function for estimating SE with linear regression
source(paste0(path$Subroutines,"/SE_LogRegression.R"))
RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)
data <- SE_LogRegression(data,RSE$Minimum,RSE$Maximum)

data <- select(data,Code,Quantity,SE)
data[nrow(data)+1,] <- c(201,NaN,NaN)

# Check if folder with processed data exists, in case delete and create empty one
path_set <- paste0(path$root,"ProcessedData/",datafeed_name)
if(dir.exists(path_set)) unlink(path_set,recursive = TRUE) 
dir.create(path_set)

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))

for(i in 1:nrow(data))
{ 
   
  reg_num <- data$Code[i] # Get root_code of region
  reg_name <- as.character(root$region$Name[reg_num]) # Read region name
  
  # Write data to processed folder
  # First, value RHS
  export_value <- matrix(c(data$Quantity[i],0),nrow = 1,ncol = 2)
  filename_value <- paste0(datafeed_name,"_Value_",year,"_",reg_name,".csv")
  write.table(export_value,row.names = FALSE,col.names = FALSE,sep = ",",
              file = paste0(path_set,"/",filename_value))  
  
  # Second, standard errors
  export_SE <- matrix(c(data$SE[i],0),nrow = 2,ncol = 1)
  filename_SE <- paste0(datafeed_name,"_SE_",year,"_",reg_name,".csv")
  write.table(export_SE,row.names = FALSE,col.names = FALSE,sep = ",",
              file = paste0(path_set,"/",filename_SE))  
  
  # Read extraction value
  value <- paste0("DATAPATH/",datafeed_name,"/",filename_value)
  # Set SE
  SE <-  paste0("DATAPATH/",datafeed_name,"/",filename_SE) 
  
  if(reg_num == 166) # Former USSR
  {
    # Codes of former Soviet countries:
    aggreg <- c(166,65,119,117,24,205,123,73,10,14,100,209,194,102,193)
    reg_num <- paste(aggreg,collapse = ",")
    
    ALANG <- add_row(ALANG,'1' = paste0("Pauliuk EoL ",reg_name),Value = value,
                     'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
    
  } else if (reg_num == 218) # Former Yugoslavia
  {
    # Codes of countries of former Yugoslavia:
    aggreg <- c(23,86,128,132,178,183)
    reg_num <- paste(aggreg,collapse = ",")
    
    ALANG <- add_row(ALANG,'1' = paste0("Pauliuk EoL ",reg_name),Value = value,
                     'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
     
  } else if (reg_num == 47) # Czechoslovakia
  {
    # Codes of Czechia and SLovakia:
    aggreg <- c(52,182)
    reg_num <- paste(aggreg,collapse = ",")
    
    ALANG <- add_row(ALANG,'1' = paste0("Pauliuk EoL ",reg_name),Value = value,
                     'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
    
  } else if (reg_num == 16) # Belgium-Luxembourg
  {
    
    aggreg <- c(16,118)
    reg_num <- paste(aggreg,collapse = ",")
    
    ALANG <- add_row(ALANG,'1' = paste0("Pauliuk EoL ",reg_name),Value = value,
                     'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
    
  } else if (reg_num == 201) # Missing Taiwan
  {
    # Get root_code of region 
    reg_num <- as.character(reg_num)
    
    ALANG <- add_row(ALANG,'1' = paste0("Pauliuk EoL ",reg_name),Value = value,
                     'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
    
  } else
  {
    reg_num <- as.character(reg_num)
    ALANG <- add_row(ALANG,'1' = paste0("Pauliuk EoL ",reg_name),Value = value,
                     'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)  
  }
}

# Create industry concordance 
max_ind <- length(root$industry$Code)
Concord <-  matrix(0,nrow = 2, ncol = max_ind)
Concord[1,c(64,65)] <- 1
Concord[2,c(1:63,66:max_ind)] <- 1

# Set name and path to concordance and write to folder
Concorda_name <- "EoLPauliuk_Sec_Concordance"
Concord_path <- paste0(path$Concordance,"/",Concorda_name,".csv")
write.table(Concord,file = Concord_path,row.names = FALSE,col.names = FALSE,sep = ",")

# Add path to concordance to ALANG commands
ALANG$`Column grandchild` <- paste0("1:e t2 CONCPATH/",Concorda_name,".csv")

# Add other variables
ALANG$`Row child` <- "3"
ALANG$`Row grandchild` <- "2"
ALANG$`Column child` <- "1"
ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$Incl <- "Y"
ALANG$Parts <- "1"
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
ALANG$Years <- "1"
ALANG$Margin <- "1"
ALANG$Coef1 <- "1"
  
# Call script that writes the ALANG file to the repsective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))
  
print(paste0("datafeed_PIOLab_",datafeed_name," finished."))
  