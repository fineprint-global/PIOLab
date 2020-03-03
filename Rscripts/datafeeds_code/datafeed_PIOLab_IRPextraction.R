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
  
# Loading raw data
source(paste0(path$Subroutines,"/Read_ExtractionIRP.R"))

# Loading function for estimating SE with linear regression
source(paste0(path$Subroutines,"/SE_LogRegression.R"))
RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)
data <- SE_LogRegression(data,RSE$Minimum,RSE$Maximum)

# Create identity matrix to be used as concordance when doing t command 
Concord <- diag(length(root$region$Code))

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))
# Extend table with additional columns

for(i in 1:nrow(data))
{ 
  # Get root_code of region 
  reg_num <- data$Code[i]
  reg_name <- as.character(root$region$Name[reg_num])
  reg_num <- as.character(reg_num)
  
  # Read extraction value
  value <- paste0("[",data$Quantity[i],";","NaN","]")
  # Set SE
  SE <-  paste0("[",as.character(data$SE[i]),";","NaN","]") 
  
  # Add command for domestic Use table
  ALANG <- add_row(ALANG,'1' = paste0("DataFeed IRP Extraction ",reg_name),
                   Value = value,'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
}

# Add NaN to all other regions
for(i in setdiff(root$region$Code,data$Code))
{ 
  # Get root_code of region 
  reg_num <- root$region$Code[i]
  reg_name <- as.character(root$region$Name[reg_num])
  reg_num <- as.character(reg_num)
  
  # Read extraction value
  value <- SE <- paste0("[NaN;NaN]")
  
  # Add command for domestic Use table
  ALANG <- add_row(ALANG,'1' = paste0("DataFeed IRP Extraction ",reg_name),
                   Value = value,'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
}

# Add other variables

# Create industry concordance 
max_ind <- length(root$industry$Code)
Concord <-  matrix(0,nrow = 2, ncol = max_ind)
Concord[1,1:4] <- 1
Concord[2,5:max_ind] <- 1

# Write to folder
Concord_path <- paste0(path$Concordance,"/IRPextraction_Sec_Concordance.csv")
write.table(Concord,file = Concord_path,row.names = FALSE,col.names = FALSE,sep = ",")

ALANG$`Row child` <- "3"
ALANG$`Row grandchild` <- "1"
ALANG$`Column child` <- "1"
ALANG$`Column grandchild` <- "1:e t1 CONCPATH/IRPextraction_Sec_Concordance.csv"
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
 
