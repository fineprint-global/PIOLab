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

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))

# Check if folder with processed data exists, in case delete and create empty one
path_set <- paste0(path$root,"ProcessedData/",datafeed_name)
if(dir.exists(path_set)) unlink(path_set,recursive = TRUE) 
dir.create(path_set)


################################################################################
# Add lines for regions with non-zero data

for(i in 1:nrow(data))
{ 
  # Get root_code of region 
  reg_num <- data$Code[i]
  reg_name <- as.character(root$region$Name[reg_num])
  reg_num <- as.character(reg_num)
  
  # Write data to processed folder
  # First, value RHS
  export_value <- matrix(c(data$Quantity[i],0),nrow = 2,ncol = 1)
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
  
  # Add command for domestic Use table
  ALANG <- add_row(ALANG,'1' = paste0("DataFeed IRP Extraction ",reg_name),
                   Value = value,'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
}

################################################################################
# Add zero for all other regions

AllOtherRegions <- setdiff(root$region$Code,data$Code)

export <- matrix(0,nrow = 2,ncol = 1) # set values
filename <- paste0(datafeed_name,"_Value&SE_",year,"_AllOtherRegions.csv")
SE <- value <- paste0("DATAPATH/",datafeed_name,"/",filename)

# Write values/SE to file
write.table(export,row.names = FALSE,col.names = FALSE,sep = ",",
            file = paste0(path_set,"/",filename))

for(i in AllOtherRegions)
{ 
  # Get root_code of region 
  reg_num <- i
  reg_name <- as.character(root$region$Name[reg_num])
  reg_num <- as.character(reg_num)
  
  # Add command for domestic Use table
  ALANG <- add_row(ALANG,'1' = paste0("DataFeed IRP Extraction ",reg_name),
                   Value = value,'Row parent' = reg_num,'Column parent' = reg_num,S.E. = SE)
}

################################################################################

# Create industry concordance 
max_ind <- length(root$industry$Code)
Concord <-  matrix(0,nrow = 2, ncol = max_ind)
Concord[1,1:4] <- 1
Concord[2,5:max_ind] <- 1

# Set name and path to concordance and write to folder
Concorda_name <- "IRPextraction_Sec_Concordance"
Concord_path <- paste0(path$Concordance,"/",Concorda_name,".csv")
write.table(Concord,file = Concord_path,row.names = FALSE,col.names = FALSE,sep = ",")

# Add path to concordance to ALANG commands
ALANG$`Column grandchild` <- paste0("1:e a CONCPATH/",Concorda_name,".csv")

# Add other variables for regions with data
ALANG$`Row child` <- "3"
ALANG$`Row grandchild` <- "1"
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
 
