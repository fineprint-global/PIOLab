################################################################################
################################################################################
#
datafeed_name <- "BACI"
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

# Loading raw data
source(paste0(path$Subroutines,"/Load_BACI.R"))

# Load function to calculate standard errors 
source(paste0(path$Subroutines,"/SE_LogRegression.R"))
RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)
data <- SE_LogRegression(data,RSE$Minimum,RSE$Maximum)
data <- select(data,-value)

# Set variables
reg_max <- nrow(root$region)
n_yea <- as.character(year-2007)
n_she <- "1"
n_pro <- nrow(root$product)
n_ind <- nrow(root$industry)

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))

# Check if folder with processed data exists, in case delete and create empty one
path_set <- paste0(path$root,"ProcessedData/",datafeed_name)
if(dir.exists(path_set)) unlink(path_set,recursive = TRUE) 
dir.create(path_set)

mat <- matrix(0,nrow = n_pro,ncol = 1) # Empty matrix to put numbers in

a <- 1 # Set starting value for alang line index
for(i in unique(data$From)) # Loop over the exporting regions
{
  data_sel <- filter(data,From == i)  # Filter exporting region
  
  for(j in unique(data_sel$To)) # Loop over trade partners
  {
    # Add empty line with tag
    ALANG <- add_row(ALANG,'1' = paste0("DataFeed BACI from ",root$region$Name[i]," to ",root$region$Name[j]))
    
    trader_sel <- filter(data_sel,To == j) %>% select(-From,-To) # select data for trade partners
    
    values <- mat # Create empty column vector 
    values[trader_sel$Product,1] <- trader_sel$Quantity  # Write values
    
    filename_value <- paste0("BACI_",year,"_Values_",root$region$Name[i],"-",
                       root$region$Name[j],".csv")      # Set name of the file

    write.table(values,row.names = FALSE,col.names = FALSE, sep = ",",
                file = paste0(path_set,"/",filename_value))   # Write array to folder
    
    SE <- mat # Create empty column vector and write SE
    
    SE[trader_sel$Product,1] <- trader_sel$SE # Write SE numbers into array
    
    filename_SE <- paste0("BACI_",year,"_SE_",root$region$Name[i],"-",
                       root$region$Name[j],".csv")      # Set name of the file
    
    write.table(SE,row.names = FALSE,col.names = FALSE, sep = ",",
                file = paste0(path_set,"/",filename_SE))   # Write array to folder
    
    ALANG$Value[a] <- paste0("DATAPATH/",filename_value)
    ALANG$S.E.[a] <- paste0("DATAPATH/",filename_SE)

    ALANG$`Row parent`[a] <- as.character(i)
    ALANG$`Column parent`[a] <- as.character(j)
    
    a <- a + 1  # Increase ALANG line index
  }
}

# Add commands
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
ALANG$Coef1 <- "1"
ALANG$Incl <- "Y"
ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$Parts <- "1"
ALANG$Years <- n_yea
ALANG$Margin <- n_she
ALANG$`Row child` <- "2"
ALANG$`Row grandchild` <- "1:e"
ALANG$`Column child` <- "1"

# # Create and write sector concordance to file
# Concord <- matrix(1,nrow = n_pro,ncol = n_ind) # block matrix required for aggregation
# 
# # Set name and path to concordance and write to folder
# Concorda_name <- "BACI_Sec_Concordance"
# Concord_path <- paste0(path$Concordance,"/",Concorda_name,".csv")
# write.table(Concord,file = Concord_path,row.names = FALSE,col.names = FALSE,sep = ",")

# Add path to concordance to ALANG commands
#ALANG$`Column grandchild` <- paste0("1-e t2 CONCPATH/",Concorda_name,".csv")
ALANG$`Column grandchild` <- "1-e"

# Call script that writes the ALANG file to the repsective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))


