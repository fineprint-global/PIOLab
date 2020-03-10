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

# Load function to write arrays to files
source(paste0(path$Subroutines,"/Numbers2File.R"))

# Loading raw data
source(paste0(path$Subroutines,"/Load_BACI.R"))

# Load function to calculate standard errors 
source(paste0(path$Subroutines,"/SE_LogRegression.R"))
RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)
data <- SE_LogRegression(data,RSE$Minimum,RSE$Maximum)
data <- select(data,-value)

# Set variables
n_reg <- nrow(root$region)
n_yea <- as.character(year-2007)
n_she <- "1"
n_pro <- nrow(root$product)
n_ind <- nrow(root$industry)

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))

# Check if folder with processed data exists, in case delete and create empty one
if(dir.exists(path$df_Processed)) unlink(path$df_Processed,recursive = TRUE) 
dir.create(path$df_Processed)

a <- 1 # Set starting value for alang line index

for(i in unique(data$From)) # Loop over the exporting regions
{
  sel <- list("data" = filter(data,From == i))  # Filter exporting region
  
  for(j in unique(sel$data$To)) # Loop over trade partners
  {
    # Add empty line with tag
    ALANG <- add_row(ALANG,'1' = paste0("DataFeed BACI from ",root$region$Name[i]," to ",root$region$Name[j]))
    
    # select data for trade partners:
    sel[["trader"]] <- filter(sel$data,To == j) %>% select(Product,Quantity,SE) 
    
    df <- data.frame("RHS" = rep(0,n_pro),"SE" = rep(0,n_pro)) # Create empty data frame
    
    df$RHS[sel$trader$Product] <- sel$trader$Quantity # Write values
    df$SE[sel$trader$Product] <- sel$trader$SE # Write standard errors
    
    # Set name of the files:
    filename <- list("RHS" = paste0("/BACI/BACI_",year,"_RHS_",root$region$Name[i],
                                    "-",root$region$Name[j],".csv"),
                     "SE" = paste0("/BACI/BACI_",year,"_SE_",root$region$Name[i],"-",
                                   root$region$Name[j],".csv")) 
    
    # Write data to file:
    Numbers2File(df$RHS,paste0(path$Processed,filename$RHS))
    Numbers2File(df$SE,paste0(path$Processed,filename$SE))
    
    ALANG$Value[a] <- paste0("DATAPATH",filename$RHS)
    ALANG$S.E.[a] <- paste0("DATAPATH",filename$SE)

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
ALANG$`Column grandchild` <- "1-e"

# Call script that writes the ALANG file to the repsective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))


