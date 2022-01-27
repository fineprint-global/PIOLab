################################################################################
#
datafeed_name <- "BACI"

# Determine loaction of root folder
################################################################################

# Set library path depending on whether data feed runs on Uni Sydney server or local
if(Sys.info()[1] == "Linux")
{
  # Define location of root directory on the Uni Sydney server:
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"
  
  if(dir.exists(root_folder))
  {
    # Setting the R package library folder on Uni Sydney server
    .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")  
  } else{
    # Define location of root directory on the WU Vienna server:
    root_folder <- "/data/WULab/Roots/PIOLab/"
  }
  
  
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

path[["df_Processed"]] <- paste0(path$Processed,"/",datafeed_name)
path["df_Subroutines"] <- paste0(path$Rscripts,"/datafeeds_code/datafeed_subroutines/") 
path$ALANG <- paste0(path$ALANG,"/",datafeed_name)  # Set path to specific ALANG folder

source(paste0(path$Subroutines,"/Numbers2File.R"))  # Load fun. to write arrays to files

# Call script to clear ALANG and processed data folders of the present data feed
source( paste0( path$df_Subroutines,"ClearFolders.R" ) )


### Ad-hoc trial to filter only non-RoW regions for feeding ###
IEdatafeed_name <- "Ind30Pro39v1"
test_regagg <- "049"
source(paste0(path$Subroutines,"/Read_BaseClassification.R"))

year <- 2008

# for(year in 2008:2017)
# {
  # Create folder for present year
  dir.create( paste0( path$df_Processed,"/", year,"/") )
  
  source(paste0(path$Subroutines,"/Load_BACI.R")) # Loading raw data
  
  # Set variables
  reg_max <- nrow(root$region)
  pro_max <- nrow(root$flow)
  n_yea <- "1"
  n_she <- "1"
  
  # Read trade pairs
  
  index_all <- select(data,From,To,Quantity) %>% group_by(From,To) %>% 
    summarise(Q = sum(Quantity)) %>% ungroup(From,To) %>% select(From,To)
  
  # For testing select only non-RoW regions
  e <- root$region %>% filter( ISO2digitCode %in% base$region$Abbrev ) %>% pull(Code)
    
  index_NonRoW <- index_all %>% filter(From %in% e & To %in% e)  # Select trade pairs for d
    
  # index_NonRoW <- index_all
    
  for( d in unique(index_NonRoW$From) )
  {
  
    reg_abbr <- root$region$RootCountryAbbreviation[d]
    
    # Create folder for region d data
    dir.create( paste0( path$df_Processed,"/", year,"/", reg_abbr) )   
    
    # Select export of region d
    index <- index_NonRoW %>% filter(From == d)
    
    # Create empty ALANG table with header
    source(paste0(path$Subroutines,"/makeALANGheadline.R"))
  
    # Set length of ALANG file
    ALANG_new <- as.data.frame( matrix(NA,nrow = nrow(index) ,ncol = ncol(ALANG) ) )
    colnames(ALANG_new) <- colnames(ALANG)
    ALANG <- ALANG_new
    remove(ALANG_new)
    
    # Prepare ALANG sheet for adding raw data:
    
    ALANG$`Row parent` <- reg_abbr
    ALANG$`Column parent` <- root$region$RootCountryAbbreviation[ index$To ]
    ALANG$`Row child` <- 2
    ALANG$`Row grandchild` <- "1:e t2 CONCPATH/Root2Root_Pro_Concordance.csv"
    ALANG$`Column child` <- 1
    ALANG$`Column grandchild` <- "1-e"
    
    ALANG$Coef1 <- 1
    ALANG$Incl <- "Y"
    
    ALANG$Parts <- 1
    ALANG$Years <- n_yea
    ALANG$Margin <- n_she
    
    ALANG$`Pre-map` <- ""
    ALANG$`Post-map` <-  ""
    ALANG$`Pre-Map` <- ""
    ALANG$`Post-Map` <- ""
    
    # read upper and lower error bounds from settings file
    RSE <- filter(read.xlsx(path$RSE_settings),Item == datafeed_name)
    
    ALANG$S.E. <- paste0("E MX",RSE$Maximum,";MN",RSE$Minimum,";")
    
    ALANG$`1` <- paste0( year,"_BACI_RHS_", ALANG$`Row parent` ,
                         "_to_", ALANG$`Column parent` )
    
    ALANG$`#` <- 1:nrow(ALANG)             # Add index
    
    # Write datapath into AISHA command:
    ALANG$Value <- paste0("DATAPATH/",datafeed_name,"/",year,"/",reg_abbr,"/",ALANG$`1`,".csv")
    
    vec <- data.frame( "pro" = 1:pro_max, "RHS" = NA )
    
    # vec$RHS[256:266] <- NA
    
    ### 2. Write commands by looping over export regions:
    
    for( i in 1:nrow(index) )
    {
      sel <- filter(data, From == index$From[i], To == index$To[i] ) # Select exporter
      
      v <- vec  # Create empty object for data storage
      
      v$RHS[sel$Product] <- sel$Quantity
      
      # Set filenames:
      filename <- paste0(path$df_Processed,"/",year,"/",reg_abbr,"/",ALANG$`1`[i],".csv")
      
      # Write values as row vectors to file:
      Numbers2File( table = t( v$RHS ) , filename )
    }
    
    ALANG[] <- lapply(ALANG,as.character)  # Convert all entries to character
    
    ### 3. Call script that writes the ALANG file to the repsective folder in the root
    filename <-  paste0(path$ALANG,"/",gsub("-","",Sys.Date()),
                        "_PIOLab_",root$region$RootCountryAbbreviation[d],"_000_Constraints-",
                        year,"_000_",datafeed_name,".txt")
    print(filename)  
    
    write.table(ALANG,file = filename, row.names = FALSE, quote = FALSE, sep = "\t")
  
  }
  
# }

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

rm(list = ls()) # clear workspace
