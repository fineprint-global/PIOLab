####################################
# datafeed_PIOLab_balancing
#
# This data feed formulates the ALANG commands for the balancing condition
# for both industries and products
#

datafeed_name <- "balancing"
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

# Check if ALANG folder exists and create new empty folder for storage
path_set <- paste0(path$root,"ALANGfiles/",datafeed_name)
if(dir.exists(path_set)) unlink(path_set,recursive = TRUE) 
dir.create(path_set)

for (year in 1970:2017){

source(paste0(path$Subroutines,"/makeALANGheadline.R"))  # Create ALANG header

ALANG <- ALANG[,c(1:19,11:19)]  # Extend table with additional columns

# Balancing industries
ALANG <- add_row(ALANG,'1' = "Balancing industries",
                 Coef1 = "1",'Row parent' = "1-e",'Row child' = "1-e",'Row grandchild' = "1-e",
                 'Column parent' = "1:e",'Column child' = "1",'Column grandchild' = "1:e",
                 'Coef1.1' = "-1",'Row parent.1' = "1:e",'Row child.1' = "1",'Row grandchild.1' = "1:e",
                 'Column parent.1' = "1-e",'Column child.1' = "1-e",'Column grandchild.1' = "1-e")
  
# Balancing products
ALANG <- add_row(ALANG,'1' = "Balancing product markets",
                 Coef1 = "1",'Row parent' = "1:e",'Row child' = "1",'Row grandchild' = "1-e",
                 'Column parent' = "1:e~3",'Column child' = "2",'Column grandchild' = "1:e",
                 'Coef1.1' = "-1",'Row parent.1' = "1:e",'Row child.1' = "2",'Row grandchild.1' = "1:e",
                 'Column parent.1' = "1-e",'Column child.1' = "1-e",'Column grandchild.1' = "1-e")  


# Add other variables
ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$Incl <- "Y"
ALANG$Parts <- "2"
ALANG$Value <- "0"
ALANG$S.E. <- "0"
ALANG$Years <- ALANG$Years.1 <- "1"
ALANG$Margin <- ALANG$Margin.1 <- "1"
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
  
# Call script that writes the ALANG file to the respective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))
}
print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

rm(list = ls()) # clear workspace

