################################################################################
# This syntax reads WSA Source2Root concordances for products and industries
# from a xlsx file and writes them clean into a csv file
# Date: 04.20.2020
# hanspeter.wieland@wu.c.at

print(paste0("Start of Source2Root WSA sector concfeed."))

# Set library path when running on suphys server
if(Sys.info()[1] == "Linux"){
  .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")
  # Define location for root directory
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"}else{
    root_folder <- "C:/Users/hwieland/Github workspace/PIOLab/"}

# Initializing R script (load R packages and set paths to folders etc.)
source(paste0(root_folder,"Rscripts/Subroutines/InitializationR.R"))

# Load function to write tables to file
source(paste0(path$root,"Rscripts/Subroutines/Numbers2File.R"))

S2R <- list("process" =  read.xlsx(paste0(path$Concordance,"/WSA/WSA_Source2Root_WithLabelsV4.xlsx"), sheet = 1),
            "flow" = read.xlsx(paste0(path$Concordance,"/WSA/WSA_Source2Root_WithLabelsV4.xlsx"), sheet = 2) )

# Read WSA settings to get exact number of feeds
Settings <- read.xlsx(paste0(path$Settings,"/datafeeds_settings/WSA_settings.xlsx"))

# Select range of the binary concordance and transpose to source to root format
S2R$process <- t( S2R$process[1:nrow(root$process),Settings$FeedName] )
S2R$flow <- t( S2R$flow[1:nrow(root$flow),Settings$FeedName] )

filename <- paste0(path$Concordance,"/WSA/",gsub("-","",Sys.Date()),
                   "_WSA_Source2Root_Industry.csv")

Numbers2File(S2R$process,filename)  # Write R2M for flows to folder in concordance library

filename <- paste0(path$Concordance,"/WSA/",gsub("-","",Sys.Date()),
                   "_WSA_Source2Root_Product.csv")

Numbers2File(S2R$flow,filename)  # Write R2M for flows to folder in concordance library

