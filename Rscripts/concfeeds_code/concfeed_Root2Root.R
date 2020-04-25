################################################################################
# This code creates the Root2Root product, industry and region concordance
# Date: 04.25.2020
# hanspeter.wieland@wu.c.at

print(paste0("Start of Root2Root concfeed."))

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

# Read number of root items
item <- c( "reg" = nrow(root$region), "ind" = nrow(root$process), "pro" = nrow(root$flow) )

R2R <- list("reg" = diag( rep(1,item["reg"]) ),
            "ind" = diag( rep(1,item["ind"]) ),
            "pro" = diag( rep(1,item["pro"]) ) )

# Set filename and path to R2R concordance 
filename <- c("reg" = "/Root2Root_Reg_Concordance.csv",
              "ind" = "/Root2Root_Ind_Concordance.csv",
              "pro" = "/Root2Root_Pro_Concordance.csv" )

filename <- paste0(path$Concordance,filename)

# Write R2R to concordance folders
for(i in 1:3) Numbers2File(R2R[[i]],filename[i])


