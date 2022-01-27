################################################################################
#
# This script includes basic settings and paths 
# that are loaded by other datafeeds and Initial Extimates
#
# Note for HP: Insert code to read root folder from HANDLER variable here

# Load packages

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
# Because the data.table package is not available for the R version currently running on the USYD server
# use the forerunner version of it, that is the reshape2 package, to use its functionalities
#library(data.table)
suppressMessages(library(reshape2))
suppressMessages(library(openxlsx))
suppressMessages(library(stringr))
suppressMessages(library(R.matlab))
# library(ff)

################################################################################
# 1. Set global variables and paths

# Read current export (aka working or mother) directory, for debugging we have the following if-else  
# Note that only when run via the server, a .mat file can be found in the IEfeeds_code folder
OnServer <- paste0(root_folder,"ProcessedData/WorkingDirectory4R.mat")
if(file.exists(OnServer))
{
  # Path to libraries
  .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")
  # Read current working directory
  mother <- readMat(OnServer)
  mother <- c(mother$out)
  # Delete the file
  unlink(OnServer)
} else
{
  if((Sys.info()[1] == "Windows") & (dir.exists("C:/Users/hwieland/Github workspace/PIOLab/")))
  {
    mother <- readMat("C:/Users/hwieland/Github workspace/PIOLab/WorkingDirectory4R.mat")
    mother <- c(mother$out)
  }else if((Sys.info()[1] == "Windows")&(dir.exists("C:/Users/polly/Google Drive/WU/GIT/PIOLab/"))){
    mother <- readMat("C:/Users/polly/Google Drive/WU/GIT/PIOLab/WorkingDirectory4R.mat")
    mother <- c(mother$out)  
  }else if((Sys.info()[1] == "Windows")&(dir.exists("C:/Users/Rene/Google Drive/WU/GIT/PIOLab/"))){
    mother <- readMat("C:/Users/Rene/Google Drive/WU/GIT/PIOLab/WorkingDirectory4R.mat")
    mother <- c(mother$out)  
  }else
  {
    mother <- "xxx"
  }
}

# Set paths to folders
path <- list("Raw" = paste0(root_folder,"RawDataRepository"),
             "Processed" = paste0(root_folder,"ProcessedData"),
             "Concordance" = paste0(root_folder,"ConcordanceLibrary"),
             "ALANG" = paste0(root_folder,"ALANGfiles"),
             "Rscripts" = paste0(root_folder,"Rscripts"),
             "Subroutines" = paste0(root_folder,"Rscripts/Subroutines"),
             "root" = root_folder,
             "mother" = mother,
             "Settings" = paste0(root_folder,"Settings"),
             "RSE_settings" = paste0(root_folder,"Settings/datafeeds_settings/df_RSE_settings.xlsx"))

# Read root region, industry and products

path_rootclass <- paste0(path$Settings,"/Root/Legends/PIOLab_RootClassification.xlsx")

root <- list("region" = read.xlsx(path_rootclass,sheet = 1),
             "process" = read.xlsx(path_rootclass,sheet = 2)[,1:2],
             "flow" = read.xlsx(path_rootclass,sheet = 3),
             "demand" = read.xlsx(path_rootclass,sheet = 4),
             "input" = read.xlsx(path_rootclass,sheet = 5) )



remove(mother,OnServer,root_folder, path_rootclass)
