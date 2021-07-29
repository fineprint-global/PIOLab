options(warn=-1)
################################################################################
datafeed_name <- "PANJIVA"
print(paste0("datafeed_PIOLab_",datafeed_name," initiated."))
######################################################################
library(readxl)
library(xlsx)
library(stringr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(magrittr)
# Determine loaction of root folder
################################################################################
#install.packages("xlsx", INSTALL_opts=c("--no-multiarch"))
#install.packages("xlsx")



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
#set year
year <- 2016
#init load file with functions and variables
source(paste0(root_folder,"Rscripts/Subroutines/Load_PANJIVA_one file.R"))  
#define 2digit HS codes
HS_4dig_now <- products_26[2]
#HS_2dig_now <- products_class[1]
#filename <- paste0(root_folder,"RawDataRepository/PAN/",HS_2dig_now,"_",year,".xlsx")
#details_Port_Lad(filename)
#for all 4 digit HS Codes 
for (HS_2dig_now in products_class){
  for (HS_4dig_now in get (paste0("products_", HS_2dig_now))){
    filename <- paste0(root_folder,"RawDataRepository/PAN/",HS_2dig_now,"_",year,".xlsx")
    sheet1 <- details_Port_Lad(filename)
    if(HS_4dig_now==products_26[1]){ 
      sheet1_write <- sheet1
    }
    else{
      sheet1_write <- rbind(sheet1_write,sheet1)
    }
  }
}
SHEET1 <- sheet1_write
names(SHEET1) <- c("HS Code", countries)
write.xlsx(SHEET1, file=paste0(root_folder,"Panjiva/","Panjiva_overview_",year,".xlsx"),col.names = T, sheetName=rnames[1], row.names=FALSE)
rm(list = ls()) # clear workspace
