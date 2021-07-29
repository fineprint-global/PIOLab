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
source(paste0(root_folder,"Rscripts/Subroutines/Load_PANJIVA_one_file.R"))  

#for all 4 digit HS Codes 
for (HS_2dig_now in products_class[1]){
  for (HS_4dig_now in get (paste0("products_", HS_2dig_now))){
    filename <- paste0(root_folder,"RawDataRepository/PAN/",HS_2dig_now,"_",year,".xlsx")
    sheet1 <- data.frame(details_Port_Lad(filename)[1])
    sheet2 <- data.frame(details_Port_Lad(filename)[2])
    sheet3 <- data.frame(details_Port_Lad(filename)[3])
    sheet4 <- data.frame(details_Port_Lad(filename)[4])
    sheet5 <- data.frame(details_Port_Lad(filename)[5])
    
    sheet1u <- data.frame(details_Port_Unlad(filename)[1])
    sheet2u <- data.frame(details_Port_Unlad(filename)[2])
    
    if(HS_4dig_now==products_26[1]){ 
      sheet1_write <- sheet1
      sheet2_write <- sheet2
      sheet3_write <- sheet3
      sheet4_write <- sheet4
      sheet5_write <- sheet5
      
      sheet1u_write <- sheet1u
      sheet2u_write <- sheet2u
    }
    else{
      sheet1_write <- rbind(sheet1_write,sheet1)
      sheet2_write <- rbind(sheet2_write,sheet2)
      sheet3_write <- rbind(sheet3_write,sheet3)
      sheet4_write <- rbind(sheet4_write,sheet4)
      sheet5_write <- rbind(sheet5_write,sheet5)
      
      sheet1u_write <- rbind(sheet1u_write,sheet1u)
      sheet2u_write <- rbind(sheet2u_write,sheet2u)
    }
  }
}
sheet1_write
sheet2_write
sheet3_write
sheet4_write
sheet5_write

sheet1u_write
sheet2u_write

names(sheet1_write) <- c("HS Code", countries)
names(sheet2_write) <- c("HS Code", countries)
names(sheet3_write) <- c("HS Code", countries)
names(sheet4_write) <- c("HS Code", countries)
names(sheet5_write) <- c("HS Code", countries)

names(sheet1u_write) <- c("HS Code", countries)
names(sheet2u_write) <- c("HS Code", countries)

rnames <- c("NumberOfUniquePortsUnlading", "%ofNAforPortsLading","4dig%ofshipments & 6dig%of4dig","AppearenceInMultipleHScodes", "AppearenceInSingleHScodes")
rnamesu <- c("NumberOfUniquePortsUnlading", "%ofNAforPortsUnlading")


write.xlsx(sheet1_write, file=paste0(root_folder,"Panjiva/","Panjiva_overview_PL_",year,".xlsx"),col.names = T, sheetName=rnames[1], row.names=FALSE)
write.xlsx(sheet2_write, file=paste0(root_folder,"Panjiva/","Panjiva_overview_PL_",year,".xlsx"),col.names = T, sheetName=rnames[2], row.names=FALSE, append=T)
write.xlsx(sheet3_write, file=paste0(root_folder,"Panjiva/","Panjiva_overview_PL_",year,".xlsx"),col.names = T, sheetName=rnames[3], row.names=FALSE, append=T)
#write.xlsx(sheet4_write, file=paste0(root_folder,"Panjiva/","Panjiva_overview_PL_",year,".xlsx"),col.names = T, sheetName=rnames[4], row.names=FALSE, append=T)
#write.xlsx(sheet5_write, file=paste0(root_folder,"Panjiva/","Panjiva_overview_PL_",year,".xlsx"),col.names = T, sheetName=rnames[5], row.names=FALSE, append=T)


write.xlsx(sheet1u_write, file=paste0(root_folder,"Panjiva/","Panjiva_overview_PU_",year,".xlsx"),col.names = T, sheetName=rnamesu[1], row.names=FALSE)
write.xlsx(sheet2u_write, file=paste0(root_folder,"Panjiva/","Panjiva_overview_PU_",year,".xlsx"),col.names = T, sheetName=rnamesu[2], row.names=FALSE, append=T)

#filename <- paste0(root_folder,"RawDataRepository/PAN/","26_",year,".xlsx")
#details_HS_whole(filename)

rm(list = ls()) # clear workspace
