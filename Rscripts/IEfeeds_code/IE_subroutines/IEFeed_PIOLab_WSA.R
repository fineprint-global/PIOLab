#########################################
#                                       #
# This data feed processes the raw data #
#     of the Worldsteel Yearbooks       #
#                                       #
#########################################


IEFeed_PIOLab_WSA <- function(year,path)
{
  print("IEFeed_PIOLab_WSA initiated.")
  
  # Load specific yearbook
  source(paste0(path$Subroutines,"/Load_YearbookWSA.R"))
  
  # Load function to align data with root classification
  source(paste0(path$Subroutines,"/Read_ProductionWSA.R"))
  
  # Load region aggregator and look up base table codes
  source(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"))
  reg_agg <- Root2Base_RegionAggregator(RegionAggregator)
  
  # Loop over selected items 
  for(i in 1:length(items))
  {
    item_page <- items[[i]]$page
    item_name <- items[[i]]$name
    
    # Read values and align with root classification
    data_clean <- Read_ProductionWSA(path,year,item_page,yb,concord)
      
    data_clean <- left_join(data_clean,reg_agg,by = c("Code" = "root"),copy = FALSE) %>% select(base,Quantity)
    data_clean <- data_clean %>% group_by(base) %>% summarise(Quantity = sum(Quantity))
    
    # Check if subfolder in processed data exists and if not create it
    path_set <- paste0(path$IE_Processed,"/WSA")
    if(!dir.exists(path_set)) dir.create(path_set)
    
    # Export data to folder (note that the unit is metric tons)
    write.csv(data_clean,
              file = paste0(path_set,"/WSA_",year,"_",item_name,".csv"),
              row.names = FALSE)
  }
  print("IEFeed_PIOLab_WSA finished.")
}
