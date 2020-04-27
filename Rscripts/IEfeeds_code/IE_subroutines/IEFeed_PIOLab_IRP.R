#############################################################
#     This function reads the extraction accounts of IRP    #
#     and saves them as csv files for AISHA                 #
#############################################################


IEFeed_PIOLab_IRP <- function(year,path)
{
  print("IEFeed_PIOLab_IRP initiated.")
  # Load data and filter for iron ores
  
  source(paste0(path$Subroutines,"/Read_ExtractionIRP.R"))
  
  # To aggregate from root to base classification, load region aggregator first
  source(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"))
  reg_agg <- Root2Base_RegionAggregator(R2M$region)
  
  data <- left_join(data,reg_agg,by = c("Code" = "root"),copy = FALSE) %>% select(base,Quantity)
  data <- data %>% group_by(base) %>% summarise(Quantity = sum(Quantity))
    
  # Check if subfolder in processed data exists and if not create it
  path_set <- paste0(path$IE_Processed,"/IRP")
  if(!dir.exists(path_set)) dir.create(path_set)
  
  # Export data to folder (note that the unit is metric tons)
  write.csv(data,file = paste0(path_set,"/IRP_",year,".csv"),row.names = FALSE)
}