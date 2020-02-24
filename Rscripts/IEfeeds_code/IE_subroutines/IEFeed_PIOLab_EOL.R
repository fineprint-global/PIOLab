###############################################
#                                             #
#   This data feed processes the end-of-life  #
#     estimates from Pauliuk and Krausmann    #
#                                             #
###############################################

# Note that krausmann is only needed now for 2009+

IEFeed_PIOLab_EOL <- function(year,path)
{
  print("IEFeed_PIOLab_EOL initiated.")

  # Load Pauliuk data in root classification 
  source(paste0(path$Subroutines,"/Load_PauliukEoL.R"))
  
  # Load region aggregator
  source(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"))
  reg_agg <- Root2Base_RegionAggregator(RegionAggregator)
  
  # Look up base table codes
  data <- left_join(data,reg_agg,by = c("Code" = "root"),copy = FALSE) 
  # Add "Other" to last ROW
  data[data$EOL == "Other","base"] <- nrow(base$region)
  # Filter and aggregate
  data_clean <- data %>% select(base,Quantity) %>% group_by(base) %>% summarise(Quantity = sum(Quantity))
  
  # Check if subfolder in processed data exists and if not create it
  path_set <- paste0(path$IE_Processed,"/EOL")
  if(!dir.exists(path_set)) dir.create(path_set)
  
  write.csv(data_clean,
            file = paste0(path_set,"/EOL_",year,".csv"),
            row.names = FALSE)
  
  print("IEFeed_PIOLab_EOL finished.")
  
}


