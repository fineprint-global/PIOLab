#############################################################
#     This function reads the iron and steel trade data     #
#     from BACI and writes it into a csv file for AISHA     #
#############################################################

IEFeed_PIOLab_BACI <- function(year,path)
{
  print("IEFeed_PIOLab_BACI initiated.")
  
  # Load function that aligns raw data with root classification
  source(paste0(path$Subroutines,"/Load_BACI.R"))
  # Load region aggregator
  source(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"))
  reg_agg <- Root2Base_RegionAggregator(IEdatafeed_name)
  # Load product aggregation matrix
  source(paste0(path$Subroutines,"/Root2Base_ProductAggregator.R"))
  prod_agg <- Root2Base_ProductAggregator(IEdatafeed_name)
  
  # Aggregate trade data from the root (245 reg and 266 prod) to the base classification
  data_clean <- left_join(data,reg_agg,by = c("From" = "root"),copy = FALSE)
  data_clean <- left_join(data_clean,reg_agg,by = c("To" = "root"),copy = FALSE) 
  data_clean <- left_join(data_clean,prod_agg,by = c("Product" = "root"),copy = FALSE) 
  data_clean <- select(data_clean,base.x,base.y,base,quantity)
  colnames(data_clean)[1:3] <- c("From","To","Product")
  data_clean <- data_clean %>% group_by(From,To,Product) %>% summarise(quantity = sum(quantity)) %>%
    ungroup(From,To,Product)
  
  # Remove trade flows between RoW countries
  data_clean <- data_clean %>% filter(From != To)
  
  # Check if subfolder in processed data exists and if not create it
  path_set <- paste0(path$IE_Processed,"/BACI/")
  if(!dir.exists(path_set)) dir.create(path_set)
  
  # Write clean data to target folder
  write.csv(data_clean,file = paste0(path_set,"BACI_",year,".csv"),row.names = FALSE)
    
  # Note that trade flows are measured in metric tons of material 
  
  print("IEFeed_PIOLab_BACI finished.")
}



