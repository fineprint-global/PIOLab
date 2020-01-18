###################################################
#                                                 #
#   This data feed processes ore grade factors    #
#   to be used in the IELab for the StandardPIOT  #
#                                                 #
###################################################

DataFeed_Grades <- function(path)
{
  print("DataFeed_Grades initiated.")
  # Load raw data and filter for iron ore grades
  data <- read.csv(paste0(path$Raw,"/Grades/WU_metal_concentrations_unitf_usedf_20180622.csv"),
                   sep = ";") %>% select(Commodity.Name,ISOAlpha.2,Concentration) %>% 
    filter(Commodity.Name == 'Iron ores') %>% select(ISOAlpha.2,Concentration) 
  
  # According to the data set the global average is 0.46. Use this for RoW regions
  global_average <- 0.46

  # Load root classification and use 2-digit ISOCode to look-up country codes
  root_class <- read.xlsx(paste0(path$Concordance,"/StandardPIOT_RootClassification.xlsx"),sheet = 1)  %>% 
    select(RootRegionCode,ISO2digitCode) 
  # Warning messages are turned off for the following join
  options(warn = -1)
  data <- left_join(data,root_class,by = c("ISOAlpha.2" = "ISO2digitCode"),copy = FALSE) %>% 
    select(RootRegionCode, Concentration)
  # Turned on again
  options(warn = 0)
  # Load region aggregator and filter for all non-RoW countries
  source(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"))
  reg_agg <- Root2Base_RegionAggregator(paste0(path$Concordance,"/Region Aggregators/StandardPIOT_RegionAggregator.csv")) 
  reg_agg <- reg_agg %>% filter(base != max(reg_agg$base))
  
  # Look-up base classification codes
  data_clean <- left_join(data,reg_agg,by = c("RootRegionCode" = "root"),copy = FALSE) %>% 
    filter(!is.na(base)) %>% select(base,Concentration)
  
  # Add RoW ore grade entry 
  data_clean <- add_row(data_clean,base = 35,Concentration = global_average)
  
  # Check if subfolder in processed data exists and if not create it
  path_set <- paste0(path$Processed,"/Grades")
  if(!dir.exists(path_set)) dir.create(path_set)
  
  write.csv(data_clean,
            file = paste0(path_set,"/IronOreGrades.csv"),
            row.names = FALSE)
}