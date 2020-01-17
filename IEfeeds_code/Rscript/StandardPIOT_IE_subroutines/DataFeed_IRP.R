#############################################################
#     This function reads the extraction accounts of IRP    #
#     and saves them as csv files for AISHA                 #
#############################################################


DataFeed_IRP <- function(year,path)
{
  print("DataFeed_IRP initiated.")
  # Load data and filter for iron ores
  data <- read.csv(paste0(path$Raw,"/IRP/DE_CCC_ResearchDB.csv"),stringsAsFactors=FALSE)
  colnames(data)[7:50] <- 1970:2013
  data <- data %>% filter(CCC_Name == "Iron ores") %>% select(Country,AlphaNumISO,as.character(year)) 
  
  # transform 3-digit ISO codes and translate into root classification, first load the root
  root_class <- read.xlsx(paste0(path$Concordance,"/StandardPIOT_RootClassification.xlsx"),sheet = 1) %>% 
    select(RootRegionCode,ISO3digitCode)
  
  data <- data %>% separate(AlphaNumISO, into = c('ISOCode', 'num'), sep = 3) %>% select(-num)  
  colnames(data)[ncol(data)] <- "Quantity"
  data <- filter(data,Quantity > 0)
  
  # Look up root codes
  data_clean <- left_join(data,root_class,by = c("ISOCode" = "ISO3digitCode"),copy = FALSE) %>% select(RootRegionCode,Quantity)
  
  # Remove Yugoslavia (242) and USSR (233) from data
  data_clean <- data_clean %>% filter(!RootRegionCode %in% c(233,242)) 
  
  # To aggregate from root to base classification, load region aggregator first
  source(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"))
  reg_agg <- Root2Base_RegionAggregator(paste0(path$Concordance,"/Region Aggregators/StandardPIOT_RegionAggregator.csv"))
  
  data_clean <- left_join(data_clean,reg_agg,by = c("RootRegionCode" = "root"),copy = FALSE) %>% select(base,Quantity)
  data_clean <- data_clean %>% group_by(base) %>% summarise(Quantity = sum(Quantity))
    
  # Check if subfolder in processed data exists and if not create it
  path_set <- paste0(path$Processed,"/IRP")
  if(!dir.exists(path_set)) dir.create(path_set)
  
  # Export data to folder (note that the unit is metric tons)
  write.csv(data_clean,
            file = paste0(path_set,"/IRP_",year,".csv"),
            row.names = FALSE)
}