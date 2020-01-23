#########################################
#                                       #
#     This data feed processes IEA's    #
#     extended World Energy Balances    #
#                                       #
#########################################


IEFeed_PIOLab_IEA <- function(year,path)
{
  print("IEFeed_PIOLab_IEA initiated.")
  # Important note: Data from IEA (which includes all years), due to download threshold, 
  # can not be stored in one file. Therefore before processing, download respective 
  # raw data csv sheet for each year and add appropriate name to the file that includes the year.   
  # Note that energy flows are measured in Tera Joule
  
  # Import raw data, select columns and filter rows that have values in it
  raw_data <- read.csv(paste0(path$Raw,"/IEA/IEAEB_",year,".csv"),stringsAsFactors=FALSE) %>% 
    select(COUNTRY,COUNTRY.1,PRODUCT,PRODUCT.1,FLOW,FLOW.1,Value,Flags)  %>% filter(Value < 0 | Value > 0)
  
  # Load root classification
  root_class <- read.xlsx(paste0(path$Concordance,"/StandardPIOT_RootClassification.xlsx"),sheet = 1) %>% 
    select(RootRegionCode,RootRegionName,ISO3digitCode)
  
  # Look up root classification code
  data <- left_join(raw_data,root_class,by = c("COUNTRY" = "ISO3digitCode"),copy = FALSE) %>% 
    filter(!is.na(RootRegionCode))
  
  # Load region aggregator and look up base table codes
  source(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"))
  reg_agg <- Root2Base_RegionAggregator(paste0(path$Concordance,"/Region Aggregators/StandardPIOT_RegionAggregator.csv"))
  
  data <- left_join(data,reg_agg,by = c("RootRegionCode" = "root"),copy = FALSE) %>% 
    select(base,PRODUCT,PRODUCT.1,FLOW,FLOW.1,Value)
  
  # Aggregate into base table classificiation
  data_clean <- data %>% group_by(base,PRODUCT,PRODUCT.1,FLOW,FLOW.1) %>% 
    summarise(Value = sum(Value)) %>% ungroup(data_clean)
  
  # TBLASTFUR includes the production of recovered gases (e.g. blast furnace gas and
  # oxygen steel furnace gas) whereas EBLASTFUR represents the energy which is used in blast furnaces.
  # The IEA considers the blast furnace to be a transformation process. Inputs are shown with
  # negative signs and outputs with positive
  
  # Select production of blast furnace gas and tranform into GJ
  BF_gas <- filter(data_clean,FLOW == "TBLASTFUR",PRODUCT.1 == "Blast furnace gas") %>%
    select(base,Value)
  BF_gas$Value <- BF_gas$Value * 1000
  
  # OGASES: By-product of the production of steel in an oxygen furnace, recovered on
  # leaving the furnace. The gases are also known as converter gas, LD gas or BOS gas.
  
  # Select production of BOF gas and tranform into GJ
  BOF_gas <- filter(data_clean, PRODUCT == "OGASES",FLOW.1 == "Iron and steel") %>%
    select(base,Value)
  BOF_gas$Value <- BOF_gas$Value * 1000
  
  # Select total energy consumption in the iron and steel sector and transform into GJ
  Energy_Con <- filter(data_clean,FLOW.1 == "Iron and steel",PRODUCT.1 == "Total") %>%
    select(base,Value)
  Energy_Con$Value <- Energy_Con$Value * 1000
  
  # Check if subfolder in processed data exists and if not create it
  path_set <- paste0(path$IE_Processed,"/IEA")
  if(!dir.exists(path_set)) dir.create(path_set)
  
  # Export data to folder (note that all is measured in GJ)
  write.csv(BF_gas,
            file = paste0(path_set,"/IEA_",year,"_BlastFurnaceGas.csv"),
            row.names = FALSE)
  
  write.csv(BOF_gas,
            file = paste0(path_set,"/IEA_",year,"_BlastOxygenFurnaceGas.csv"),
            row.names = FALSE)
  
  write.csv(Energy_Con,
            file = paste0(path_set,"/IEA_",year,"_SteelIndustryEnergyConsumption.csv"),
            row.names = FALSE)
  
  print("IEFeed_PIOLab_IEA finished.")
  
}