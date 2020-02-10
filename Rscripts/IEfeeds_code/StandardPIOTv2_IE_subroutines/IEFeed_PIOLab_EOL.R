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
  # Import raw data from Pauiuk and filter for years selected
  data <- read.xlsx(paste0(path$Raw,"/EOL/1_F_steel_200R_F_13_14_inflow_waste_mgt.xlsx"),
                    sheet = 2)[,c("aspect.6.:.destination_region","aspect.7.:.time","value")]
  colnames(data) <- c("EOL","Year","Quantity")
  data <- filter(data,Year == year) %>% select(EOL,Quantity)
  
  # Transform from Gg (=kt) to metric tons
  data$Quantity <- data$Quantity * 1000
  
  # ABOUT region names and USSR & Russia
  # Please note that Pauliuk, assumingly because it is a 100+ years time series, 
  # does not include Russia but instead "Former USSR". The concordance between EOLRegions 
  # and the Root regions includes this link, meaning what Pauliuk calls USSR is in our data Russia. 
  # However, what Pauliuk calls "Other" is not included in the concordance and must be added 
  # to RoW in the code here
  
  # Load EOL-root region concordance 
  root_class <- read.xlsx(paste0(path$Concordance,"/EOL/EOL_RegionConcordance.xlsx"),sheet = 1) %>% 
    filter(!is.na(EOL))
  
  # Look up root table codes. Note that "Others" is still NA
  data <- left_join(data,root_class,by = c("EOL"),copy = FALSE)
  
  # Load region aggregator
  source(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"))
  reg_agg <- Root2Base_RegionAggregator(IEdatafeed_name)
  
  # Look up base table codes
  data <- left_join(data,reg_agg,by = c("Code" = "root"),copy = FALSE) 
  # Add "Other" to ROW
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


