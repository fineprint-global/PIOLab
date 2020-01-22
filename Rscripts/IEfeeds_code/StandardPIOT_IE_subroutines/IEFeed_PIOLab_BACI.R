#############################################################
#     This function reads the iron and steel trade data     #
#     from BACI and writes it into a csv file for AISHA     #
#############################################################

IEFeed_PIOLab_BACI <- function(year,path)
{
  print("IEFeed_PIOLab_BACI initiated.")
  # Creat list that links the PIOT commodities to HS-codes and the BACI dataset
  # Warning: Do not mix 4- and 6-digit numbers under one product, can cause trouble.
  # Note that this list-object will be substituted by the sector aggregator matrix
  items <- list(list("name" = "IronOre","code" = c(2601)),
                list("name" = "SpongeIron","code" = c(7203)),
                list("name" = "PigIron","code" = c(7201)),
                list("name" = "Ingot", "code" = c(720610,720690,721810,722410)),
                list("name" = "Slab", "code" = c(720712,721890,722490)),
                list("name" = "Billet&Bloom", "code" = c(720711,720719,720720)),
                list("name" = "FlatRolledProducts","code" = c(7208:7212,7219,7220,7225,7226)),
                list("name" = "LongRolledProducts", "code" = c(7213:7217,7221:7223,7227:7229,7301,7302,7304:7308)),
                list("name" = "ScrapAfterTreatment", "code" = 7204))
  
  # Define number of regions
  n_reg <- 35
  
  # Import raw data
  data <- read.csv(paste0(path$Raw,"/BACI/baci92_",year,".csv"),stringsAsFactors=FALSE)
  
  colnames(data) <- c("year","hs6","from","to","value","quantity")
  
  # Read BACI's HS-6-digit commodity codes and country codes 
  code <- list("HS" = read.csv(paste0(path$Raw,"/BACI/product_code_baci92.csv"),stringsAsFactors=FALSE),
               "country" = read.csv(paste0(path$Raw,"/BACI/country_code_baci92.csv"),stringsAsFactors=FALSE))
  
  # splitting the 6-digit code into 2 columns for better filtering
  disag <- t(sapply(code$HS$CODE,function(x) substring(x,c(1,5),c(4,6))))
  colnames(disag) <- c("C1","C2")
  
  # Set to numeric and NA's to zero
  # Note that warnings are turned off and after the transformation on.
  options(warn = -1)
  storage.mode(disag) <- "integer"
  disag[is.na(disag)] <- 0
  
  # Add the two columns to the original code array
  code$HS <- cbind(disag,code$HS)
  storage.mode(code$HS$CODE) <- "integer"
  code$HS$CODE[is.na(code$HS$CODE)] <- 0
  # Warnings turned back on
  options(warn = 0)
  
  # Loop to filter, aggregate (as defined in items) and finally write csv file into target folder
  for(i in 1:length(items))
  {
    # First check whether the HS-code is 4 or 6 digit and read 6-digits out
    if(items[[i]]$code[1] > 9999)
    {
      product_codes <- code$HS %>% filter(CODE %in% items[[i]]$code) %>% select(CODE)
    }else
    {
      product_codes <- code$HS %>% filter(C1 %in% items[[i]]$code) %>% select(CODE)
    }
    
    data_select <- data %>% filter(hs6 %in% product_codes$CODE) %>% select(-year) 
    
    # Check missing values for quantities and fill gaps with global average prices
    quantity_missing <- data_select %>% mutate(price = value/quantity) %>% filter(is.na(price)) %>% select(-price)
    
    # Calculate global average prices for missing products 
    average_prices <- data_select %>% filter(hs6 %in% unique(quantity_missing$hs6),!is.na(quantity),!is.na(value)) %>% 
                      group_by(hs6) %>% summarise(value = sum(value),quantity = sum(quantity)) %>% 
                      mutate(price = value/quantity) %>% select(hs6,price)
    
    # Add price vector to missing quantities and calculate quantities
    quantity_missing <- left_join(quantity_missing,average_prices,by = "hs6",copy = FALSE)
    quantity_missing$quantity <- quantity_missing$value * quantity_missing$price
    quantity_missing <- select(quantity_missing,-price)
    
    # Remove rows/cases with NA's from dataset and add manipulated/new data
    data_select <- data_select %>% filter(!is.na(quantity)) %>% bind_rows(quantity_missing)
    
    # Aggregate across products
    trade_data_clean <- data_select %>% select(-hs6) %>% group_by(from,to) %>% 
                        summarise(value = sum(value),quantity = sum(quantity))
    
    # Till now official country codes from baci are used. Next, translate into the root-classification
    # To do this, first we need to import the country root classification
    root_class <- read.xlsx(paste0(path$Concordance,"/StandardPIOT_RootClassification.xlsx"),sheet = 1) %>% select(RootRegionCode,BACICode)
    
    # Look up root codes and exchange with baci codes
    trade_data_clean <- left_join(trade_data_clean,root_class,by = c("from" = "BACICode"),copy = FALSE)
    trade_data_clean <- left_join(trade_data_clean,root_class,by = c("to" = "BACICode"),copy = FALSE) 
    
    # remove South African Customs Union (BaciCode: 711) from trade data
    trade_data_clean <- filter(trade_data_clean,!from == 711) %>% filter(!to == 711)
    # and french southern territories
    trade_data_clean <- filter(trade_data_clean,!from == 260) %>% filter(!to == 260)
    
    # delete old baci codes
    trade_data_clean <- trade_data_clean %>% ungroup() %>% select(RootRegionCode.x,RootRegionCode.y,value,quantity)
    colnames(trade_data_clean)[1:2] <- c("From","To")
    
    #Test if NA's are produced and drop monetary value caus is not needed anymore
    #print(paste0("Check whether the summation gives a numeric value or an error for ",items[[i]]$name))
    #print(colSums(trade_data_clean))
    trade_data_clean <- select(trade_data_clean,-value)
    
    ###################################################################################
    # Starting from here, the raw data will be aggregated according to the base table #
    ###################################################################################
    
    
    # Load region aggregator
    source(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"))
    reg_agg <- Root2Base_RegionAggregator(paste0(path$Concordance,"/Region Aggregators/StandardPIOT_RegionAggregator.csv"))
    
    # Aggregate trade data from the root (245 reg) to the base classification (35 reg)
    trade_data_clean <- left_join(trade_data_clean,reg_agg,by = c("From" = "root"),copy = FALSE)
    trade_data_clean <- left_join(trade_data_clean,reg_agg,by = c("To" = "root"),copy = FALSE) 
    trade_data_clean <- select(trade_data_clean,base.x,base.y,quantity)
    colnames(trade_data_clean)[1:2] <- c("From","To")
    trade_data_clean <- trade_data_clean %>% group_by(From,To) %>% summarise(quantity = sum(quantity))
    
    # Remove trade flows between RoW countries to zero
    trade_data_clean <- trade_data_clean %>% filter(From != To)
    
    # Check if subfolder in processed data exists and if not create it
    path_set <- paste0(path$IE_Processed,"/BACI/")
    if(!dir.exists(path_set)) dir.create(path_set)
  
    # Write clean data to target folder
    write.csv(trade_data_clean,
              file = paste0(path_set,"BACI_",year,"_",items[[i]]$name,".csv"),
              row.names = FALSE)
    
    # Note that trade flows are measured in metric tons of material 
  }
  print("IEFeed_PIOLab_BACI finished.")
}



