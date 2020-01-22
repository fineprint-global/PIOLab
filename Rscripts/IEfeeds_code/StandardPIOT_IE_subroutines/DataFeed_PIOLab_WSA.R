#########################################
#                                       #
# This data feed processes the raw data #
#     of the Worldsteel Yearbooks       #
#                                       #
#########################################


DataFeed_PIOLab_WSA <- function(year,path)
{
  print("DataFeed_PIOLab_WSA initiated.")
  # Depending on the year that is processed, the data feed either sources numbers 
  # from the 2018 or the 2005 yearbook
  if(year >= 2008)
  {
    # Loading raw RData-File containing the full yearbook
    # Note that at the moment the data feed does not encompass the transformation
    # of the original Yearbook pdf into an RData-File.
    
    load(file = paste0(path$Raw,"/WSA/Worldsteeldata 2018.RData"))
    
    # Defining the page(s) where the production values should be imported from.
    items <- list(list("page"=96,"name"="SpongeIron"),
                  list("page"=c(90,91),"name"="PigIron"),
                  list("page"=c(7,8),"name"="Ingots"),
                  list("page"=20,"name"="SteelOxygenBlownConverters"),
                  list("page"=c(22,23),"name"="SteelElectricFurnaces"),
                  list("page"=26,"name"="SteelOpenHearthFurnaces"),
                  list("page"=c(32,33),"name"="LongRolledProducts"),
                  list("page"=34,"name"="FlatRolledProducts"))
    
    # The years contained in the yearbook
    years <- 2008:2017
    
  }else
  {
    load(file = paste0(path$wsa,"Worldsteeldata 2005.RData"))
    
    items <- list(list("page"=3,"name"="SpongeIron"),
                  list("page"=c(1,2),"name"="PigIron"),
                  list("page"=c(13,14),"name"="Ingots"),
                  list("page"=c(29,30),"name"="SteelOxygenBlownConverters"),
                  list("page"=c(33,34),"name"="SteelElectricFurnaces"),
                  list("page"=37,"name"="SteelOpenHearthFurnaces"),
                  list("page"=c(45,46),"name"="LongRolledProducts"),
                  list("page"=c(47,48),"name"="FlatRolledProducts"))
    
    years <- 1995:2004
  }
  
  # Change name of the object (to be changeg asap)
  yb <- out
  remove(out)
  
  # load concordance to match Worldsteel country names with 3digitISO codes
  concord <- read.xlsx(xlsxFile = paste0(path$Concordance,"/WSA_RegionConcordance.xlsx"),sheet = 1)
  
  # Load root classification
  root_class <- read.xlsx(paste0(path$Concordance,"/StandardPIOT_RootClassification.xlsx"),sheet = 1)
  
  # The following loop processes each item/commodity separately
  
  for(i in 1:length(items))
  {
    item_page <- items[[i]]$page
    item_name <- items[[i]]$name
    
    # For the case where the data is on one page
    if(length(item_page) == 1)
    {
      # Extract production data reported on a single page:
      data <- yb[[item_page]][-1,]
    }
    
    # For the case where the data is on two page
    if(length(item_page) == 2)
    {
      # Extract production data reported on two pages:
      yb_1 <- yb[[item_page[1]]][-1,]
      yb_2 <- yb[[item_page[2]]][-1,]
      
      # create empty matrix
      data <- matrix(data = NA,
                     nrow = nrow(yb_1)+nrow(yb_2),
                     ncol = ncol(yb_1))
      
      data[1:nrow(yb_1),] <- yb_1
      data[(nrow(yb_1)+1):nrow(data),] <- yb_2
      remove(yb_1,yb_2)
    }
    
    # separate region names from the raw data
    reg_names <- data[,1]                
    data <- data[,-1]
    
    colnames(data) <- years
    
    ###############################################
    # In the following, the raw data is coerced into numeric values and cleaned
    # Remove empty spaces and delete e's
    data <- apply(data,2,function(x)gsub('\\s+', '',x))
    data <- apply(data,2,function(x)gsub('e', '',x))
    
    # Transform entries from character to integer
    # Warning messages are turned off for the following
    options(warn = -1)
    storage.mode(data) <- "integer"
    options(warn = 0)
    
    # Set NA to zero (to be discussed how to treat such entries in the future)
    data[is.na(data)] <- 0
    
    # Identify entries with the same name 
    # (can be the case for 'Other Europe' that might refer to a region or only a subset of countries, 
    # see Export of Iron Ore)

    if(TRUE %in% duplicated(reg_names))
    {
      dupl <- reg_names[duplicated(reg_names)]
      loc <- which(reg_names %in% dupl)
      loc.min <- which.min(rowSums(data[loc,]))
      reg_names[loc[loc.min]] <- paste0(reg_names[loc[loc.min]],"-countries")
    }
    
    # Translate Worldsteel country codes into 3digitISO codes 
    concord_country <- concord %>% filter(original %in% reg_names) 
    concord_country$original <- factor(concord_country$original,levels = reg_names)
    concord_country <- concord_country[order(concord_country$original),]
    concord_country$index <- 1:nrow(concord_country)
    
    # Filter specific countries from the full concordance table
    concord_country <- concord_country %>% filter(!is.na(ISO3digitCode)) %>% select(index,countries,ISO3digitCode)
    # Filter country data and a specific year from the WSA data 
    data_clean <- cbind(concord_country,data[concord_country$index,as.character(year)]) %>% select(-countries,-index)
    colnames(data_clean)[2] <- "Quantity"
    # Look up root classification code and filter specific year
    data_clean <- left_join(data_clean,root_class,by = c("ISO3digitCode"),copy = FALSE) %>% select(RootRegionCode,Quantity)
    # Translate quantities into metric tons
    data_clean$Quantity <- data_clean$Quantity * 1000
    
    # Load region aggregator and look up base table codes
    source(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"))
    reg_agg <- Root2Base_RegionAggregator(paste0(path$Concordance,"//Region Aggregators/StandardPIOT_RegionAggregator.csv"))
    
    data_clean <- left_join(data_clean,reg_agg,by = c("RootRegionCode" = "root"),copy = FALSE) %>% select(base,Quantity)
    data_clean <- data_clean %>% group_by(base) %>% summarise(Quantity = sum(Quantity))
    
    # Check if subfolder in processed data exists and if not create it
    path_set <- paste0(path$Processed,"/WSA")
    if(!dir.exists(path_set)) dir.create(path_set)
    
    # Export data to folder (note that the unit is metric tons)
    write.csv(data_clean,
              file = paste0(path_set,"/WSA_",year,"_",item_name,".csv"),
              row.names = FALSE)
  }
  print("DataFeed_PIOLab_WSA finished.")
}
