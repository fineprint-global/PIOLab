################################################################################
# This function reads and aligns the WSA steel production raw data with the root classification

Read_ProductionWSA <- function(path,year,item_page,yb,concord)
{
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
  concord_country <- concord_country %>% filter(!is.na(ISO3digitCode)) %>% 
    select(index,countries,ISO3digitCode)
  # Filter country data and a specific year from the WSA data 
  data_clean <- cbind(concord_country,data[concord_country$index,as.character(year)]) %>% 
    select(-countries,-index)
  colnames(data_clean)[2] <- "Quantity"
  # Look up root classification code and filter specific year
  data_clean <- left_join(data_clean,root$region,by = c("ISO3digitCode"),copy = FALSE) %>% select(Code,Quantity)
  # Translate quantities into metric tons
  data_clean$Quantity <- data_clean$Quantity * 1000
  
  # The Statistical Yearbook notes that zero indicates that the quantity concerned 
  # is less than 500 tonnes, thus add 500 tons to zeros
  data_clean$Quantity[data_clean$Quantity == 0] <- 500
  
  return(data_clean)
}
