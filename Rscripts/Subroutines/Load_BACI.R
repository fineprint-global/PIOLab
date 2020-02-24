###############################################################################
#
# This script loads the BACI rwa data and performs some pre-processing 
#
#
# load BACI concordances
conco <- list("region" =  read.xlsx(paste0(path$Concordance,"/BACI/BACI_concordance.xlsx"),sheet = 1),
              "product" = read.xlsx(paste0(path$Concordance,"/BACI/BACI_concordance.xlsx"),sheet = 2))

# Import raw data and change headers
data <- read.csv(paste0(path$Raw,"/BACI/baci92_",year,".csv"),stringsAsFactors=FALSE)
colnames(data) <- c("year","hs6","from","to","value","Quantity")

# Look up root country codes and exchange with baci codes
data <- left_join(data,conco$region,by = c("from" = "baci"),copy = FALSE)
data <- left_join(data,conco$region,by = c("to" = "baci"),copy = FALSE) 

# remove South African Customs Union (BaciCode: 711) from trade data
data <- filter(data,!from == 711) %>% filter(!to == 711)
# and french southern territories
data <- filter(data,!from == 260) %>% filter(!to == 260)

# Filter products that belong to the PIOLab root and look up root product classification code
data <- filter(data,hs6 %in% conco$product$baci)  
data <- left_join(data,conco$product,by = c("hs6" = "baci"),copy = FALSE) %>% 
  select(-hs6)

# delete old baci codes
data <- data %>% select(root.x,root.y,root,value,Quantity)
colnames(data)[1:3] <- c("From","To","Product")

# Check missing values for quantities and fill gaps with global average prices
quantity_missing <- data %>% mutate(price = value/Quantity) %>% filter(is.na(price)) %>% select(-price)

# Calculate global average prices for missing products 
average_prices <- data %>% filter(Product %in% unique(quantity_missing$Product),!is.na(Quantity),!is.na(value)) %>% 
  group_by(Product) %>% summarise(value = sum(value),Quantity = sum(Quantity)) %>% 
  mutate(price = value/Quantity) %>% select(Product,price)

# Add price vector to missing quantities and calculate quantities
quantity_missing <- left_join(quantity_missing,average_prices,by = "Product",copy = FALSE)
quantity_missing$Quantity <- quantity_missing$value * quantity_missing$price
quantity_missing <- select(quantity_missing,-price)

# Remove rows/cases with NA's from dataset and add manipulated/new data
data <- data %>% filter(!is.na(Quantity)) %>% bind_rows(quantity_missing)

remove(quantity_missing,average_prices,conco)
