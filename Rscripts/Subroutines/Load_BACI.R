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
colnames(data) <- c("year","hs6","from","to","value","quantity")

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
data <- data %>% select(root.x,root.y,root,value,quantity)
colnames(data)[1:3] <- c("From","To","Product")

# Check missing values for quantities and fill gaps with global average prices
quantity_missing <- data %>% mutate(price = value/quantity) %>% filter(is.na(price)) %>% select(-price)

# Calculate global average prices for missing products 
average_prices <- data %>% filter(Product %in% unique(quantity_missing$Product),!is.na(quantity),!is.na(value)) %>% 
  group_by(Product) %>% summarise(value = sum(value),quantity = sum(quantity)) %>% 
  mutate(price = value/quantity) %>% select(Product,price)

# Add price vector to missing quantities and calculate quantities
quantity_missing <- left_join(quantity_missing,average_prices,by = "Product",copy = FALSE)
quantity_missing$quantity <- quantity_missing$value * quantity_missing$price
quantity_missing <- select(quantity_missing,-price)

# Remove rows/cases with NA's from dataset and add manipulated/new data
data <- data %>% filter(!is.na(quantity)) %>% bind_rows(quantity_missing)

remove(quantity_missing,average_prices)




# # Read BACI's HS-6-digit commodity codes and country codes 
# code <- list("HS" = read.csv(paste0(path$Raw,"/BACI/product_code_baci92.csv"),stringsAsFactors=FALSE),
#              "country" = read.csv(paste0(path$Raw,"/BACI/country_code_baci92.csv"),stringsAsFactors=FALSE))
# 
# # splitting the 6-digit code into 2 columns for better filtering
# disag <- t(sapply(code$HS$CODE,function(x) substring(x,c(1,5),c(4,6))))
# colnames(disag) <- c("C1","C2")
# 
# # Set to numeric and NA's to zero
# # Note that warnings are turned off and after the transformation on.
# options(warn = -1)
# storage.mode(disag) <- "integer"
# disag[is.na(disag)] <- 0
# 
# # Add the two columns to the original code array
# code$HS <- cbind(disag,code$HS)
# storage.mode(code$HS$CODE) <- "integer"
# code$HS$CODE[is.na(code$HS$CODE)] <- 0
# # Warnings turned back on
# options(warn = 0)


