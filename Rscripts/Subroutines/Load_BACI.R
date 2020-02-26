################################################################################
# This script (i) loads the BACI raw data, (ii) performs some gap filling where physical 
# quantities are missing but prices are available and (iii) creates the source2root concordance  
#
# hanspeter.wieland@wu.ac.at (c)
# 26.02.2020

# Loading BACI classification
classi <- list("region" = read.csv(paste0(path$Raw,"/BACI/country_code_baci92.csv"),stringsAsFactors=FALSE),
              "product" = read.csv(paste0(path$Raw,"/BACI/product_code_baci92.csv"),stringsAsFactors=FALSE))

length(unique(classi$region$iso3))

# Import raw data and change headers
data <- read.csv(paste0(path$Raw,"/BACI/baci92_",year,".csv"),stringsAsFactors=FALSE)
colnames(data) <- c("year","hs6","from","to","value","Quantity")

# remove South African Customs Union (BaciCode: 711) from trade data
data <- filter(data,!from == 711) %>% filter(!to == 711)
# and french southern territories
data <- filter(data,!from == 260) %>% filter(!to == 260)

# Filter products that belong to the PIOLab root
data <- filter(data,hs6 %in% root$product$BACI[!is.na(root$product$BACI)])  

# Look up baci country codes
data <- left_join(data,classi$region[c("iso3","i")],by = c("from" = "i"),copy = FALSE)
data <- left_join(data,classi$region[c("iso3","i")],by = c("to" = "i"),copy = FALSE) 

# AD HOC SOLUTION FOR THE MOMENT becasue there are a lot of NULL (regions),
# filter only the regions that are in the root. For 2008, this will delete ca. 3% of trade

# Look up root region code
data <- left_join(data,root$region[c("RootCountryAbbreviation","Code")],
                  by = c("iso3.x" = "RootCountryAbbreviation"),copy = FALSE)

data <- left_join(data,root$region[c("RootCountryAbbreviation","Code")],
                  by = c("iso3.y" = "RootCountryAbbreviation"),copy = FALSE)

# Look up root product code
data <- left_join(data,root$product[c("BACI","Code")],
                  by = c("hs6" = "BACI"),copy = FALSE)

# delete old baci codes
data <- select(data, Code.x, Code.y, Code, value, Quantity)
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

remove(quantity_missing,average_prices,classi)
