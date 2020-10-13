################################################################################
# This script (i) loads the BACI raw data, (ii) performs some gap filling where physical 
# quantities are missing but prices are available and (iii) creates the source2root concordance  
#
# hanspeter.wieland@wu.ac.at (c)
# 26.02.2020

# Loading BACI classification
classi <- list("region" = read.csv(paste0(path$Raw,"/BACI/country_code_baci92.csv"),stringsAsFactors=FALSE),
              "product" = read.csv(paste0(path$Raw,"/BACI/product_code_baci92.csv"),stringsAsFactors=FALSE))

# Note that Taiwan is not explicity included in the BACI's region classification,
# but after thoroughly checking all countries and other (aggregated) regions
# it can be safely assumed for now that "other asia" (baci code 490) must refer to Taiwan

# Insert Taiwan region into baci list
classi$region$iso3[classi$region$i == 490] <- "TWN"

# Import raw data and change headers
data <- read.csv(paste0(path$Raw,"/BACI/baci92_",year,".csv"),stringsAsFactors=FALSE)
colnames(data) <- c("year","hs6","from","to","value","Quantity")

# remove South African Customs Union (BaciCode: 711) from trade data
data <- filter(data,!from == 711) %>% filter(!to == 711)
# and french southern territories
data <- filter(data,!from == 260) %>% filter(!to == 260)

# Filter products that belong to the PIOLab root
data <- filter(data,hs6 %in% root$flow$BACI[!is.na(root$flow$BACI)])  
head(data)
# Look up baci country codes
data <- left_join(data,classi$region[c("iso3","i")],
                  by = c("from" = "i"),copy = FALSE)
head(data)
data <- left_join(data,classi$region[c("iso3","i")],
                  by = c("to" = "i"),copy = FALSE) 
head(data)
# Look up root region code
data <- left_join(data,root$region[c("RootCountryAbbreviation","Code")],
                  by = c("iso3.x" = "RootCountryAbbreviation"),copy = FALSE)
head(data)
data <- left_join(data,root$region[c("RootCountryAbbreviation","Code")],
                  by = c("iso3.y" = "RootCountryAbbreviation"),copy = FALSE)
head(data)
# AD HOC SOLUTION FOR THE MOMENT becasue there are ca. 500 entries of NULL (regions),
# filter only the regions that are in the root. For 2008, this will delete ca. 0.01% of trade
nrow(data[is.na(data$Code.y),])
data <- data[!is.na(data$Code.x),]
data <- data[!is.na(data$Code.y),]

# Look up root product code
data <- left_join(data,root$flow[c("BACI","Code")],
                  by = c("hs6" = "BACI"),copy = FALSE)
head(data)
# Delete old baci codes
data <- select(data, Code.x, Code.y, Code, value, Quantity)
colnames(data)[1:3] <- c("From","To","Product")
head(data)
# Check missing values for quantities and fill gaps with global average prices
quantity_missing <- data %>% mutate(price = value/Quantity) %>% filter(is.na(price)) %>% select(-price)
head(quantity_missing)
head(data)
# Calculate global average prices for missing products 
average_prices <- data %>% filter(Product %in% unique(quantity_missing$Product),!is.na(Quantity),!is.na(value)) %>% 
  group_by(Product) %>% summarise(value = sum(value),Quantity = sum(Quantity)) %>% 
  mutate(price = value/Quantity) %>% select(Product,price)
head(average_prices)
# Add price vector to missing quantities and calculate quantities
quantity_missing <- left_join(quantity_missing,average_prices,by = "Product",copy = FALSE)
quantity_missing$Quantity <- quantity_missing$value * quantity_missing$price
quantity_missing <- select(quantity_missing,-price)

# Remove rows/cases with NA's from dataset and add manipulated/new data
data <- data %>% filter(!is.na(Quantity)) %>% bind_rows(quantity_missing)

remove(quantity_missing,average_prices,classi)
