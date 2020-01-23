

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
data <- left_join(data,root_class,by = c("ISOCode" = "ISO3digitCode"),copy = FALSE) %>% select(RootRegionCode,Quantity)

# Remove Yugoslavia (242) and USSR (233) from data
data <- data %>% filter(!RootRegionCode %in% c(233,242)) 
