data <- read.csv(paste0(path$Raw,"/IRP/DE_CCC_ResearchDB.csv"),stringsAsFactors=FALSE)
colnames(data)[7:51] <- 1970:2014
data <- data %>% filter(CCC_Name == "Iron ores") %>% select(Country,AlphaNumISO,as.character(year)) 

data <- data %>% separate(AlphaNumISO, into = c('ISOCode', 'num'), sep = 3) %>% select(-num)  
colnames(data)[ncol(data)] <- "Quantity"
data <- filter(data,Quantity > 0)

# Remove Yugoslavia, USSR, Belgium-Luxembourg and Serbia (Montenegro) from data because (sub-) regions are already included
data <- data %>% filter(!ISOCode %in% c("YUG","SUN","BLG","SCG")) 

# Look up root codes
data <- left_join(data,root$region[c("Code","RootCountryAbbreviation")] ,
                  by = c("ISOCode" = "RootCountryAbbreviation"),copy = FALSE) %>% select(Code,Quantity)

