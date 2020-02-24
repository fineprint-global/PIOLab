# Import raw data from Pauiuk and filter for years selected
data <- read.xlsx(paste0(path$Raw,"/EOL/1_F_steel_200R_F_13_14_inflow_waste_mgt.xlsx"),
                  sheet = 2)[,c("aspect.6.:.destination_region","aspect.7.:.time","value")]
colnames(data) <- c("EOL","Year","Quantity")
data <- filter(data,Year == year,Quantity > 0) %>% select(EOL,Quantity)

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
