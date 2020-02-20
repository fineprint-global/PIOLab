################################################################################
################################################################################
#
datafeed_name <- "IRPimports"
print(paste0("datafeed_PIOLab_",datafeed_name," initiated."))

################################################################################
# Set library path when running on suphys server
if(Sys.info()[1] == "Linux"){
  .libPaths("/suphys/hwie3321/R/x86_64-redhat-linux-gnu-library/3.5")
  # Define location for root directory
  root_folder <- "/import/emily1/isa/IELab/Roots/PIOLab/"}else{
  root_folder <- "C:/Users/hwieland/Github workspace/PIOLab/"}
################################################################################
# Initializing R script (load R packages and set paths to folders etc.)
source(paste0(root_folder,"Rscripts/Subroutines/InitializationR.R"))

# Loading raw data
data <- read.csv(paste0(path$Raw,"/IRP/all_CCC_Imp_ResearchDB.csv"),stringsAsFactors=FALSE)
colnames(data)[6:49] <- 1970:2013
data <- data %>% filter(CCC_IEATrade_Name == "Iron ores") %>% select(Country,AlphaNumISO,as.character(year)) 
# Remove NA
data <- data[!is.na(data$`2008`),]

data <- data %>% separate(AlphaNumISO, into = c('ISOCode', 'num'), sep = 3) %>% select(-num)  
colnames(data)[ncol(data)] <- "Quantity"
data <- filter(data,Quantity > 0)
# Look up root codes
data <- left_join(data,root$region[,c("Code","ISO3digitCode")],by = c("ISOCode" = "ISO3digitCode"),copy = FALSE) %>% select(Code,Quantity)

# Remove Yugoslavia (242) and USSR (233) from data
data <- data %>% filter(!Code %in% c(233,242)) 

# Add standard errors 
source(paste0(path$Subroutines,"/SE_LogRegression.R"))
data <- SE_LogRegression(data,0.3,0.05)

reg_max <- nrow(root$region)

# Create empty ALANG table with header
source(paste0(path$Subroutines,"/makeALANGheadline.R"))
# Extend table with additional columns

for(i in 1:nrow(data))
{ 
  # Get root_code of regions 
  reg <- data$Code[i]
  
  if(reg == 1) reg_range <- paste0("2-",as.character(reg_max))
  
  if(reg == 2) reg_range <- paste0("[1,3-",as.character(reg_max),"]")
  
  if(reg == reg_max) reg_range <- paste0("1-",as.character(reg_max-1))
  
  if(reg == (reg_max-1)) reg_range <- paste0("[1-",as.character(reg_max-2),",",as.character(reg_max),"]")
  
  if(reg > 2 & reg < (reg_max-1)) {
    reg_range <- paste0("[1-",as.character(reg-1),",",as.character(reg+1),"-",
                        as.character(reg_max),"]") }
  
  # Read import value
  value <- as.character(data$Quantity[i])
  # Set SE to 5%
  SE <- as.character(data$SE[i])
  reg_name <- root$region$Name[reg]
  reg <- as.character(reg)
  
  # Add command
  ALANG <- add_row(ALANG,'1' = paste0("DataFeed IRP imports to ",reg_name),
                   Value = value,S.E. = SE,
                   'Row parent' = reg_range,'Row child' = "2",'Row grandchild' = "1-e",
                   'Column parent' = reg,'Column child' = "1",'Column grandchild' = "1-e")
}
# Add other variables
ALANG$`#` <- as.character(1:nrow(ALANG))
ALANG$Incl <- "Y"
ALANG$Parts <- "1"
ALANG$`Pre-map` <- ""
ALANG$`Post-map` <- ""
ALANG$`Pre-Map` <- ""
ALANG$`Post-Map` <- ""
ALANG$Years <- "1"
ALANG$Margin <- "1"
ALANG$Coef1 <- "1"

# Call script that writes the ALANG file to the repsective folder in the root
source(paste0(path$root,"Rscripts/datafeeds_code/datafeed_subroutines/WriteALANG2Folder.R"))

print(paste0("datafeed_PIOLab_",datafeed_name," finished."))

