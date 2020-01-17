#################################################
#                                               #
# This data feed imports the fabrication yields #
#   and the end use share as calculated in      #
#     Cullen et al 2012 and available on        # 
#     on the IE data commons of Pauliuk         #
#                                               #
#################################################

DataFeed_Cullen <- function(path)
{
  print("DataFeed_Cullen initiated.")
  # 1. Importing data on fabrication yields
  raw_data <- read.xlsx(paste0(path$Raw,"/Cullen/4_PY_FabricationYield_Cullen2012.xlsx"),
                        sheet = 2)
  
  # Select columns from data
  data <- raw_data[,c("aspect.3.:.commodity","value")]
  colnames(data) <- c("Yield.Sector","Yield.Factor")
  data$Yield.Factor <- data$Yield.Factor/100  
  data <- as.data.frame(data)
  
  # Check if subfolder in processed data exists and if not create it
  path_set <- paste0(path$Processed,"/Cullen")
  if(!dir.exists(path_set)) dir.create(path_set)

  # Write to file
  write.csv(data,file = paste0(path_set,"/FabricationYields.csv"),row.names = FALSE)  
  
  # 2. importing sankey raw data and select end-use flows in order to calculate shares
  
  raw_data <- read.xlsx(paste0(path$Raw,"/Cullen/1_F_steel_SankeyFlows_2008_Global.xlsx"),
                        sheet = 2)
  
  # Select columns
  data <- raw_data[,c("aspect.2.:.destination_process","aspect.3.:.commodity","value")]
  colnames(data) <- c("EndUse","Commodity","Quantity")
  data <- data[,c("Commodity","EndUse","Quantity")]
  
  # The end-use categories used in Cullen et al 2012
  end_use  <- c("Buildings","Infrastructure","Cars","Trucks","Ships + other","Mechanical equipment",
                "Electrical equipment","Food packaging","Domestic appliances")
  
  # Filter end-use categories
  data <- filter(data,EndUse %in% end_use)
  
  # Set up concordance to link the commodity to groups 
  groups <- data.frame("index" = 1:length(unique(data$Commodity)),
                     "Commodity.Group" = c(rep("Long",6),rep("Flat",10),"Long",rep("Castings",2)),
                     "item" = unique(data$Commodity),
                     stringsAsFactors = FALSE)
    
  data <- left_join(data,groups,c("Commodity" = "item"),copy = FALSE)
  data <- data[,c("index","Commodity","Commodity.Group","EndUse","Quantity")]
  data$index <- 1:nrow(data)
  
  # Write to file
  write.csv(data,file = paste0(path_set,"/ProductsToEndUse.csv"),row.names = FALSE) 
}