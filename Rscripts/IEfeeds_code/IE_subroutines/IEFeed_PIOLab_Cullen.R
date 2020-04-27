#################################################
#                                               #
# This data feed imports the fabrication yields #
#   and the end use share as calculated in      #
#     Cullen et al 2012 and available on        # 
#     on the IE data commons of Pauliuk         #
#                                               #
#################################################

IEFeed_PIOLab_Cullen <- function(path)
{
  print("IEFeed_PIOLab_Cullen initiated.")
  
  # 1. Importing data on fabrication yields
  
  raw_data <- read.xlsx(paste0(path$Raw,"/Cullen/4_PY_FabricationYield_Cullen2012.xlsx"),
                        sheet = 2)
  
  # Select columns from data
  data <- raw_data[,c("aspect.3.:.commodity","value")]
  colnames(data) <- c("Yield.Sector","Yield.Factor")
  data$Yield.Factor <- data$Yield.Factor/100  
  yields <- as.data.frame(data)
  
  # Check if subfolder in processed data exists and if not create it
  
  path_set <- paste0(path$IE_Processed,"/Cullen")
  
  if(!dir.exists(path_set)) dir.create(path_set)


  # Write to file
  
  write.csv(yields,file = paste0(path_set,"/FabricationYields.csv"),row.names = FALSE)  
  
  
  # 2. importing sankey raw data and select end-use flows in order to calculate shares
  
  raw_data <- read.xlsx(paste0(path$Raw,"/Cullen/1_F_steel_SankeyFlows_2008_Global.xlsx"),
                        sheet = 2)
  
  # Select columns
  
  data <- raw_data[,c("aspect.2.:.destination_process","aspect.3.:.commodity","value")]
  
  colnames(data) <- c("EndUse","Commodity","Quantity")
  
  data <- data[,c("Commodity","EndUse","Quantity")]
  
  
  # The end-use categories used in Cullen et al 2012
  
  end_use  <- c("Buildings","Infrastructure","Cars","Trucks","Ships + other","Mechanical equipment",
                "Electrical equipment","Food packaging","Domestic appliances","Metal goods","Food packaging")
  
  # Filter end-use categories
  
  data <- filter(data,EndUse %in% end_use)
  
  # Set up concordance to link the products to end-use categories 
  
  Conco <- reshape(data = data,
                   idvar = "Commodity",
                   v.names = "Quantity",
                   timevar = "EndUse",
                   direction = "wide")
  
  remove(data,raw_data)  # Delete objects that are not needed anymore
  
  rownames(Conco) <- Conco$Commodity  # Move product names to rownames 
  
  Conco$Commodity <- NULL # Delete product name column
  
  colnames(Conco) <- gsub("Quantity.","",colnames(Conco)) # Cleaning colnames
  
  colnames(Conco)[colnames(Conco) == "Ships + other"] <- "Ships/other"
  
  Conco[is.na(Conco)] <- 0
  
  # Write to file
  
  write.csv(Conco,file = paste0(path_set,"/ProductsToEndUse.csv"))
}