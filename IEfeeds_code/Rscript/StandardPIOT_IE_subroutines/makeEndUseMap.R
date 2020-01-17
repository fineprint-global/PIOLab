# Function to calculate shares to allocate flat and long rolled products to IO sectors

makeEndUseMap <- function(reg,name)
{
  EndUse <- read.csv(paste0(path$Processed,"/Cullen/ProductsToEndUse.csv"))
  # For StandardPIOT Castings are not considered
  EndUse <- EndUse[EndUse$Commodity.Group != "Castings",]
  
  # Load IO codes of aggregated EXIOBASE data
  load(paste0(path$Processed,"/EXIOWasteMFAIO/IO.codes.RData"))    
  x <- read.table(paste0(path$Processed,"/EXIOWasteMFAIO/",year,"_x.csv"),col.names = FALSE)
  x <- x[,1]
  
  ##############################################################################
  # 2. Create concordances between the product classifications (Cullen, EXIOBASE)
  
  Conco <- data.frame("index" = 1:length(unique(IO.codes$commodity)),
                      "IO" = unique(IO.codes$commodity),
                      "EndUse.Group" = c("Mechanical equipment","Mechanical equipment","Electrical equipment","Electrical equipment","Domestic appliances",
                                         "Domestic appliances","Cars","Ships + other","Domestic appliances","Infrastructure"),
                      "Yield.Sector" = c("Metal goods","Mechanical equipment","Electrical equipment","Electrical equipment","Domestic appliances",
                                         "Domestic appliances","Cars","Ships/other","Metal goods","Infrastructure"),
                      stringsAsFactors = FALSE)
  
  # Add aggregated classification to EndUse sectors
  EndUseGroup <- data.frame("EndUseItem" = unique(EndUse$EndUse),
                            "EndUse.Group" = c("Infrastructure","Infrastructure","Mechanical equipment","Cars","Cars","Ships + other","Electrical equipment",
                                               "Domestic appliances","Domestic appliances"),
                            stringsAsFactors = FALSE)
  
  EndUse <- left_join(EndUse,EndUseGroup,c("EndUse" = "EndUseItem"),copy = FALSE) %>%
    select(Commodity.Group,EndUse.Group,Quantity) %>% group_by(Commodity.Group,EndUse.Group) %>% 
    summarise(Quantity = sum(Quantity)) %>% group_by(Commodity.Group) %>% 
    mutate(Total = sum(Quantity),EndUse.Share = Quantity/Total) %>% select(Commodity.Group,EndUse.Group,EndUse.Share)
  
  EndUse <- left_join(EndUse,Conco,c("EndUse.Group"),copy = FALSE)
  
  map <- filter(EndUse,Commodity.Group == name)
  
  x.select <- data.frame("IO" = IO.codes$commodity[1:10],
                         "Value" = x[IO.codes$index[IO.codes$base == reg]],
                         stringsAsFactors = FALSE)
  
  map <- left_join(map,x.select,c("IO"),copy = FALSE) %>% group_by(EndUse.Group) %>% 
    mutate(Total = sum(Value),IO.Share = Value/Total,Share = EndUse.Share*IO.Share) %>%
    ungroup(map) 
  
  return(map)
}

