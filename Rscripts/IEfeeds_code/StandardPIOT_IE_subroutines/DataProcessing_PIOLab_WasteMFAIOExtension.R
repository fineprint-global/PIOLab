###############################################
#                                             #
#   This code uses the data on fabrication    #
#   yields and products flows to end-use to   # 
#     build the IO extension and estimate     # 
#     the amount of fabrication scrap         #
#                                             #
###############################################


DataProcessing_PIOLab_WasteMFAIOExtension <- function(year,path)
{
  print("DataProcessing_PIOLab_WasteMFAIOExtension initiated.")
  #############################################################################
  
  # Load IO codes of aggregated EXIOBASE data
  load(paste0(path$Processed,"/EXIOWasteMFAIO/IO.codes.RData"))
  
  # Load function to create allocation map
  source(paste0(path$Subroutines,"/makeEndUseMap.R"))
  # 1. Defining functions
  
  # aggregation function
  Agg <- function(x)
  {
    x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
    return(x)
  }
  
  # Writing values of rolled products into arrays (extension and fabrication scrap)
  WriteValues2Array <- function(data,name)
  {
    for(i in 1:nrow(data))
    {
      #print(i)
      reg <- data$base[i]
      
      # call function to create specific map 
      map <- makeEndUseMap(reg,name)
      
      map <- map %>% mutate(Value = Share * data$Quantity[data$base == reg]) %>% 
        select(index,IO,Yield.Sector,Value) 
      
      map <- left_join(map,Yields,c("Yield.Sector"),copy = FALSE) %>% mutate(EndUse = Value*Yield.Factor,Scrap = Value - EndUse) %>%
        select(index,IO,EndUse,Scrap)
      
      index <- map$index+((reg-1)*10)
      
      if(name == "Flat")
      {
        Q[1,index] <<- map$EndUse
        Scrap[1,index] <<- map$Scrap
      }else
      {
        Q[2,index] <<- map$EndUse
        Scrap[2,index] <<- map$Scrap
      }
    }
    
  }
  
  # Load processed data from Cullen et al 2012
  Yields <- read.csv(paste0(path$Processed,"/Cullen/FabricationYields.csv"))

    ##############################################################################
  # 1. The first section calculates the net use of rolled steel products in regions
  # by adding imports and subtracting exports from the WSA production values
  
  # Load finished steel production
  Flat <- read.csv(paste0(path$Processed,"/WSA/WSA_",year,"_FlatRolledProducts.csv"))
  Long <- read.csv(paste0(path$Processed,"/WSA/WSA_",year,"_LongRolledProducts.csv"))
  # Load BACI trade flows 
  Flat_trade <- read.csv(paste0(path$Processed,"/BACI/BACI_",year,"_FlatRolledProducts.csv"))
  Long_trade <- read.csv(paste0(path$Processed,"/BACI/BACI_",year,"_LongRolledProducts.csv"))
  
  # Estimate the net-use of products in countries
  Flat_import <- Flat_trade %>% group_by(To) %>% summarise(Quantity = sum(quantity))
  Flat_export <- Flat_trade %>% group_by(From) %>% summarise(Quantity = sum(quantity))
  Flat_trade <- full_join(Flat_import,Flat_export,c("To" = "From"),copy = FALSE)
  colnames(Flat_trade) <- c("base","import","export")
  # Regional production + imports - exports = net-use
  Flat <- full_join(Flat,Flat_trade,c("base"),copy = FALSE) 
  Flat[is.na(Flat)] <- 0
  Flat <- Flat %>% mutate(NetUse = Quantity + import - export) %>%
    select(base,NetUse)
  colnames(Flat)[2] <- "Quantity"
  
  Long_import <- Long_trade %>% group_by(To) %>% summarise(Quantity = sum(quantity))
  Long_export <- Long_trade %>% group_by(From) %>% summarise(Quantity = sum(quantity))
  Long_trade <- full_join(Long_import,Long_export,c("To" = "From"),copy = FALSE)
  colnames(Long_trade) <- c("base","import","export")
  # Regional production + imports - exports = net-use
  Long <- full_join(Long,Long_trade,c("base"),copy = FALSE) 
  Long[is.na(Long)] <- 0
  Long <- Long %>% mutate(NetUse = Quantity + import - export) %>%
    select(base,NetUse)
  colnames(Long)[2] <- "Quantity"
  
  ##############################################################################
  # 3. Start processing
  
  # Create two empty arrays for steel for the extension (Q) and for the fabrication scrap
  Scrap <- Q <- as.data.frame(matrix(0,2,nrow(IO.codes)))
  rownames(Scrap) <- rownames(Q) <- c("Flat","Long")
  
  # Execute functions
  # Warnings are turned of for the run.
  options(warn = -1)
  WriteValues2Array(Flat,"Flat")
  WriteValues2Array(Long,"Long")
  options(warn = 0)

  # Save extension of the MFA-Waste-IO Model
  save(Q,file = paste0(path$Processed,"/EXIOWasteMFAIO/",year,"_Q.RData"))
  
  # Prepate fabrication scrap data for export
  Scrap <- colSums(Scrap)
  Scrap <- data.frame(IO.codes,
                     "Quantity" = Scrap,
                     stringsAsFactors = FALSE)
  
  # Check if subfolder in processed data exists and if not create it
  path_set <- paste0(path$Processed,"/FabricationScrap")
  if(!dir.exists(path_set)) dir.create(path_set)
  
  # Write to folder
  write.csv(Scrap,file = paste0(path_set,"/FabricationScrap_",year,".csv"),row.names = FALSE)
  
  print("DataProcessing_PIOLab_WasteMFAIOExtension finished.")
  
}