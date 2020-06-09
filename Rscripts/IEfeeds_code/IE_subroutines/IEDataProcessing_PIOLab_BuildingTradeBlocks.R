###################################
#                                 #  
#   This code writes data into    #
#   the trade blocks for AISHA    #
#                                 #  
###################################

IEDataProcessing_PIOLab_BuildingTradeBlocks <- function(year,path)
{
  print("IEDataProcessing_PIOLab_BuildingTradeBlocks initiated.")
  
  path[["IE_classification"]] <- paste0(path$Settings,"/Base/",IEdatafeed_name,"_BaseSectorClassification.xlsx")
  
  # Load WSA settings (for codes and feed names):
  Settings <- read.xlsx(paste0(path$Settings,"/datafeeds_settings/WSA_settings.xlsx"))
  
  # Load prorating function and Number2File:
  
  source(paste0(path$Subroutines,"/Prorate.R"))
  source(paste0(path$Subroutines,"/Numbers2File.R"))
  
  
  # Setting <- list( "WSA" = read.xlsx(xlsxFile = paste0(path$Settings,"/datafeeds_settings/WSA_settings.xlsx"), sheet = 1 ),
  #                  "IE" = read.xlsx(xlsxFile = paste0(path$Settings,"/datafeeds_settings/IE_settings.xlsx"), sheet = 1 )
  #                 )
  
  # Load flow data where all regions are included in one object:
  
  data <- list( "FinalDemand" =  read.csv( paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_","FinalDemand.csv") ),
                "Fab2Demand" = read.csv( paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_","Fabrication2FinalDemand.csv") ),
                "BACI" =  read.csv(paste0(path$IE_Processed,"/BACI/BACI_",year,".csv") ) 
              )

  # Select codes for finished steel:
  
  Code <- list("base" = list("Finished" = filter(base$flow,Type == 'Finished') %>% pull(Code),
                             "Final" = filter(base$flow,Type == 'Final') %>% pull(Code),
                             "Forgings" = base$flow$Code[base$flow$Name == 'Forgings'],
                             "CoilSheetStrip" = base$flow$Code[base$flow$Name == "Hot rolled coil-sheet-strip"]),
               "industry" = list("manu" = filter(base$process,Type == 'Final') %>% pull(Code),
                                 "others" = filter(base$process,Type != 'Final') %>% pull(Code)),
               "product" = list("manu" = filter(base$flow,Type == 'Final') %>% pull(Code),
                                "others" = filter(base$flow,Type != 'Final') %>% pull(Code)),
               "WSA" = Settings %>% filter(Type == 'Finished') %>% pull(id))
  
  # Load SUT templates
  
  SUT_temp <- list( "Supply" = as.matrix( read.xlsx(path$IE_classification, sheet = 5,rowNames = TRUE) ),
                    "Use" = as.matrix( read.xlsx(path$IE_classification, sheet = 6,rowNames = TRUE) )
                    )
  
  # Run syntax to load Source2Root maps:
  
  set <- read.xlsx(xlsxFile = paste0(path$Settings,"/Base/IE_settings.xlsx"),sheet = 2)
  
  path_sel <- list("flow" = paste0(path$Concordance,"/WSA/WSA_Source2Root_Product.csv"),
                   "process" = paste0(path$Concordance,"/WSA/WSA_Source2Root_Industry.csv")
                   )
  
  Source2Root <- list("WSA" = as.matrix( read.csv(path_sel$flow,header = FALSE) ))
  
  Source2Root$WSA <- Source2Root$WSA[Code$WSA,]  # Select only finished steel
  
  Source2Root$WSA <- Source2Root$WSA / rowSums(Source2Root$WSA)  # Create map (normalized concordance)
  
  Source2Root[["Cullen"]] <- list("Product" = as.matrix( read.csv(paste0(path$Concordance,"/Cullen/Cullen_Source2Root_Product.csv"),header = FALSE) ),
                                  "EndUse" = as.matrix( read.csv(paste0(path$Concordance,"/Cullen/Cullen_Source2Root_EndUse.csv"),header = FALSE) ))
  
  # Compile Source to mother concordance for finished steel products
  
  Source2Mother <- list("WSA" = Source2Root$WSA %*% as.matrix(R2M$flow[,Code$base$Finished]) )
  
  # Load WSA Source2Source map for estimate final use of hot rolled coil-sheet-strip
  
  Source2Source <- list("WSA" = as.matrix(read.csv(paste0(path$Concordance,"/WSA/WSA_Source2Source.csv"),header = FALSE)) )
  
  Source2Source$WSA <- Source2Source$WSA[Code$WSA,Code$WSA]
  
  
  # Create map to allocate mother to source:
  
  Mother2Source_Map <- list("WSA" = t(Source2Mother$WSA) / colSums(Source2Mother$WSA) ) 
  
  Mother2Source_Map$WSA[is.na(Mother2Source_Map$WSA)] <- 0 # Because forgings is not form WSA, set NA to 0
  
  # Create mother2mother concordance = parent to child processes
  
  Mother2Mother <-  list("WSA" = Mother2Source_Map$WSA %*% Source2Source$WSA %*% Source2Mother$WSA )
  
  colnames(Mother2Mother$WSA) <- rownames(Mother2Mother$WSA) <- Code$base$Finished
  
  
  # Load products to end-use map from Cullen et al 2012
  
  Products2EndUse <- read.csv(paste0(path$IE_Processed,"/Cullen/ProductsToEndUse.csv"))
  
  rownames(Products2EndUse) <- Products2EndUse$X  # Write products names to row names
  
  Products2EndUse$X <- NULL  # Delete product name column 
  
  Products2EndUse <- as.matrix(Products2EndUse)
  
  # Create Source to Root and Source to mother maps:
  
  Source2Root_Map <- list("Cullen" = list("EndUse" = (Source2Root$Cullen$EndUse / rowSums(Source2Root$Cullen$EndUse) ),
                                          "Product" = (Source2Root$Cullen$Product / rowSums(Source2Root$Cullen$Product) )
  )
  )
  
  Source2Mother[["Cullen"]] <- list("EndUse" =  Source2Root_Map$Cullen$EndUse %*% as.matrix(R2M$flow[,Code$base$Final]),
                                    "Product" = Source2Root_Map$Cullen$Product %*% as.matrix(R2M$flow[,Code$base$Finished])
  )
  
  Source2Mother$Cullen$Yields <- Source2Mother$Cullen$EndUse  # Add concordance for yields
  
  Source2Mother$Cullen$Yields[Source2Mother$Cullen$Yields > 0] <- 1  # Set shares to 1 for yields
  
  
  # Add mother sector names to maps:
  
  colnames(Source2Mother$Cullen$EndUse) <- base$flow$Name[Code$base$Final]
  
  colnames(Source2Mother$Cullen$Product) <- base$flow$Name[Code$base$Finished]
  
  # Translate product to end-use map to base classification of mother table: 
  
  Products2EndUse <- t(Source2Mother$Cullen$Product)  %*% Products2EndUse %*% Source2Mother$Cullen$EndUse
  
  
  
  # Add forgings (no information):
  
  Products2EndUse[rownames(Products2EndUse) == "Forgings",c(1,2,7,8)] <- 1  
  
  # Define functions for creating intermediate use and final output tables:
  
  CreateUse <- function() 
  {
    x <- data.frame(matrix(0,num$flow,num$process))
    
    colnames(x) <- base$process$Name
    
    rownames(x) <- base$flow$Name
    
    x <- as.matrix(x)
    
    return(x)
  }
  
  CreateOutput <- function() 
  {
    x <- data.frame( matrix(0,num$process + num$flow,num$demand) )
    
    colnames(x) <- base$demand$Name
    
    rownames(x) <- c( base$process$Name, base$flow$Name )
    
    x <- as.matrix(x)
    
    return(x)
  }
  
  
  # j refers to the column and i to the row view. j loops through all country codes
  
  for(j in 1:num$region)
  {
    print(paste("Imports of",base$region$Name[j]))
    
    # Load production values of region j:
    
    Supply <- read.csv( paste0(path$IE_Processed,"/SUT/",year,"_Supply_Region",j,".csv"), header = FALSE )
    
    Supply <- rowSums(Supply)
    
    # i loops only over the regions where i != j
    
    for(i in (1:num$region)[-j])
    {
      Use <- CreateUse()  # Create empty use table
      
      Final <- CreateOutput()  # Create empty final output table
      
      
      ### Allocating finished steel to fabrication/manufacturing use ###
      
      vec <- filter(data$BACI, From == i, To == j, Product %in% Code$base$Finished)
      
      # Read production of intermed. fabrication in region i:
      
      filter <- Supply[filter(base$process,Type == "Final") %>% pull(Code)] 
      
      filter[filter > 1] <- 1  # Set to one for filtering the map
      
      filter <- Products2EndUse %*%  diag(filter)  # Apply filter
      
      Map <- filter / rowSums(filter)  # Create map 
      
      Map <- Map[vec$Product - 10,]  # Select rows of imported products
      
      Value <- Map * vec$Quantity  # Map values to sectors
      
      Use[vec$Product,filter(base$process,Type == "Final") %>% pull(Code)] <- Value
  
      
      ### Allocate manufacturing intermediate imports to manufacturing use ###
      
      Value <- filter(data$Fab2Demand,base.from == i,base.to == j) %>%
        select(Quantity,sector.from,sector.to) 
      
      Value$sector.from <- Code$base$Final[Value$sector.from]
      
      Value$sector.to <- Code$industry$manu[Value$sector.to]
      
      index <- as.matrix( Value[, c("sector.from","sector.to") ] )
      
      Use[index] <- Value$Quantity  # # Write values in table
      
      
      
      ### Allocate all other intermediate products ###
      
      # Read concordance for all other industries except manufacturing:
      
      Map <- SUT_temp$Use[base$flow$Code,]
      
      Map <- Prorate(Map,Supply)
      
      vec <- filter(data$BACI, From == i, To == j, ! Product %in% Code$base$Finished)
      
      Value <- Map[vec$Product,] * vec$Quantity

      Use[vec$Product,] <- Value 
      
      
      ### Allocate Final Outputs ###
      
      Value <- filter(data$FinalDemand, base.from == i, base.to == j) %>% 
        select(Quantity, sector, demand)
        
      Value$sector <- Code$product$manu[Value$sector] + num$process
      
      index <- as.matrix( Value[, c("sector","demand") ] )
      
      Final[index] <- Value$Quantity
      
      # Delete column and row names
      
      Use <- round(Use,3)
      
      Final <- round(Final,3)
      
      colnames(Use) <- NULL
      rownames(Use) <- NULL
      colnames(Final) <- NULL
      rownames(Final) <- NULL
      
      # Write table to file
      
      Numbers2File(Use, paste0(path$IE_Processed,"/SUT/",year,"_IntermediateTrade_",i,"_",j,".csv") )
      
      Numbers2File(Final, paste0(path$IE_Processed,"/SUT/",year,"_FinalTrade_",i,"_",j,".csv") )
      
    }
  }
}
