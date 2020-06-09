###############################################
#                                             #
#   This code uses the data on fabrication    #
#   yields and products flows to end-use to   # 
#     build the IO extension and estimate     # 
#     the amount of fabrication scrap         #
#                                             #
###############################################


IEDataProcessing_PIOLab_WasteMFAIOExtension <- function(year,path)
{
  
  print("IEDataProcessing_PIOLab_WasteMFAIOExtension initiated.")

  
  # Load sector codes of aggregated EXIOBASE data
  
  IO.codes <- read.csv(file = paste0(path$IE_Processed,"/EXIOWasteMFAIO/IO_codes.csv"),
                    row.names = NULL)
                
  # Load variables and calculate gross output (x):
  
  L <- as.matrix( read.csv(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_L.csv"),
                  row.names = NULL,
                  header = FALSE)
                )
  
  Y <- as.matrix( read.csv(paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_Y.csv"),
                           row.names = NULL,
                           header = FALSE)
                )
  
  x <- rowSums( L %*% Y ) # Calculate total output and create vector
  
  source(paste0(path$Subroutines,"/Numbers2File.R"))  # Load fun. to write arrays to files
  
  # 1. Defining functions
  
  # Load WSA settings (for codes and feed names):
  
  Settings <- read.xlsx(paste0(path$Settings,"/datafeeds_settings/WSA_settings.xlsx"))
  
  
  # Select codes for finished steel:
  
  Code <- list("base" = list("Finished" = filter(base$flow,Type == 'Finished') %>% pull(Code),
                             "Final" = filter(base$flow,Type == 'Final') %>% pull(Code),
                             "Forgings" = base$flow$Code[base$flow$Name == 'Forgings'],
                             "CoilSheetStrip" = base$flow$Code[base$flow$Name == "Hot rolled coil-sheet-strip"]),
               "WSA" = Settings %>% filter(Type == 'Finished') %>% pull(id) )
  
 
  # Run syntax to load Source2Root maps:
  
  path_sel <- list("flow" = paste0(path$Concordance,"/WSA/WSA_Source2Root_Product.csv"),
                   "process" = paste0(path$Concordance,"/WSA/WSA_Source2Root_Industry.csv")
                   )
  
  Source2Root <-list("WSA" = as.matrix( read.csv(path_sel$flow,header = FALSE) ))
  
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
  
  colnames(Source2Mother$Cullen$EndUse) <- base$product$Name[Code$base$Final]
  
  colnames(Source2Mother$Cullen$Product) <- base$product$Name[Code$base$Finished]
  
  # Translate product to end-use map to base classification of mother table: 
  
  Products2EndUse <- t(Source2Mother$Cullen$Product)  %*% Products2EndUse %*% Source2Mother$Cullen$EndUse
  
  rownames(Products2EndUse) <- Code$base$Finished
  
  # Load Yield data from Cullen et al.:
  
  Yields <- read.csv(paste0(path$IE_Processed,"/Cullen/FabricationYields.csv"))
  
  Yields <- Yields[Yields$Yield.Sector != "All products",] # Remove average yield
  
  Yields <- Source2Mother$Cullen$Yields * Yields$Yield.Factor # Distribute yields to base sectors
  
  Yields <- apply(Yields, 2, function(x) max(x, na.rm = TRUE))  # Select largest yields available
  
  
  # Load BACI trade data:
  
  BACI <- read.csv(paste0(path$IE_Processed,"/BACI/BACI_",year,".csv")) 
  
  
  # Create two empty arrays for steel for the extension (Q) and for the fabrication total use (R)
  
  Q <- R <- as.data.frame(matrix(0,length(Code$base$Finished),nrow(IO.codes)))
  
  rownames(Q) <- rownames(R) <- base$flow$Name[Code$base$Finished]
  
  
  # Loop over base regions:
  
  for(r in base$region$Code)
  {
    
    map <- Source2Mother$WSA  # Create Map for base region and finished steel 
    
    colnames(map) <- Code$base$Finished
    
    # Loop over finished steel data feeds:
    
    for(p in 1:length(Code$WSA))
    {
      
      data <- read.csv(paste0(path$IE_Processed,"/WSA/WSA_",year,"_",Settings$FeedName[Code$WSA[p]],".csv"))
      
      vec <- rep(1,nrow(map)) # create empty vector
      
      if(r %in% data$base) # Check if region exists in production account
      {
        vec[p] <- filter(data,base == r) %>% pull(Quantity) # Write value in vector   
      }
      
      map <- map * vec  # Add value to map
      
    }
    
    # Load forging production
    data <- read.csv(paste0(path$IE_Processed,"/AligningData/Forgings_",year,".csv"))
    
    if(r %in% data$base) # Check if region exists in production account
    {
      value <- filter(data,base == r) %>% pull(Quantity)
    }
    
    map <- rbind(map,0) # Add empty row
    
    map[nrow(map),colnames(map) == Code$base$Forgings] <- value
    
    # Aggregate across sources and transform in data table:
    
    df <- data.frame("Code" = Code$base$Finished,  
                     "Production" = colSums(map),
                     "Import" = 0,
                     "Export" = 0,
                     "Use" = 0,
                     "Dom.Use" = 0)
    
    # Read and aggregate exports and imports across trade partners:
    
    BACI_export <- filter(BACI,From == r) %>% select(Product,Quantity) %>%
      group_by(Product) %>% summarise(Quantity = sum(Quantity)) %>%
      ungroup(Product) %>% filter(Product %in% Code$base$Finished)
    
    BACI_import <- filter(BACI,To == r) %>% select(Product,Quantity) %>%
      group_by(Product) %>% summarise(Quantity = sum(Quantity)) %>%
      ungroup(Product) %>% filter(Product %in% Code$base$Finished)
    
    
    # Write trade flows into data frame and estimate domestic use:
    
    df$Import[df$Code %in% BACI_import$Product] <- BACI_import$Quantity
    df$Export[df$Code %in% BACI_export$Product] <- BACI_export$Quantity
    
    df$Dom.Use <- df$Production - df$Export
    
       
    # Subtract production values of cold rolled products from hot rolled coil-sheet-strip
    
    Subtract <- sum(Mother2Mother$WSA[rownames(Mother2Mother$WSA) == Code$base$CoilSheetStrip,] * df$Production)
    
    df$Dom.Use[df$Code == Code$base$CoilSheetStrip] <- df$Dom.Use[df$Code == Code$base$CoilSheetStrip] - Subtract 
    
    
    print("minimum values before adjustment to domestic production:")
    print(df[df$Dom.Use < 0,])
    
    df$Dom.Use[df$Dom.Use < 0] <- 0 # Set use to zero where negative
    
    df$Use <- df$Dom.Use + df$Import
    
    print(paste0("Minimum of ",base$region$Name[r],": ",min(df$Use))) # Check min.
    
    
    x_filter <- diag(x[IO.codes$index[IO.codes$base == r]])  # Read production of region r
    
    x_filter[x_filter > 0] <- 1  # Set non-zero values to 1 for filtering 
    
    # Create map (including forging)
    
    map <- Products2EndUse %*% x_filter  # Apply filter
    colnames(map) <- IO.codes$commodity[IO.codes$base == r]
    map[rownames(map) == Code$base$Forgings, c(1,2,7,8)] <- 1  # Add forgings (no information)
    map <- map / rowSums(map)
    
    
    map_tot <-  map * df$Use # Create data frame with total material use from map
    
    Scrap <- colSums(map_tot) * (1-Yields)  # Estimate scrap flows
    
    # Write into data frame with base industry codes
    
    Scrap <- data.frame("base" = base$process$Code[base$process$Type == "Final"],
                        "Quantity" = Scrap)
    
    rownames(Scrap) <- NULL 
    
    # Write Scrap data to folder
    
    write.csv(Scrap,file = paste0(path$IE_Processed,"/Cullen/FabricationScrap_",
                                  year,"_",base$region$Name[r],".csv"),row.names = FALSE)
    
    map_tot_use <- map_tot %*% diag(Yields)  # Estimate useful output
    
    Q[,IO.codes$index[IO.codes$base == r]] <- map_tot_use  # Write values into extension
    
    
    # Create data frame for use of domestic products by domestic fabrication
    map_dom <-  map * df$Dom.Use # Create data frame with total material use from map
    
    # Write domestic use to folder
    Numbers2File(map_dom,paste0(path$IE_Processed,"/Cullen/FabricationUse_",year,
                                "_",base$region$Name[r],".csv"))
    
  }
  
  # Save extension of the MFA-Waste-IO Model
  
  Numbers2File( Q, paste0(path$IE_Processed,"/EXIOWasteMFAIO/",year,"_Q.csv") )
  
}