###################################
#                                 #  
#   This code writes data into    #
#   the trade blocks for AISHA    #
#                                 #  
###################################

DataProcessing_PIOLab_BuildingTradeBlocks <- function(year,path)
{
  print("DataProcessing_PIOLab_BuildingTradeBlocks initiated.")
  
  # Load function to create allocation map
  source(paste0(path$Subroutines,"/makeEndUseMap.R"))
  
  # Load the base codes for products
  base_product <- read.xlsx(paste0(path$Concordance,"/StandardPIOT_RootClassification.xlsx"),sheet = 7)
  # Load the base codes for industries
  base_industry <- read.xlsx(paste0(path$Concordance,"/StandardPIOT_RootClassification.xlsx"),sheet = 8)
  # Load region aggregator
  source(paste0(path$Subroutines,"/Root2Base_RegionAggregator.R"))
  reg_agg <- Root2Base_RegionAggregator(paste0(path$Concordance,"/Region Aggregators/StandardPIOT_RegionAggregator.csv"))

  # Define general variables
  n_pro <- nrow(base_product)
  n_ind <- nrow(base_industry)
  n_va <- 2
  n_fd <- 2
  # Read number of base regions
  n_reg <- max(reg_agg$base)

  ##############################################################################
  # 1. Load data on traded commodities
  # Load BACI trade data
  FlatRolled <- read.csv(paste0(path$Processed,"/BACI/BACI_",year,"_FlatRolledProducts.csv"))
  LongRolled <- read.csv(paste0(path$Processed,"/BACI/BACI_",year,"_LongRolledProducts.csv"))
  BilletBloom <- read.csv(paste0(path$Processed,"/BACI/BACI_",year,"_Billet&Bloom.csv"))
  IronOre <- read.csv(paste0(path$Processed,"/BACI/BACI_",year,"_IronOre.csv"))
  Ingot <- read.csv(paste0(path$Processed,"/BACI/BACI_",year,"_Ingot.csv"))
  PigIron <- read.csv(paste0(path$Processed,"/BACI/BACI_",year,"_PigIron.csv"))
  Slab <- read.csv(paste0(path$Processed,"/BACI/BACI_",year,"_Slab.csv"))
  SpongeIron <- read.csv(paste0(path$Processed,"/BACI/BACI_",year,"_SpongeIron.csv"))
  Scrap <- read.csv(paste0(path$Processed,"/BACI/BACI_",year,"_ScrapAfterTreatment.csv"))
  # Load steel in final demand
  FinalDemand <- read.csv(paste0(path$Processed,"/EXIOWasteMFAIO/",year,"_SteelInFinalDemand.csv")) 
  
  # Define function to create empty Use table
  CreateIntermediateUse <- function() {
    Use <- data.frame(matrix(0,n_pro,n_ind))
    colnames(Use) <- base_industry$BaseIndustryName
    rownames(Use) <- base_product$BaseProductName
    Use <- as.matrix(Use)
    return(Use)}
  
  CreateFinalUse <- function() {
    FinalUse <- data.frame(matrix(0,n_pro,n_fd))
    colnames(FinalUse) <- c("FinalConsumption","Environment")
    rownames(FinalUse) <- base_product$BaseProductName
    FinalUse <- as.matrix(FinalUse)
    return(FinalUse)}
  
  WriteValue <- function(data,from,to,Use)
    {
      data.sel <- data %>% filter(From == i & To == j)
      # Check if available
      if(nrow(data.sel) == 1) 
      {value <- sum(data %>% filter(From == i & To == j) %>% select(quantity))} else
          {value <- 0}
      Use[from,to] <- value
      return(Use)
    }
    
  # j refers to the column and i to the row view. j loops through all country codes
  for(j in 1:n_reg)
  {
    #print(paste0("Imports of region ",j))
    # i loops only over the regions where i != j
    for(i in (1:n_reg)[-j])
    {
      #Create empty use table
      Use <- CreateIntermediateUse()
      
      ##########################################################################
      # Write flows into table that match 1:1
      Use <- WriteValue(Ingot,"Ingots","Flat rolling",Use)
      Use <- WriteValue(PigIron,"Pig iron","Oxygen blown & open hearth furnace",Use)
      Use <- WriteValue(Slab,"Slabs","Flat rolling",Use)
      Use <- WriteValue(SpongeIron,"Sponge iron","Electric arc furnace",Use)
      Use <- WriteValue(BilletBloom,"Billets & blooms","Long rolling",Use)
      
      ##########################################################################
      # Allocating iron ore trade flows to blast furnace and direct reduction
      
      # Read out the production of pig and sponge iron and calculate shares
      Share <- data.frame("Pig" = 0,"Sponge" = 0)
      Pig <- read.csv(paste0(path$Processed,"/WSA/WSA_",year,"_PigIron.csv")) 
      if(j %in% Pig$base) Share$Pig <- Pig$Quantity[Pig$base == j] * 0.85
      Sponge <- read.csv(paste0(path$Processed,"/WSA/WSA_",year,"_SpongeIron.csv")) 
      if(j %in% Sponge$base) Share$Sponge <- Sponge$Quantity[Sponge$base == j] 
      Share <- Share/sum(Share)
      
      # In case WSa does not report any production values, allocate everything to BOF 
      if(is.na(Share$Pig) & is.na(Share$Sponge))
      {
        Share$Pig <- 1
        Share$Sponge <- 0
      }
      
      IronOre.sel <- IronOre %>% filter(From == i & To == j)
      # Check if available and write values
      if(nrow(IronOre.sel) == 1) 
      {
        Use["Iron ore","Blast furnace"] <- IronOre.sel$quantity * Share$Pig
        Use["Iron ore","Direct reduction"] <- IronOre.sel$quantity * Share$Sponge 
      }
      
      ##########################################################################
      # Allocating scrap trade flows to BOF and EAF
    
      # Read out the production of liquid steel in order to divide scrap input 
      # to oxygen blown and electric furnace
      Share <- data.frame("BOF" = 0,"EAF" = 0)
      BOF <- read.csv(paste0(path$Processed,"/WSA/WSA_",year,"_SteelOxygenBlownConverters.csv")) 
      if(j %in% BOF$base) Share$BOF <- BOF$Quantity[BOF$base == j] * 0.10
      EAF <- read.csv(paste0(path$Processed,"/WSA/WSA_",year,"_SteelElectricFurnaces.csv")) 
      if(j %in% EAF$base) Share$EAF <- EAF$Quantity[EAF$base == j] * 0.5 
      Share <- Share/sum(Share)
      
      Scrap.sel <- Scrap %>% filter(From == i & To == j)
      # Check if available and write values
      if(nrow(Scrap.sel) == 1) 
      {
        Use["Scrap steel","Oxygen blown & open hearth furnace"] <- Scrap.sel$quantity * Share$BOF
        Use["Scrap steel","Electric arc furnace"] <- Scrap.sel$quantity * Share$EAF 
      }
      
      ##########################################################################
      # Allocate Flat rolled product trade flow
      # call function to create specific map 
      map <- makeEndUseMap(j,"Flat")
      map <- map$Share[order(map$index)]
      
      # Select flat rolled trade flow
      Flat.sel <- FlatRolled %>% filter(From == i, To == j)
      # If available allocate according to shares
      if(nrow(Flat.sel) == 1)
      {
        values <- map * Flat.sel$quantity
        Use["Flat rolled products",(ncol(Use)-length(map)+1):ncol(Use)] <- values
      
      }
      
      ##########################################################################
      # Allocate Long rolled product trade flow
      # call function to create specific map 
      map <- makeEndUseMap(j,"Long")
      map <- map$Share[order(map$index)]
      
      # Select flat rolled trade flow
      Long.sel <- LongRolled %>% filter(From == i, To == j)
      # If available allocate according to shares
      if(nrow(Flat.sel) == 1)
      {
        values <- map * Flat.sel$quantity
        Use["Long rolled products",(ncol(Use)-length(map)+1):ncol(Use)] <- values
      }
      
      ##########################################################################
      # Create tables for final demand trade flows
      FD <- CreateFinalUse()
      # Filter data (from i to j)
      data.sel <- FinalDemand %>% filter(From.Region == i, To.Region == j) %>%
        select(Product, Quantity)
      # Write into table
      FD[data.sel$Product,"FinalConsumption"] <- data.sel$Quantity
      
      ##########################################################################
      # Delete column and row names
      Use <- round(Use,2)
      FD <- round(FD,2)
      
      colnames(Use) <- NULL
      rownames(Use) <- NULL
      colnames(FD) <- NULL
      rownames(FD) <- NULL
      
      # Write table to file
      write.table(Use,file = paste0(path$Processed,"/SUT/",year,"_IntermediateTrade_",i,"_",j,".csv"),
                  col.names = FALSE,
                  row.names = FALSE,
                  sep = ",")
      
      write.table(FD,file = paste0(path$Processed,"/SUT/",year,"_FinalTrade_",i,"_",j,".csv"),
                  col.names = FALSE,
                  row.names = FALSE,
                  sep = ",")
      
      # Write to working directory if on server
      if(dir.exists(path$mother))
      {
        write.table(Use,file = paste0(path$mother,"Data/IE/",year,"_IntermediateTrade_",i,"_",j,".csv"),
                    col.names = FALSE,
                    row.names = FALSE,
                    sep = ",")
        
        write.table(FD,file = paste0(path$mother,"Data/IE/",year,"_FinalTrade_",i,"_",j,".csv"),
                    col.names = FALSE,
                    row.names = FALSE,
                    sep = ",")
      } 
    }
  }
  print("End of DataProcessing_PIOLab_BuildingTradeBlocks.")
}
