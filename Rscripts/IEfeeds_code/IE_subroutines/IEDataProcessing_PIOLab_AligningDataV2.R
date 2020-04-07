################################################################################
# This function uses WSA data to estimate forging output and 
# aligns trade data with IRP extraction
# It differs from version 1 in not filling the gaps in WSA by comparing with IEA
# Author: hanspeter.wieland@wu.ac.at
# Date: 26.03.2020

IEDataProcessing_PIOLab_AligningDataV2 <- function(year,path)
{
  
  print("IEDataProcessing_PIOLab_AligningDataV2 initiated.")
  
  # Loading BACI trade data and IRP extraction accounts:
  
  BACI <- read.csv(file = paste0(path$IE_Processed,"/BACI/BACI_",year,".csv"))
  
  IronOre <- read.csv(file = paste0(path$IE_Processed,"/IRP/IRP_",year,".csv"))
  
  colnames(IronOre)[2] <- "Extraction"
  
  sel <- setdiff(base$region$Code,IronOre$base) # Countries with no extraction
  
  BACI$Quantity[BACI$From %in% sel & BACI$Product == 1] <- 0  # Set zero
  
  BACI <- BACI[BACI$Quantity != 0,] # Filter regions with non-zero values
  
  # Write to folder:
  
  write.csv(BACI,file = paste0(path$IE_Processed,"/BACI/BACI_",year,".csv"),
            row.names = FALSE)
  
  remove(IronOre,sel) # Delete objects not needed anymore
  
  
  # 2. Estimate forgings
  
  
  # Read settings list for WSA data feeds:
  
  Settings <- read.xlsx(paste0(path$Settings,"/datafeeds_settings/WSA_settings.xlsx"))
  
  # Read production, import and export of ingots from WSA and BACI: 
  
  Ingots <- list("Production" = read.csv(paste0(path$IE_Processed,"/WSA/WSA_",year,"_Ingots.csv")))
  
  Ingots[["Code"]] <- filter(base$product, Name == "Ingots") %>% pull(Code)
  
  Ingots[["Export"]] <- filter(BACI, Product == Ingots$Code) %>% select(-Product) %>%
    group_by(From) %>% summarise(Quantity = sum(Quantity)) %>% ungroup(From)
  
  Ingots[["Import"]] <- filter(BACI, Product == Ingots$Code) %>% select(-Product) %>%
    group_by(To) %>% summarise(Quantity = sum(Quantity)) %>% ungroup(To)
  
  # Estimate forging output:
  
  Forgings <- data.frame("base" = 1:nrow(base$region),
                         "Quantity" = 0,
                         "In_pro" = 0,
                         "In_im" = 0,
                         "In_ex" = 0)
  
  Forgings$In_pro[Ingots$Production$base] <- Ingots$Production$Quantity
  
  Forgings$In_im[Ingots$Import$To] <- Ingots$Import$Quantity
  
  Forgings$In_ex[Ingots$Export$From] <- Ingots$Export$Quantity
  
  Forgings$Quantity <- Forgings$In_pro - Forgings$In_ex + Forgings$In_im
  
  Forgings$Quantity[Forgings$Quantity < 0] <- Forgings$In_im[Forgings$Quantity < 0]
  
  Forgings <- select(Forgings, base, Quantity)
  
  
  # Check if folder with processed data exists and in case delete:
  
  df_Processed <- paste0(path$IE_Processed,"/AligningData")
  
  if(dir.exists(df_Processed)) unlink(df_Processed,recursive = TRUE) 
  
  dir.create(df_Processed) # Create new (empty) folder
  
  # Write data to folder
  
  write.csv(Forgings,file = paste0(df_Processed,"/Forgings_",year,".csv"),
            row.names = FALSE)
  
  print("IEDataProcessing_PIOLab_AligningDataV2 finished.")
  
}